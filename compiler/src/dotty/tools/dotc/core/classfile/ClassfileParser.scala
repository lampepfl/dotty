package dotty.tools
package dotc
package core
package classfile

import scala.language.unsafeNulls

import dotty.tools.tasty.{ TastyReader, TastyHeaderUnpickler }

import Contexts.*, Symbols.*, Types.*, Names.*, StdNames.*, NameOps.*, Scopes.*, Decorators.*
import SymDenotations.*, unpickleScala2.Scala2Unpickler.*, Constants.*, Annotations.*, util.Spans.*
import Phases.*
import ast.{ tpd, untpd }
import ast.tpd.*, util.*
import java.io.IOException

import java.lang.Integer.toHexString
import java.util.UUID

import scala.collection.immutable
import scala.collection.mutable.{ ListBuffer, ArrayBuffer }
import scala.annotation.switch
import typer.Checking.checkNonCyclic
import io.{AbstractFile, ZipArchive}
import scala.util.control.NonFatal
import dotty.tools.dotc.classpath.FileUtils.hasSiblingTasty

import scala.compiletime.uninitialized

object ClassfileParser {

  object Header:
    opaque type Version = Long

    object Version:
      val Unknown: Version = -1L

      def brokenVersionAddendum(classfileVersion: Version)(using Context): String =
        if classfileVersion.exists then
          val (maj, min) = (classfileVersion.majorVersion, classfileVersion.minorVersion)
          val scalaVersion = config.Properties.versionNumberString
          i""" (version $maj.$min),
            |  please check the JDK compatibility of your Scala version ($scalaVersion)"""
        else
          ""

      def apply(major: Int, minor: Int): Version =
        (major.toLong << 32) | (minor.toLong & 0xFFFFFFFFL)
      extension (version: Version)
        def exists: Boolean = version != Unknown
        def majorVersion: Int = (version >> 32).toInt
        def minorVersion: Int = (version & 0xFFFFFFFFL).toInt

  import ClassfileConstants.*

  /** Marker trait for unpicklers that can be embedded in classfiles. */
  trait Embedded

  /** Indicate that there is nothing to unpickle and the corresponding symbols can
    * be invalidated. */
  object NoEmbedded extends Embedded

  /** Replace raw types with wildcard applications */
  def cook(using Context): TypeMap = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: TypeRef if tp.symbol.typeParams.nonEmpty =>
        AppliedType(tp, tp.symbol.typeParams.map(Function.const(TypeBounds.empty)))
      case tp @ AppliedType(tycon, args) =>
        // disregard tycon itself, but map over it to visit the prefix
        tp.derivedAppliedType(mapOver(tycon), args.mapConserve(this))
      case tp @ TempPolyType(_, tpe) =>
        val tpe1 = this(tpe)
        if (tpe1 eq tpe) tp else tp.copy(tpe = tpe1)
      case tp @ TempClassInfoType(parents, _, _) =>
        val parents1 = parents.mapConserve(this)
        if (parents eq parents1) tp else tp.copy(parentTypes = parents1)
      case _ =>
        mapOver(tp)
    }
  }

  private[classfile] def parseHeader(classfile: AbstractFile)(using in: DataReader): Header.Version = {
    val magic = in.nextInt
    if (magic != JAVA_MAGIC)
      throw new IOException(s"class file '${classfile}' has wrong magic number 0x${toHexString(magic)}, should be 0x${toHexString(JAVA_MAGIC)}")
    val minorVersion = in.nextChar.toInt
    val majorVersion = in.nextChar.toInt
    if ((majorVersion < JAVA_MAJOR_VERSION) ||
        ((majorVersion == JAVA_MAJOR_VERSION) &&
         (minorVersion < JAVA_MINOR_VERSION)))
      throw new IOException(
        s"class file '${classfile}' has unknown version $majorVersion.$minorVersion, should be at least $JAVA_MAJOR_VERSION.$JAVA_MINOR_VERSION")
    Header.Version(majorVersion, minorVersion)
  }

  abstract class AbstractConstantPool(using in: DataReader) {
    protected val len = in.nextChar
    protected val starts = new Array[Int](len)
    protected val values = new Array[AnyRef](len)
    protected val internalized = new Array[NameOrString](len)

    { var i = 1
      while (i < starts.length) {
        starts(i) = in.bp
        i += 1
        (in.nextByte.toInt: @switch) match {
          case CONSTANT_UTF8 | CONSTANT_UNICODE =>
            in.skip(in.nextChar)
          case CONSTANT_CLASS | CONSTANT_STRING | CONSTANT_METHODTYPE
             | CONSTANT_MODULE | CONSTANT_PACKAGE =>
            in.skip(2)
          case CONSTANT_METHODHANDLE =>
            in.skip(3)
          case CONSTANT_FIELDREF | CONSTANT_METHODREF | CONSTANT_INTFMETHODREF
             | CONSTANT_NAMEANDTYPE | CONSTANT_INTEGER | CONSTANT_FLOAT
             | CONSTANT_INVOKEDYNAMIC | CONSTANT_DYNAMIC =>
            in.skip(4)
          case CONSTANT_LONG | CONSTANT_DOUBLE =>
            in.skip(8)
            i += 1
          case _ =>
            errorBadTag(in.bp - 1)
        }
      }
    }

    /** Return the name found at given index. */
    def getName(index: Int)(using in: DataReader): NameOrString = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      values(index) match {
        case name: NameOrString => name
        case null   =>
          val start = starts(index)
          if (in.getByte(start).toInt != CONSTANT_UTF8) errorBadTag(start)
          val len   = in.getChar(start + 1).toInt
          val name = new NameOrString(in.getUTF(start + 1, len + 2))
          values(index) = name
          name
      }
    }

    /** Return the name found at given index in the constant pool, with '/' replaced by '.'. */
    def getExternalName(index: Int)(using in: DataReader): NameOrString = {
      if (index <= 0 || len <= index)
        errorBadIndex(index)

      if (internalized(index) == null)
        internalized(index) = new NameOrString(getName(index).value.replace('/', '.'))

      internalized(index)
    }

    def getClassSymbol(index: Int)(using ctx: Context, in: DataReader): Symbol

    /** Return the external name of the class info structure found at 'index'.
     *  Use 'getClassSymbol' if the class is sure to be a top-level class.
     */
    def getClassName(index: Int)(using in: DataReader): NameOrString = {
      val start = starts(index)
      if (in.getByte(start).toInt != CONSTANT_CLASS) errorBadTag(start)
      getExternalName(in.getChar(start + 1))
    }

    /** Return the type of a class constant entry. Since
     *  arrays are considered to be class types, they might
     *  appear as entries in 'newarray' or 'cast' opcodes.
     */
    def getClassOrArrayType(index: Int)(using ctx: Context, in: DataReader): Type

    def getType(index: Int, isVarargs: Boolean = false)(using Context, DataReader): Type

    def getSuperClass(index: Int)(using Context, DataReader): Symbol = {
      assert(index != 0, "attempt to parse java.lang.Object from classfile")
      getClassSymbol(index)
    }

    def getConstant(index: Int)(using ctx: Context, in: DataReader): Constant = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var value = values(index)
      if (value eq null) {
        val start = starts(index)
        value = (in.getByte(start).toInt: @switch) match {
          case CONSTANT_STRING =>
            Constant(getName(in.getChar(start + 1).toInt).value)
          case CONSTANT_INTEGER =>
            Constant(in.getInt(start + 1))
          case CONSTANT_FLOAT =>
            Constant(in.getFloat(start + 1))
          case CONSTANT_LONG =>
            Constant(in.getLong(start + 1))
          case CONSTANT_DOUBLE =>
            Constant(in.getDouble(start + 1))
          case CONSTANT_CLASS =>
            getClassOrArrayType(index).typeSymbol
          case _ =>
            errorBadTag(start)
        }
        values(index) = value
      }
      value match {
        case ct: Constant  => ct
        case cls: Symbol   => Constant(cls.typeRef)
        case arr: Type     => Constant(arr)
      }
    }

    private def getSubArray(bytes: Array[Byte]): Array[Byte] = {
      val decodedLength = ByteCodecs.decode(bytes)
      val arr           = new Array[Byte](decodedLength)
      System.arraycopy(bytes, 0, arr, 0, decodedLength)
      arr
    }

    def getBytes(index: Int)(using in: DataReader): Array[Byte] = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var value = values(index).asInstanceOf[Array[Byte]]
      if (value eq null) {
        val start = starts(index)
        if (in.getByte(start).toInt != CONSTANT_UTF8) errorBadTag(start)
        val len   = in.getChar(start + 1)
        val bytes = new Array[Byte](len)
        in.getBytes(start + 3, bytes)
        value = getSubArray(bytes)
        values(index) = value
      }
      value
    }

    def getBytes(indices: List[Int])(using in: DataReader): Array[Byte] = {
      assert(!indices.isEmpty, indices)
      var value = values(indices.head).asInstanceOf[Array[Byte]]
      if (value eq null) {
        val bytesBuffer = ArrayBuffer.empty[Byte]
        for (index <- indices) {
          if (index <= 0 || AbstractConstantPool.this.len <= index) errorBadIndex(index)
          val start = starts(index)
          if (in.getByte(start).toInt != CONSTANT_UTF8) errorBadTag(start)
          val len = in.getChar(start + 1)
          val buf = new Array[Byte](len)
          in.getBytes(start + 3, buf)
          bytesBuffer ++= buf
        }
        value = getSubArray(bytesBuffer.toArray)
        values(indices.head) = value
      }
      value
    }

    /** Throws an exception signaling a bad constant index. */
    protected def errorBadIndex(index: Int)(using in: DataReader) =
      throw new RuntimeException("bad constant pool index: " + index + " at pos: " + in.bp)

    /** Throws an exception signaling a bad tag at given address. */
    protected def errorBadTag(start: Int)(using in: DataReader) =
      throw new RuntimeException("bad constant pool tag " + in.getByte(start) + " at byte " + start)
  }

  protected class NameOrString(val value: String) {
    private var _name: SimpleName = null
    def name: SimpleName = {
      if (_name eq null) _name = termName(value)
      _name
    }
  }
}

class ClassfileParser(
    classfile: AbstractFile,
    classRoot: ClassDenotation,
    moduleRoot: ClassDenotation)(ictx: Context) {

  import ClassfileConstants.*
  import ClassfileParser.*

  protected val staticModule: Symbol = moduleRoot.sourceModule(using ictx)

  protected val instanceScope: MutableScope = newScope(0) // the scope of all instance definitions
  protected val staticScope: MutableScope = newScope(0)   // the scope of all static definitions
  protected var pool: ConstantPool = uninitialized        // the classfile's constant pool

  protected var currentClassName: SimpleName = uninitialized // JVM name of the current class
  protected var classTParams: Map[Name, Symbol] = Map()

  private var Scala2UnpicklingMode = Mode.Scala2Unpickling
  private var classfileVersion: Header.Version = Header.Version.Unknown

  classRoot.info = NoLoader().withDecls(instanceScope)
  moduleRoot.info = NoLoader().withDecls(staticScope).withSourceModule(staticModule)

  private def currentIsTopLevel(using Context) = classRoot.owner.is(Flags.PackageClass)

  private def mismatchError(className: SimpleName) =
    throw new IOException(s"class file '${classfile.canonicalPath}' has location not matching its contents: contains class $className")

  def run()(using Context): Option[Embedded] = try ctx.base.reusableDataReader.withInstance { reader =>
    implicit val reader2 = reader.reset(classfile)
    report.debuglog("[class] >> " + classRoot.fullName)
    classfileVersion = parseHeader(classfile)
    this.pool = new ConstantPool
    val res = parseClass()
    this.pool =  null
    res
  }
  catch {
    case e: RuntimeException =>
      if (ctx.debug) e.printStackTrace()
      val addendum = Header.Version.brokenVersionAddendum(classfileVersion)
      throw new IOException(
        i"""  class file ${classfile.canonicalPath} is broken$addendum,
          |  reading aborted with ${e.getClass}:
          |  ${Option(e.getMessage).getOrElse("")}""")
  }

  /** Return the class symbol of the given name. */
  def classNameToSymbol(name: Name)(using Context): Symbol =
    val nameStr = name.toString
    innerClasses.get(nameStr) match
      case Some(entry) => innerClasses.classSymbol(entry)
      case None =>
        def lookupTopLevel(): Symbol = requiredClass(name)
        // For inner classes we usually don't get to this branch: `innerClasses.classSymbol` already returns the symbol
        // of the inner class based on the InnerClass table. However, if the classfile is missing the
        // InnerClass entry for `name`, it might still be that there exists an inner symbol (because
        // some other classfile _does_ have an InnerClass entry for `name`). In this case, we want to
        // return the actual inner symbol (C.D, with owner C), not the top-level symbol C$D. This is
        // what the logic below is for (see scala/bug#9937 / lampepfl/dotty#12086).
        val split = nameStr.lastIndexOf('$')
        if split < 0 || split >= nameStr.length - 1 then
          lookupTopLevel()
        else
          val outerNameStr = nameStr.substring(0, split)
          val innerNameStr = nameStr.substring(split + 1, nameStr.length)
          val outerSym = classNameToSymbol(outerNameStr.toTypeName)
          outerSym.denot.infoOrCompleter match
            case _: StubInfo =>
              // If the outer class C cannot be found, look for a top-level class C$D
              lookupTopLevel()
            case _ =>
              // We have a java-defined class name C$D and look for a member D of C. But we don't know if
              // D is declared static or not, so we have to search both in class C and its companion.
              val innerName = innerNameStr.toTypeName
              val r =
                if outerSym eq classRoot.symbol then
                  instanceScope.lookup(innerName).orElse(staticScope.lookup(innerName))
                else
                  outerSym.info.member(innerName).orElse(outerSym.asClass.companionModule.info.member(innerName)).symbol
              r.orElse(lookupTopLevel())

  var sawPrivateConstructor: Boolean = false

  def parseClass()(using ctx: Context, in: DataReader): Option[Embedded] = {
    val jflags       = in.nextChar
    val isAnnotation = hasAnnotation(jflags)
    val sflags       = classTranslation.flags(jflags)
    val isEnum       = (jflags & JAVA_ACC_ENUM) != 0
    val nameIdx      = in.nextChar
    currentClassName = pool.getClassName(nameIdx).name

    if (currentIsTopLevel &&
        currentClassName != classRoot.fullName.toSimpleName &&
        currentClassName != classRoot.fullName.encode.toSimpleName)
      mismatchError(currentClassName)

    addEnclosingTParams()

    /** Parse parents for Java classes. For Scala, return AnyRef, since the real type will be unpickled.
     *  Updates the read pointer of 'in'. */
    def parseParents: List[Type] = {
      val superType =
        if (classRoot.symbol == defn.ComparableClass ||
                 classRoot.symbol == defn.JavaCloneableClass ||
                 classRoot.symbol == defn.JavaSerializableClass) {
          // Treat these interfaces as universal traits
          in.nextChar
          defn.AnyType
        }
        else
          pool.getSuperClass(in.nextChar).typeRef
      val ifaceCount = in.nextChar
      var ifaces = for (i <- (0 until ifaceCount).toList) yield pool.getSuperClass(in.nextChar).typeRef
        // Dotty deviation: was
        //    var ifaces = for (i <- List.range(0, ifaceCount)) ...
        // This does not typecheck because the type parameter of List is now lower-bounded by Int | Char.
        // Consequently, no best implicit for the "Integral" evidence parameter of "range"
        // is found. Previously, this worked because of weak conformance, which has been dropped.

      superType :: ifaces
    }


    val result = unpickleOrParseInnerClasses()
    if (!result.isDefined) {
      var classInfo: Type = TempClassInfoType(parseParents, instanceScope, classRoot.symbol)
      // might be reassigned by later parseAttributes
      val staticInfo = TempClassInfoType(List(), staticScope, moduleRoot.symbol)

      enterOwnInnerClasses()

      classRoot.setFlag(sflags)
      moduleRoot.setFlag(Flags.JavaDefined | Flags.ModuleClassCreationFlags)

      val privateWithin = getPrivateWithin(jflags)

      classRoot.setPrivateWithin(privateWithin)
      moduleRoot.setPrivateWithin(privateWithin)
      moduleRoot.sourceModule.setPrivateWithin(privateWithin)

      for (i <- 0 until in.nextChar) parseMember(method = false)
      for (i <- 0 until in.nextChar) parseMember(method = true)

      classRoot.registerCompanion(moduleRoot.symbol)
      moduleRoot.registerCompanion(classRoot.symbol)

      setClassInfo(moduleRoot, staticInfo, fromScala2 = false)

      classInfo = parseAttributes(classRoot.symbol).complete(classInfo)
      if (isAnnotation)
        // classInfo must be a TempClassInfoType and not a TempPolyType,
        // because Java annotations cannot have type parameters.
        addAnnotationConstructor(classInfo.asInstanceOf[TempClassInfoType])

      setClassInfo(classRoot, classInfo, fromScala2 = false)
      NamerOps.addConstructorProxies(moduleRoot.classSymbol)
    }
    else if (result == Some(NoEmbedded))
      for (sym <- List(moduleRoot.sourceModule, moduleRoot.symbol, classRoot.symbol)) {
        classRoot.owner.asClass.delete(sym)
        sym.markAbsent()
      }

    result
  }

  /** Add type parameters of enclosing classes */
  def addEnclosingTParams()(using Context): Unit = {
    var sym = classRoot.owner
    while (sym.isClass && !sym.is(Flags.ModuleClass)) {
      for (tparam <- sym.typeParams)
        classTParams = classTParams.updated(tparam.name, tparam)
      sym = sym.owner
    }
  }

  def parseMember(method: Boolean)(using ctx: Context, in: DataReader): Unit = {
    val start = indexCoord(in.bp)
    val jflags = in.nextChar
    val sflags =
      if (method) Flags.Method | methodTranslation.flags(jflags)
      else fieldTranslation.flags(jflags)
    val preName = pool.getName(in.nextChar)
    if (!sflags.isOneOf(Flags.PrivateOrArtifact) || preName.name == nme.CONSTRUCTOR) {
      val sig = pool.getExternalName(in.nextChar).value
      val completer = MemberCompleter(preName.name, jflags, sig)
      val member = newSymbol(
        getOwner(jflags), preName.name, sflags, completer,
        getPrivateWithin(jflags), coord = start)

      completer.attrCompleter = parseAttributes(member)

      getScope(jflags).enter(member)

    }
    else {
      in.nextChar // info
      skipAttributes()
    }
  }

  class MemberCompleter(name: SimpleName, jflags: Int, sig: String) extends LazyType {
    var attrCompleter: AttributeCompleter = null

    def complete(denot: SymDenotation)(using Context): Unit = {
      val sym = denot.symbol
      val isEnum = (jflags & JAVA_ACC_ENUM) != 0
      val isNative = (jflags & JAVA_ACC_NATIVE) != 0
      val isTransient = (jflags & JAVA_ACC_TRANSIENT) != 0
      val isVolatile = (jflags & JAVA_ACC_VOLATILE) != 0
      val isConstructor = name eq nme.CONSTRUCTOR

      /** Strip leading outer param from constructor and trailing access tag for
        *  private inner constructors.
        */
      def normalizeConstructorParams() = innerClasses.get(currentClassName.toString) match {
        case Some(entry) if !isStatic(entry.jflags) =>
          val mt @ MethodTpe(paramNames, paramTypes, resultType) = denot.info: @unchecked
          var normalizedParamNames = paramNames.tail
          var normalizedParamTypes = paramTypes.tail
          if ((jflags & JAVA_ACC_SYNTHETIC) != 0) {
            // SI-7455 strip trailing dummy argument ("access constructor tag") from synthetic constructors which
            // are added when an inner class needs to access a private constructor.
            normalizedParamNames = paramNames.dropRight(1)
            normalizedParamTypes = paramTypes.dropRight(1)
          }
          denot.info = mt.derivedLambdaType(normalizedParamNames, normalizedParamTypes, resultType)
        case _ =>
      }

      /** Make return type of constructor be the enclosing class type,
        *  and make constructor type polymorphic in the type parameters of the class
        */
      def normalizeConstructorInfo() = {
        val rt = classRoot.typeRef appliedTo (classRoot.typeParams map (_.typeRef))

        def resultType(tpe: Type): Type = tpe match {
          case mt @ MethodType(paramNames) => mt.derivedLambdaType(paramNames, mt.paramInfos, rt)
          case pt : PolyType => pt.derivedLambdaType(pt.paramNames, pt.paramInfos, resultType(pt.resType))
        }

        denot.info = resultType(denot.info)
        addConstructorTypeParams(denot)
      }

      val isVarargs = denot.is(Flags.Method) && (jflags & JAVA_ACC_VARARGS) != 0
      denot.info = sigToType(sig, isVarargs = isVarargs)
      if (isConstructor) normalizeConstructorParams()
      if isNative then
        attrCompleter.annotations ::= Annotation.deferredSymAndTree(defn.NativeAnnot)(New(defn.NativeAnnot.typeRef, Nil))
      if isTransient then
        attrCompleter.annotations ::= Annotation.deferredSymAndTree(defn.TransientAnnot)(New(defn.TransientAnnot.typeRef, Nil))
      if isVolatile then
        attrCompleter.annotations ::= Annotation.deferredSymAndTree(defn.VolatileAnnot)(New(defn.VolatileAnnot.typeRef, Nil))
      denot.info = translateTempPoly(attrCompleter.complete(denot.info, isVarargs))
      if (isConstructor) normalizeConstructorInfo()

      if (ctx.explicitNulls) denot.info = JavaNullInterop.nullifyMember(denot.symbol, denot.info, isEnum)

      // seal java enums
      if (isEnum) {
        val enumClass = sym.owner.linkedClass
        if (!enumClass.exists)
          report.warning(em"no linked class for java enum $sym in ${sym.owner}. A referencing class file might be missing an InnerClasses entry.")
        else {
          if (!enumClass.is(Flags.Sealed)) enumClass.setFlag(Flags.AbstractSealed)
          enumClass.addAnnotation(Annotation.Child(sym, NoSpan))
        }
      }

    }
  }

  def constantTagToType(tag: Int)(using Context): Type =
    (tag: @switch) match {
      case BYTE_TAG   => defn.ByteType
      case CHAR_TAG   => defn.CharType
      case DOUBLE_TAG => defn.DoubleType
      case FLOAT_TAG  => defn.FloatType
      case INT_TAG    => defn.IntType
      case LONG_TAG   => defn.LongType
      case SHORT_TAG  => defn.ShortType
      case VOID_TAG   => defn.UnitType
      case BOOL_TAG   => defn.BooleanType
    }

  /** As specified in https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.16.1,
   *  an annotation argument of type boolean, byte, char or short will
   *  be represented as a CONSTANT_INTEGER, so we need to convert it to
   *  produce a correctly-typed tree. We need to do this each time the
   *  constant is accessed instead of storing the result of the
   *  conversion in the `values` cache, because the same constant might
   *  be used for annotation arguments of different type.
   */
  def convertTo(ct: Constant, pt: Type)(using Context): Constant = {
    if (pt eq defn.BooleanType) && ct.tag == IntTag then
      Constant(ct.value != 0)
    else
      ct.convertTo(pt)
  }

  private def sigToType(sig: String, owner: Symbol = null, isVarargs: Boolean = false)(using Context): Type = {
    var index = 0
    val end = sig.length
    def accept(ch: Char): Unit = {
      assert(sig(index) == ch, (sig(index), ch))
      index += 1
    }
    def subName(isDelimiter: Char => Boolean): SimpleName = {
      val start = index
      while (!isDelimiter(sig(index))) { index += 1 }
      termName(sig.slice(start, index))
    }
    // Warning: sigToType contains nested completers which might be forced in a later run!
    // So local methods need their own ctx parameters.
    def sig2type(tparams: immutable.Map[Name, Symbol], skiptvs: Boolean)(using Context): Type = {
      val tag = sig(index); index += 1
      (tag: @switch) match {
        case 'L' =>
          /** A type representation where inner classes become `A#B` instead of `A.this.B` (like with `typeRef`)
           *
           *  Note: the symbol must not be nested in a generic class.
           */
          def innerType(symbol: Symbol): Type =
            if symbol.is(Flags.Package) then
              symbol.thisType
            else if symbol.isType then
              TypeRef(innerType(symbol.owner), symbol)
            else
              throw new RuntimeException("unexpected term symbol " + symbol)

          def processTypeArgs(tp: Type): Type = tp match {
            case tp: TypeRef =>
              if (sig(index) == '<') {
                accept('<')
                val argsBuf = if (skiptvs) null else new ListBuffer[Type]
                while (sig(index) != '>') {
                  val arg = sig(index) match {
                    case variance @ ('+' | '-' | '*') =>
                      index += 1
                      variance match {
                        case '+' => TypeBounds.upper(sig2type(tparams, skiptvs))
                        case '-' =>
                          val argTp = sig2type(tparams, skiptvs)
                          // Interpret `sig2type` returning `Any` as "no bounds";
                          // morally equivalent to TypeBounds.empty, but we're representing Java code, so use FromJavaObjectType as the upper bound
                          if (argTp.typeSymbol == defn.AnyClass) TypeBounds.upper(defn.FromJavaObjectType)
                          else TypeBounds(argTp, defn.FromJavaObjectType)
                        case '*' => TypeBounds.upper(defn.FromJavaObjectType)
                      }
                    case _ => sig2type(tparams, skiptvs)
                  }
                  if (argsBuf != null) argsBuf += arg
                }
                accept('>')
                if (skiptvs) tp else AppliedType(tp, argsBuf.toList)
              }
              else tp
            case tp =>
              assert(sig(index) != '<', tp)
              tp
          }

          val classSym = classNameToSymbol(subName(c => c == ';' || c == '<'))
          val classTpe = if (classSym eq defn.ObjectClass) defn.FromJavaObjectType else innerType(classSym)
          var tpe = processTypeArgs(classTpe)
          while (sig(index) == '.') {
            accept('.')
            val name = subName(c => c == ';' || c == '<' || c == '.').toTypeName
            val tp = tpe.select(name)
            tpe = processTypeArgs(tp)
          }
          accept(';')
          tpe
        case ARRAY_TAG =>
          while ('0' <= sig(index) && sig(index) <= '9') index += 1
          val elemtp = sig2type(tparams, skiptvs)
          defn.ArrayOf(elemtp.translateJavaArrayElementType)
        case '(' =>
          def isMethodEnd(i: Int) = sig(i) == ')'
          def isArray(i: Int) = sig(i) == '['

          /** Is this a repeated parameter type?
           *  This is true if we're in a vararg method and this is the last parameter.
           */
          def isRepeatedParam(i: Int): Boolean =
            if !isVarargs then return false
            var cur = i
            // Repeated parameters are represented as arrays
            if !isArray(cur) then return false
            // Handle nested arrays: int[]...
            while isArray(cur) do
              cur += 1
            // Simple check to see if we're the last parameter: there should be no
            // array in the signature until the method end.
            while !isMethodEnd(cur) do
              if isArray(cur) then return false
              cur += 1
            true
          end isRepeatedParam

          val paramtypes = new ListBuffer[Type]()
          var paramnames = new ListBuffer[TermName]()
          while !isMethodEnd(index) do
            paramnames += nme.syntheticParamName(paramtypes.length)
            paramtypes += {
              if isRepeatedParam(index) then
                index += 1
                val elemType = sig2type(tparams, skiptvs = false)
                // `ElimRepeated` is responsible for correctly erasing this.
                defn.RepeatedParamType.appliedTo(elemType)
              else
                sig2type(tparams, skiptvs = false)
            }

          index += 1
          val restype = sig2type(tparams, skiptvs = false)
          MethodType(paramnames.toList, paramtypes.toList, restype)
        case 'T' =>
          val n = subName(';'.==).toTypeName
          index += 1
          //assert(tparams contains n, s"classTparams = $classTParams, tparams = $tparams, key = $n")
          if (skiptvs) defn.AnyType else tparams(n).typeRef
        case tag =>
          constantTagToType(tag)
      }
    }
    // sig2type(tparams, skiptvs)

    def sig2typeBounds(tparams: immutable.Map[Name, Symbol], skiptvs: Boolean)(using Context): Type = {
      val ts = new ListBuffer[Type]
      while (sig(index) == ':') {
        index += 1
        if (sig(index) != ':') { // guard against empty class bound
          val tp = sig2type(tparams, skiptvs)
          if (!skiptvs)
            ts += cook(tp)
        }
      }
      if (!skiptvs) {
        val bound = if ts.isEmpty then defn.AnyType else ts.reduceLeft(AndType.apply)
        TypeBounds.upper(bound)
      }
      else NoType
    }

    var tparams = classTParams

    def typeParamCompleter(start: Int) = new LazyType {
      def complete(denot: SymDenotation)(using Context): Unit = {
        val savedIndex = index
        try {
          index = start
          denot.info =
            checkNonCyclic( // we need the checkNonCyclic call to insert LazyRefs for F-bounded cycles
                denot.symbol,
                sig2typeBounds(tparams, skiptvs = false),
                reportErrors = false)
        }
        finally
          index = savedIndex
      }
    }

    val newTParams = new ListBuffer[Symbol]()
    if (sig(index) == '<') {
      assert(owner != null)
      index += 1
      val start = index
      while (sig(index) != '>') {
        val tpname = subName(':'.==).toTypeName
        val s = newSymbol(
          owner, tpname, owner.typeParamCreationFlags,
          typeParamCompleter(index), coord = indexCoord(index))
        if (owner.isClass) owner.asClass.enter(s)
        tparams = tparams + (tpname -> s)
        sig2typeBounds(tparams, skiptvs = true)
        newTParams += s
      }
      index += 1
    }
    val ownTypeParams = newTParams.toList.asInstanceOf[List[TypeSymbol]]
    val tpe =
      if ((owner == null) || !owner.isClass)
        sig2type(tparams, skiptvs = false)
      else {
        classTParams = tparams
        val parents = new ListBuffer[Type]()
        while (index < end)
          parents += sig2type(tparams, skiptvs = false) // here the variance doesn't matter
        TempClassInfoType(parents.toList, instanceScope, owner)
      }
    if (ownTypeParams.isEmpty) tpe else TempPolyType(ownTypeParams, tpe)
  }
  // sigToType

  class EnumTag(sig: String, name: NameOrString) {
    def toTree(using ctx: Context): untpd.Tree = {
      val enumClassTp = sigToType(sig)
      val enumModuleClass = enumClassTp.classSymbol.companionModule
      val tmref = TermRef(enumModuleClass.termRef, name.name)
      untpd.TypedSplice(ref(tmref))
    }
  }

  def parseAnnotArg(skip: Boolean = false)(using ctx: Context, in: DataReader): Option[untpd.Tree | EnumTag] = {

    // If we encounter an empty array literal, we need the type of the corresponding
    // parameter to properly type it, but that would require forcing the annotation
    // early. To avoid this we store annotation arguments as untyped trees

    // ... but constants need to actually be typed with a ConstantType, so we
    // can't rely on type inference, and type them early.
    def lit(c: Constant): untpd.Tree = untpd.TypedSplice(tpd.Literal(c))

    val tag = in.nextByte.toChar
    val index = in.nextChar

    tag match {
      case STRING_TAG =>
        if (skip) None else Some(lit(Constant(pool.getName(index).value)))
      case BOOL_TAG | BYTE_TAG | CHAR_TAG | SHORT_TAG =>
        if (skip) None else {
          val constant = convertTo(pool.getConstant(index), constantTagToType(tag))
          Some(lit(constant))
        }
      case INT_TAG | LONG_TAG | FLOAT_TAG | DOUBLE_TAG =>
        if (skip) None else Some(lit(pool.getConstant(index)))
      case CLASS_TAG =>
        if (skip) None else Some(lit(Constant(pool.getType(index))))
      case ENUM_TAG =>
        val sig = pool.getExternalName(index).value
        val enumCaseName = pool.getName(in.nextChar)
        if (skip) None else Some(EnumTag(sig, enumCaseName))
      case ARRAY_TAG =>
        val arr = new ArrayBuffer[untpd.Tree]()
        var hasError = false
        for (i <- 0 until index)
          parseAnnotArg(skip) match {
            case Some(c: untpd.Tree) => arr += c
            case Some(tag: EnumTag) => arr += tag.toTree
            case None => hasError = true
          }
        if (hasError) None
        else if (skip) None
        else {
          val elems = arr.toList
          Some(untpd.JavaSeqLiteral(elems, untpd.TypeTree()))
        }
      case ANNOTATION_TAG =>
        parseAnnotation(index, skip).map(_.untpdTree)
    }
  }

  class ClassfileAnnotation(annotType: Type, lazyArgs: List[(NameOrString, untpd.Tree | EnumTag)]) extends LazyAnnotation {
    private def args(using Context): List[untpd.Tree] =
      lazyArgs.map {
        case (name, tree: untpd.Tree) => untpd.NamedArg(name.name, tree).withSpan(NoSpan)
        case (name, tag: EnumTag)     => untpd.NamedArg(name.name, tag.toTree).withSpan(NoSpan)
      }

    protected var mySym: Symbol | (Context ?=> Symbol) =
      (ctx: Context) ?=> annotType.classSymbol

    protected var myTree: Tree | (Context ?=> Tree) =
      (ctx: Context) ?=> untpd.resolveConstructor(annotType, args)

    def untpdTree(using Context): untpd.Tree =
      untpd.New(untpd.TypeTree(annotType), List(args))
  }

  /** Parse and return a single annotation.  If it is malformed,
   *  return None.
   */
  def parseAnnotation(attrNameIndex: Char, skip: Boolean = false)(using ctx: Context, in: DataReader): Option[ClassfileAnnotation] = try {
    val attrType = pool.getType(attrNameIndex.toInt)
    val nargs = in.nextChar.toInt
    val argbuf = new ListBuffer[(NameOrString, untpd.Tree | EnumTag)]
    var hasError = false
    for (i <- 0 until nargs) {
      val name = pool.getName(in.nextChar)
      parseAnnotArg(skip) match {
        case Some(arg) =>
          argbuf += name -> arg

        case None =>
          hasError = !skip
      }
    }
    attrType match
      case tp: TypeRef if tp.denot.infoOrCompleter.isInstanceOf[StubInfo] =>
        // Silently ignore missing annotation classes like javac
        if ctx.debug then
          report.warning(em"Error while parsing annotations in ${classfile}: annotation class $tp not present on classpath")
        None
      case _ =>
        if (hasError || skip) None
        else Some(ClassfileAnnotation(attrType, argbuf.toList))
  }
  catch {
    case f: FatalError => throw f // don't eat fatal errors, they mean a class was not found
    case NonFatal(ex) =>
      // We want to be robust when annotations are unavailable, so the very least
      // we can do is warn the user about the exception
      // There was a reference to ticket 1135, but that is outdated: a reference to a class not on
      // the classpath would *not* end up here. A class not found is signaled
      // with a `FatalError` exception, handled above. Here you'd end up after a NPE (for example),
      // and that should never be swallowed silently.
      report.warning(em"Caught: $ex while parsing annotations in $classfile")
      if (ctx.debug) ex.printStackTrace()

      None // ignore malformed annotations
  }

  /** A completer for attributes
   *
   *  Top-level classes complete attributes eagerly, while members complete lazily.
   *
   *  @note We cannot simply store the bytes of attributes, as the bytes may
   *  contain references to the constant pool, where the constants are loaded
   *  lazily.
   */
  class AttributeCompleter(sym: Symbol) {
    var sig: String = null
    var constant: Constant = null
    var exceptions: List[NameOrString] = Nil
    var annotations: List[Annotation] = Nil
    var namedParams: Map[Int, TermName] = Map.empty
    def complete(tp: Type, isVarargs: Boolean = false)(using Context): Type = {
      val updatedType =
        if sig == null then tp
        else {
          val newType = sigToType(sig, sym, isVarargs)
          if (ctx.debug && ctx.verbose)
            println("" + sym + "; signature = " + sig + " type = " + newType)
          newType
        }

      val newType =
        if this.constant != null then
          val ct = convertTo(this.constant, updatedType)
          if ct != null then ConstantType(ct) else updatedType
        else updatedType

      annotations.foreach(annot => sym.addAnnotation(annot))

      exceptions.foreach { ex =>
        val cls = getClassSymbol(ex.name)
        sym.addAnnotation(ThrowsAnnotation(cls.asClass))
      }

      def fillInParamNames(t: Type): Type = t match
        case mt @ MethodType(oldp) if namedParams.nonEmpty =>
          mt.derivedLambdaType(List.tabulate(oldp.size)(n => namedParams.getOrElse(n, oldp(n))))
        case pt: PolyType if namedParams.nonEmpty =>
          pt.derivedLambdaType(pt.paramNames, pt.paramInfos, fillInParamNames(pt.resultType))
        case _ => t

      cook.apply(fillInParamNames(newType))
    }
  }

  def parseAttributes(sym: Symbol)(using ctx: Context, in: DataReader): AttributeCompleter = {
    val res = new AttributeCompleter(sym)

    def parseAttribute(): Unit = {
      val attrName = pool.getName(in.nextChar).name.toTypeName
      val attrLen = in.nextInt
      val end = in.bp + attrLen
      attrName match {
        case tpnme.SignatureATTR =>
          val sig = pool.getExternalName(in.nextChar)
          res.sig = sig.value

        case tpnme.SyntheticATTR =>
          sym.setFlag(Flags.SyntheticArtifact)

        case tpnme.BridgeATTR =>
          sym.setFlag(Flags.Bridge)

        case tpnme.DeprecatedATTR =>
          val msg = Literal(Constant("see corresponding Javadoc for more information."))
          val since = Literal(Constant(""))
          res.annotations ::= Annotation.deferredSymAndTree(defn.DeprecatedAnnot) {
            New(defn.DeprecatedAnnot.typeRef, msg :: since :: Nil)
          }

        case tpnme.ConstantValueATTR =>
          val c = pool.getConstant(in.nextChar)
          if (c ne null) res.constant = c
          else report.warning(em"Invalid constant in attribute of ${sym.showLocated} while parsing ${classfile}")

        case tpnme.MethodParametersATTR =>
          val paramCount = in.nextByte
          for i <- 0 until paramCount do
            val index = in.nextChar
            val flags = in.nextChar
            if index != 0 then
              val name = pool.getName(index)
              if (flags & JAVA_ACC_SYNTHETIC) == 0 then
                res.namedParams += (i -> name.name)

        case tpnme.AnnotationDefaultATTR =>
          sym.addAnnotation(Annotation(defn.AnnotationDefaultAnnot, Nil, sym.span))

        // Java annotations on classes / methods / fields with RetentionPolicy.RUNTIME
        case tpnme.RuntimeVisibleAnnotationATTR
          | tpnme.RuntimeInvisibleAnnotationATTR =>
          parseAnnotations(attrLen)

        // TODO 1: parse runtime visible annotations on parameters
        // case tpnme.RuntimeParamAnnotationATTR

        // TODO 2: also parse RuntimeInvisibleParamAnnotation
        // i.e. java annotations with RetentionPolicy.CLASS?

        case tpnme.ExceptionsATTR =>
          parseExceptions(attrLen)

        case tpnme.CodeATTR =>
          in.skip(attrLen)
          // flag test will trigger completion and cycles, thus have to be lazy
          if (sym.owner.flagsUNSAFE.isAllOf(Flags.JavaInterface)) {
            sym.resetFlag(Flags.Deferred)
            sym.owner.resetFlag(Flags.PureInterface)
            report.log(s"$sym in ${sym.owner} is a java 8+ default method.")
          }

        case _ =>
      }
      in.bp = end
    }

    /**
     * Parse the "Exceptions" attribute which denotes the exceptions
     * thrown by a method.
     */
    def parseExceptions(len: Int): Unit = {
      val nClasses = in.nextChar
      for (n <- 0 until nClasses) {
        // FIXME: this performs an equivalent of getExceptionTypes instead of getGenericExceptionTypes (SI-7065)
        val cls = pool.getClassName(in.nextChar.toInt)
        res.exceptions ::= cls
      }
    }


    /** Parse a sequence of annotations and attaches them to the
     *  current symbol sym, except for the ScalaSignature annotation that it returns, if it is available. */
    def parseAnnotations(len: Int): Unit = {
      val nAttr = in.nextChar
      for (n <- 0 until nAttr)
        parseAnnotation(in.nextChar) match {
          case Some(annot) =>
            sym.addAnnotation(annot)
          case None =>
        }
    }

    // begin parseAttributes
    for (i <- 0 until in.nextChar)
      parseAttribute()

    res
  }

  /** Annotations in Scala are assumed to get all their arguments as constructor
   *  parameters. For Java annotations we need to fake it by making up the constructor.
   */
  def addAnnotationConstructor(classInfo: TempClassInfoType)(using Context): Unit =
    newSymbol(
      owner = classRoot.symbol,
      name = nme.CONSTRUCTOR,
      flags = Flags.Synthetic | Flags.JavaDefined | Flags.Method,
      info = new AnnotConstructorCompleter(classInfo)
    ).entered

  class AnnotConstructorCompleter(classInfo: TempClassInfoType) extends LazyType {
    def complete(denot: SymDenotation)(using Context): Unit = {
      val attrs = classInfo.decls.toList.filter(sym => sym.isTerm && sym != denot.symbol && sym.name != nme.CONSTRUCTOR)
      val paramNames = attrs.map(_.name.asTermName)
      val paramTypes = attrs.map(_.info.resultType)
      denot.info = MethodType(paramNames, paramTypes, classRoot.typeRef)
    }
  }

  /** Enter own inner classes in the right scope. It needs the scopes to be set up,
   *  and implicitly current class' superclasses.
   */
  private def enterOwnInnerClasses()(using Context, DataReader): Unit = {
    def enterClassAndModule(entry: InnerClassEntry, file: AbstractFile, jflags: Int) =
      SymbolLoaders.enterClassAndModule(
          getOwner(jflags),
          entry.originalName,
          new ClassfileLoader(file),
          classTranslation.flags(jflags),
          getScope(jflags))

    for entry <- innerClasses.valuesIterator do
      // create a new class member for immediate inner classes
      if entry.outer.name == currentClassName then
        val file = ctx.platform.classPath.findClassFile(entry.externalName.toString) getOrElse {
          throw new AssertionError(entry.externalName)
        }
        enterClassAndModule(entry, file, entry.jflags)
  }

  // Nothing$ and Null$ were incorrectly emitted with a Scala attribute
  // instead of ScalaSignature before 2.13.0-M2, see https://github.com/scala/scala/pull/5952
  private val scalaUnpickleWhitelist = List(tpnme.nothingClass, tpnme.nullClass)

  /** Parse inner classes. Expects `in.bp` to point to the superclass entry.
   *  Restores the old `bp`.
   *  @return Some(unpickler) iff classfile is from Scala, so no Java info needs to be read.
   */
  def unpickleOrParseInnerClasses()(using ctx: Context, in: DataReader): Option[Embedded] = {
    val oldbp = in.bp
    try {
      skipSuperclasses()
      skipMembers() // fields
      skipMembers() // methods
      val attrs = in.nextChar
      val attrbp = in.bp

      def scan(target: TypeName): Boolean = {
        in.bp = attrbp
        var i = 0
        while (i < attrs && pool.getName(in.nextChar).name.toTypeName != target) {
          val attrLen = in.nextInt
          in.skip(attrLen)
          i += 1
        }
        i < attrs
      }

      def unpickleScala(bytes: Array[Byte]): Some[Embedded] = {
        val allowed = ctx.settings.Yscala2Unpickler.value

        def failUnless(cond: Boolean) =
          assert(cond,
            s"Unpickling ${classRoot.symbol.showLocated} from ${classRoot.symbol.associatedFile} is not allowed with -Yscala2-unpickler $allowed")

        if (allowed != "always") {
          failUnless(allowed != "never")
          val allowedList = allowed.split(java.io.File.pathSeparator).toList
          val file = classRoot.symbol.associatedFile
          // Using `.toString.contains` isn't great, but it's good enough for a debug flag.
          failUnless(file == null || allowedList.exists(path => file.toString.contains(path)))
        }

        val unpickler = new unpickleScala2.Scala2Unpickler(bytes, classRoot, moduleRoot)(ctx)
        withMode(Scala2UnpicklingMode)(unpickler.run())
        Some(unpickler)
      }

      def parseScalaSigBytes: Array[Byte] = {
        val tag = in.nextByte.toChar
        assert(tag == STRING_TAG, tag)
        pool getBytes in.nextChar
      }

      def parseScalaLongSigBytes: Array[Byte] = {
        val tag = in.nextByte.toChar
        assert(tag == ARRAY_TAG, tag)
        val stringCount = in.nextChar
        val entries =
          for (i <- 0 until stringCount) yield {
            val stag = in.nextByte.toChar
            assert(stag == STRING_TAG, stag)
            in.nextChar.toInt
          }
        pool.getBytes(entries.toList)
      }

      if (scan(tpnme.TASTYATTR)) {
        val hint =
          if classfile.hasSiblingTasty then "This is likely a bug in the compiler. Please report."
          else "This `.tasty` file is missing. Try cleaning the project to fix this issue."
        report.error(s"Loading Scala 3 binary from $classfile. It should have been loaded from `.tasty` file. $hint", NoSourcePosition)
        return None
      }

      if scan(tpnme.ScalaATTR) && !scalaUnpickleWhitelist.contains(classRoot.name)
        && !(classRoot.name.startsWith("Tuple") && classRoot.name.endsWith("$sp"))
        && !(classRoot.name.startsWith("Product") && classRoot.name.endsWith("$sp"))
      then
        // To understand the situation, it's helpful to know that:
        // - Scalac emits the `ScalaSig` attribute for classfiles with pickled information
        // and the `Scala` attribute for everything else.
        // - Dotty emits the `TASTY` attribute for classfiles with pickled information
        // and the `Scala` attribute for _every_ classfile.
        //
        // Therefore, if the `Scala` attribute is present but the `TASTY`
        // attribute isn't, this classfile is a compilation artifact.
        return Some(NoEmbedded)

      if (scan(tpnme.ScalaSignatureATTR) && scan(tpnme.RuntimeVisibleAnnotationATTR)) {
        val attrLen = in.nextInt
        val nAnnots = in.nextChar
        var i = 0
        while (i < nAnnots) {
          val attrClass = pool.getType(in.nextChar).typeSymbol
          val nArgs = in.nextChar
          var j = 0
          while (j < nArgs) {
            val argName = pool.getName(in.nextChar)
            if (argName.name == nme.bytes) {
              if attrClass == defn.ScalaSignatureAnnot then
                return unpickleScala(parseScalaSigBytes)
              else if attrClass == defn.ScalaLongSignatureAnnot then
                return unpickleScala(parseScalaLongSigBytes)
            }
            parseAnnotArg(skip = true)
            j += 1
          }
          i += 1
        }
      }

      if (scan(tpnme.InnerClassesATTR)) {
        val attrLen = in.nextInt
        val entries = in.nextChar.toInt
        for (i <- 0 until entries) {
          val innerIndex = in.nextChar
          val outerIndex = in.nextChar
          val nameIndex = in.nextChar
          val jflags = in.nextChar
          if (innerIndex != 0 && outerIndex != 0 && nameIndex != 0) {
            val inner = pool.getClassName(innerIndex)
            val outer = pool.getClassName(outerIndex)
            val name  = pool.getName(nameIndex)
            val entry = InnerClassEntry(inner, outer, name, jflags)
            innerClasses(inner.value) = entry
          }
        }
      }
      None
    }
    finally in.bp = oldbp
  }

  /** An entry in the InnerClasses attribute of this class file. */
  case class InnerClassEntry(external: NameOrString, outer: NameOrString, name: NameOrString, jflags: Int) {
    def externalName = external.value
    def outerName    = outer.value
    def originalName = name.name

    // The name of the outer class, without its trailing $ if it has one.
    def strippedOuter = outer.name.stripModuleClassSuffix
  }

  private object innerClasses extends util.HashMap[String, InnerClassEntry] {
    /** Return the Symbol of the top level class enclosing `name`,
     *  or 'name's symbol if no entry found for `name`.
     */
    def topLevelClass(name: String)(using Context): Symbol = {
      val tlName = if (contains(name)) {
        var entry = this(name)
        while (contains(entry.outerName))
          entry = this(entry.outerName)
        entry.outerName
      }
      else
        name
      classNameToSymbol(tlName.toTypeName)
    }

    /** Return the class symbol for `entry`. It looks it up in its outer class.
     *  This might force outer class symbols.
     */
    def classSymbol(entry: InnerClassEntry)(using Context): Symbol = {
      def getMember(sym: Symbol, name: Name)(using Context): Symbol =
        if (isStatic(entry.jflags))
          if (sym == classRoot.symbol)
            staticScope.lookup(name)
          else {
            var moduleClass = sym.registeredCompanion
            if (!moduleClass.exists && sym.isAbsent())
              moduleClass = sym.scalacLinkedClass
            moduleClass.info.member(name).symbol
          }
        else if (sym == classRoot.symbol)
          instanceScope.lookup(name)
        else if (sym == classRoot.symbol.owner && name == classRoot.name)
          classRoot.symbol
        else
          sym.info.member(name).symbol

      val outerName = entry.strippedOuter
      val innerName = entry.originalName
      val owner = classNameToSymbol(outerName)
      val result = owner.denot.infoOrCompleter match
        case _: StubInfo if hasAnnotation(entry.jflags) =>
          requiredClass(innerName.toTypeName)
            // It's okay for the classfiles of Java annotations to be missing
            // from the classpath. If an annotation is defined as an inner class
            // we need to avoid forcing the outer class symbol here, and instead
            // return a new stub symbol for the inner class. This is tested by
            // `surviveMissingInnerClassAnnot` in AnnotationsTests.scala
        case _ =>
          atPhase(typerPhase)(getMember(owner, innerName.toTypeName))
      assert(result ne NoSymbol,
        i"""failure to resolve inner class:
           |externalName = ${entry.externalName},
           |outerName = $outerName,
           |innerName = $innerName
           |owner.fullName = ${owner.showFullName}
           |while parsing ${classfile}""")
      result
    }
  }

  def skipAttributes()(using in: DataReader): Unit = {
    val attrCount = in.nextChar
    for (i <- 0 until attrCount) {
      in.skip(2); in.skip(in.nextInt)
    }
  }

  def skipMembers()(using in: DataReader): Unit = {
    val memberCount = in.nextChar
    for (i <- 0 until memberCount) {
      in.skip(6); skipAttributes()
    }
  }

  def skipSuperclasses()(using in: DataReader): Unit = {
    in.skip(2) // superclass
    val ifaces = in.nextChar
    in.skip(2 * ifaces)
  }

  protected def getOwner(flags: Int): Symbol =
    if (isStatic(flags)) moduleRoot.symbol else classRoot.symbol

  protected def getScope(flags: Int): MutableScope =
    if (isStatic(flags)) staticScope else instanceScope

  private def getPrivateWithin(jflags: Int)(using Context): Symbol =
    if ((jflags & (JAVA_ACC_PRIVATE | JAVA_ACC_PUBLIC)) == 0)
      classRoot.enclosingPackageClass
    else
      NoSymbol

  private def isPrivate(flags: Int)     = (flags & JAVA_ACC_PRIVATE) != 0
  private def isStatic(flags: Int)      = (flags & JAVA_ACC_STATIC) != 0
  private def hasAnnotation(flags: Int) = (flags & JAVA_ACC_ANNOTATION) != 0

  class ConstantPool(using in: DataReader) extends AbstractConstantPool {
    def getClassSymbol(index: Int)(using ctx: Context, in: DataReader): Symbol = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      var c = values(index).asInstanceOf[Symbol]
      if (c eq null) {
        val start = starts(index)
        if (in.getByte(start).toInt != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        c = ClassfileParser.this.getClassSymbol(name.name)
        values(index) = c
      }
      c
    }

    def getClassOrArrayType(index: Int)(using ctx: Context, in: DataReader): Type = {
      if (index <= 0 || len <= index) errorBadIndex(index)
      val value = values(index)
      var c: Type = null
      if (value eq null) {
        val start = starts(index)
        if (in.getByte(start).toInt != CONSTANT_CLASS) errorBadTag(start)
        val name = getExternalName(in.getChar(start + 1))
        if (name.value.charAt(0) == ARRAY_TAG) {
          c = sigToType(name.value)
          values(index) = c
        }
        else {
          val sym = classNameToSymbol(name.name)
          values(index) = sym
          c = sym.typeRef
        }
      }
      else c = value match {
          case tp: Type => tp
          case cls: Symbol => cls.typeRef
      }
      c
    }

    def getType(index: Int, isVarargs: Boolean = false)(using Context, DataReader): Type =
      sigToType(getExternalName(index).value, isVarargs = isVarargs)
  }

  def getClassSymbol(name: SimpleName)(using Context): Symbol =
    if (name.endsWith("$") && (name ne nme.nothingRuntimeClass) && (name ne nme.nullRuntimeClass))
      // Null$ and Nothing$ ARE classes
      requiredModule(name.dropRight(1))
    else classNameToSymbol(name)

}

package dotty.tools.backend.jvm

import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.util.{SourcePosition, NoSourcePosition}
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Decorators.em
import scala.tools.asm.ClassWriter
import scala.tools.asm.tree.ClassNode

/**
 * Implements late stages of the backend that don't depend on a Global instance, i.e.,
 * optimizations, post-processing and classfile serialization and writing.
 */
class PostProcessor(val frontendAccess: PostProcessorFrontendAccess, val bTypes: BTypes) {
  self =>
  import bTypes.{classBTypeFromInternalName}
  import frontendAccess.{backendReporting, compilerSettings}

  val backendUtils = new BackendUtils(this)
  val classfileWriters = new ClassfileWriters(frontendAccess)
  val classfileWriter  = classfileWriters.ClassfileWriter()

  type ClassnamePosition = (String, SourcePosition)
  private val caseInsensitively = new ConcurrentHashMap[String, ClassnamePosition]

  def sendToDisk(clazz: GeneratedClass, sourceFile: AbstractFile): Unit = if !compilerSettings.outputOnlyTasty then {
    val classNode = clazz.classNode
    val internalName = classNode.name.nn
    val bytes =
      try
        if !clazz.isArtifact then setSerializableLambdas(classNode)
        warnCaseInsensitiveOverwrite(clazz)
        setInnerClasses(classNode)
        serializeClass(classNode)
      catch
        case e: java.lang.RuntimeException if e.getMessage != null && e.getMessage.nn.contains("too large!") =>
          backendReporting.error(em"Could not write class $internalName because it exceeds JVM code size limits. ${e.getMessage}")
          null
        case ex: Throwable =>
          if compilerSettings.debug then ex.printStackTrace()
          backendReporting.error(em"Error while emitting $internalName\n${ex.getMessage}")
          null

     if bytes != null then
        if (AsmUtils.traceSerializedClassEnabled && internalName.contains(AsmUtils.traceSerializedClassPattern))
          AsmUtils.traceClass(bytes)
        val clsFile = classfileWriter.writeClass(internalName, bytes, sourceFile)
        if clsFile != null then clazz.onFileCreated(clsFile)
  }

  def sendToDisk(tasty: GeneratedTasty, sourceFile: AbstractFile): Unit = {
    val GeneratedTasty(classNode, tastyGenerator) = tasty
    val internalName = classNode.name.nn
    classfileWriter.writeTasty(classNode.name.nn, tastyGenerator(), sourceFile)
  }

  private def warnCaseInsensitiveOverwrite(clazz: GeneratedClass) = {
    val name = clazz.classNode.name.nn
    val lowerCaseJavaName = name.nn.toLowerCase
    val clsPos = clazz.position
    caseInsensitively.putIfAbsent(lowerCaseJavaName, (name, clsPos)) match {
      case null => ()
      case (dupName, dupPos) =>
        // Order is not deterministic so we enforce lexicographic order between the duplicates for error-reporting
        val ((pos1, pos2), (name1, name2)) =
          if (name < dupName) ((clsPos, dupPos), (name, dupName))
          else ((dupPos, clsPos), (dupName, name))
        val locationAddendum =
          if pos1.source.path == pos2.source.path then ""
          else s" (defined in ${pos2.source.file.name})"
        def nicify(name: String): String = name.replace('/', '.').nn
        if name1 == name2 then
          backendReporting.error(
            em"${nicify(name1)} and ${nicify(name2)} produce classes that overwrite one another", pos1)
        else
          backendReporting.warning(
            em"""Generated class ${nicify(name1)} differs only in case from ${nicify(name2)}$locationAddendum.
                |  Such classes will overwrite one another on case-insensitive filesystems.""", pos1)
    }
  }

  private def setSerializableLambdas(classNode: ClassNode): Unit = {
    import backendUtils.{collectSerializableLambdas, addLambdaDeserialize}
    val serializableLambdas = collectSerializableLambdas(classNode)
    if serializableLambdas.nonEmpty then
      addLambdaDeserialize(classNode, serializableLambdas)
  }

  private def setInnerClasses(classNode: ClassNode): Unit = {
    import backendUtils.{collectNestedClasses, addInnerClasses}
    classNode.innerClasses.nn.clear()
    val (declared, referred) = collectNestedClasses(classNode)
    addInnerClasses(classNode, declared, referred)
  }

  def serializeClass(classNode: ClassNode): Array[Byte] = {
    val cw = new ClassWriterWithBTypeLub(backendUtils.extraProc)
    classNode.accept(cw)
    cw.toByteArray.nn
  }

  // -----------------------------------------------------------------------------------------
  // finding the least upper bound in agreement with the bytecode verifier (given two internal names handed by ASM)
  // Background:
  //  http://gallium.inria.fr/~xleroy/publi/bytecode-verification-JAR.pdf
  //  http://comments.gmane.org/gmane.comp.java.vm.languages/2293
  //  https://github.com/scala/bug/issues/3872
  // -----------------------------------------------------------------------------------------

  /*  An `asm.ClassWriter` that uses `jvmWiseLUB()`
   *  The internal name of the least common ancestor of the types given by inameA and inameB.
   *  It's what ASM needs to know in order to compute stack map frames, http://asm.ow2.org/doc/developer-guide.html#controlflow
   */
  final class ClassWriterWithBTypeLub(flags: Int) extends ClassWriter(flags) {

    /**
     * This method is used by asm when computing stack map frames. It is thread-safe: it depends
     * only on the BTypes component, which does not depend on global.
     * TODO @lry move to a different place where no global is in scope, on bTypes.
     */
    override def getCommonSuperClass(inameA: String, inameB: String): String = {
      // All types that appear in a class node need to have their ClassBType cached, see [[cachedClassBType]].
      val a = classBTypeFromInternalName(inameA)
      val b = classBTypeFromInternalName(inameB)
      val lub = a.jvmWiseLUB(b)
      val lubName = lub.internalName
      assert(lubName != "scala/Any")
      lubName // ASM caches the answer during the lifetime of a ClassWriter. We outlive that. Not sure whether caching on our side would improve things.
    }
  }
}

/**
 * The result of code generation. [[isArtifact]] is `true` for mirror.
 */
case class GeneratedClass(
  classNode: ClassNode,
  sourceClassName: String,
  position: SourcePosition,
  isArtifact: Boolean,
  onFileCreated: AbstractFile => Unit)
case class GeneratedTasty(classNode: ClassNode, tastyGen: () => Array[Byte])
case class GeneratedCompilationUnit(sourceFile: AbstractFile, classes: List[GeneratedClass], tasty: List[GeneratedTasty])(using val ctx: Context)


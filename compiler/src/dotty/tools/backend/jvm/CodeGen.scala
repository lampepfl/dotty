package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.Trees.{PackageDef, ValDef}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Phases.Phase

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

import dotty.tools.dotc.interfaces
import dotty.tools.dotc.report

import java.util.Optional
import dotty.tools.dotc.sbt.ExtractDependencies
import dotty.tools.dotc.core.*
import Contexts.*
import Phases.*
import Symbols.*
import StdNames.nme

import java.io.DataOutputStream
import java.nio.channels.ClosedByInterruptException

import dotty.tools.tasty.{ TastyBuffer, TastyHeaderUnpickler }
import dotty.tools.dotc.core.tasty.TastyUnpickler

import scala.tools.asm
import scala.tools.asm.tree.*
import tpd.*
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.util
import dotty.tools.dotc.util.NoSourcePosition


class CodeGen(val int: DottyBackendInterface, val primitives: DottyPrimitives)( val bTypes: BTypesFromSymbols[int.type]) { self =>
  import DottyBackendInterface.symExtensions
  import bTypes.*

  private lazy val mirrorCodeGen = Impl.JMirrorBuilder()

  private def genBCode(using Context) = Phases.genBCodePhase.asInstanceOf[GenBCode]
  private def postProcessor(using Context) = genBCode.postProcessor
  private def generatedClassHandler(using Context) = genBCode.generatedClassHandler

  /**
   * Generate ASM ClassNodes for classes found in a compilation unit. The resulting classes are
   * passed to the `GenBCode.generatedClassHandler`.
   */
  def genUnit(unit: CompilationUnit)(using ctx: Context): Unit = {
    val generatedClasses = mutable.ListBuffer.empty[GeneratedClass]
    val generatedTasty = mutable.ListBuffer.empty[GeneratedTasty]

    def genClassDef(cd: TypeDef): Unit =
      try
        val sym = cd.symbol
        val sourceFile = unit.source.file


        val mainClassNode = genClass(cd, unit)
        val mirrorClassNode =
          if !sym.isTopLevelModuleClass then null
          else if sym.companionClass == NoSymbol then genMirrorClass(sym, unit)
          else
            report.log(s"No mirror class for module with linked class: ${sym.fullName}", NoSourcePosition)
            null

        if sym.isClass then
          val tastyAttrNode = if (mirrorClassNode ne null) mirrorClassNode else mainClassNode
          genTastyAndSetAttributes(sym, tastyAttrNode)

        def registerGeneratedClass(classNode: ClassNode, isArtifact: Boolean): Unit =
          if classNode ne null then
            generatedClasses += GeneratedClass(classNode,
              sourceClassName = sym.javaClassName,
              position = sym.srcPos.sourcePos,
              isArtifact = isArtifact,
              onFileCreated = onFileCreated(classNode, sym, unit.source)
            )

        registerGeneratedClass(mainClassNode, isArtifact = false)
        registerGeneratedClass(mirrorClassNode, isArtifact = true)
      catch
        case ex: InterruptedException => throw ex
        case ex: Throwable =>
          ex.printStackTrace()
          report.error(s"Error while emitting ${unit.source}\n${ex.getMessage}", NoSourcePosition)


    def genTastyAndSetAttributes(claszSymbol: Symbol, store: ClassNode): Unit =
      import Impl.createJAttribute
      for (binary <- unit.pickled.get(claszSymbol.asClass)) {
        generatedTasty += GeneratedTasty(store, binary)
        val tasty =
          val uuid = new TastyHeaderUnpickler(TastyUnpickler.scala3CompilerConfig, binary()).readHeader()
          val lo = uuid.getMostSignificantBits
          val hi = uuid.getLeastSignificantBits

          // TASTY attribute is created but only the UUID bytes are stored in it.
          // A TASTY attribute has length 16 if and only if the .tasty file exists.
          val buffer = new TastyBuffer(16)
          buffer.writeUncompressedLong(lo)
          buffer.writeUncompressedLong(hi)
          buffer.bytes

        val dataAttr = createJAttribute(nme.TASTYATTR.mangledString, tasty, 0, tasty.length)
        store.visitAttribute(dataAttr)
      }

    def genClassDefs(tree: Tree): Unit =
      tree match {
        case EmptyTree => ()
        case PackageDef(_, stats) => stats foreach genClassDefs
        case ValDef(_, _, _) => () // module val not emitted
        case td: TypeDef => frontendAccess.frontendSynch(genClassDef(td))
      }

    genClassDefs(unit.tpdTree)
    generatedClassHandler.process(
      GeneratedCompilationUnit(unit.source.file, generatedClasses.toList, generatedTasty.toList)
    )
  }

  // Creates a callback that will be evaluated in PostProcessor after creating a file
  private def onFileCreated(cls: ClassNode, claszSymbol: Symbol, sourceFile: util.SourceFile)(using Context): AbstractFile => Unit = {
    val isLocal = atPhase(sbtExtractDependenciesPhase) {
      claszSymbol.isLocal
    }
    clsFile => {
      val className = cls.name.replace('/', '.')
      if (ctx.compilerCallback != null)
        ctx.compilerCallback.onClassGenerated(sourceFile, convertAbstractFile(clsFile), className)

      if isLocal then
        ctx.withIncCallback(_.generatedLocalClass(sourceFile, clsFile.jpath))
    }
  }

  /** Convert a `dotty.tools.io.AbstractFile` into a
   *  `dotty.tools.dotc.interfaces.AbstractFile`.
   */
  private def convertAbstractFile(absfile: dotty.tools.io.AbstractFile): interfaces.AbstractFile =
    new interfaces.AbstractFile {
      override def name = absfile.name
      override def path = absfile.path
      override def jfile = Optional.ofNullable(absfile.file)
    }

  private def genClass(cd: TypeDef, unit: CompilationUnit): ClassNode = {
    val b = new Impl.SyncAndTryBuilder(unit) {}
    b.genPlainClass(cd)
    b.cnode
  }

  private def genMirrorClass(classSym: Symbol, unit: CompilationUnit): ClassNode = {
    mirrorCodeGen.genMirrorClass(classSym, unit)
  }


  sealed transparent trait ImplEarlyInit{
    val int: self.int.type = self.int
    val bTypes: self.bTypes.type = self.bTypes
    protected val primitives: DottyPrimitives = self.primitives
  }
  object Impl extends ImplEarlyInit with BCodeSyncAndTry
}

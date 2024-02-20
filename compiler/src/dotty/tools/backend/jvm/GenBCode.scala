package dotty.tools.backend.jvm

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.report
import dotty.tools.dotc.core.*
import dotty.tools.dotc.interfaces.CompilerCallback
import Contexts.*
import Symbols.*
import dotty.tools.io.*
import scala.collection.mutable
import scala.compiletime.uninitialized

class GenBCode extends Phase { self =>

  override def phaseName: String = GenBCode.name

  override def description: String = GenBCode.description

  override def isRunnable(using Context) = super.isRunnable && !ctx.usedBestEffortTasty

  private val superCallsMap = new MutableSymbolMap[Set[ClassSymbol]]
  def registerSuperCall(sym: Symbol, calls: ClassSymbol): Unit = {
    val old = superCallsMap.getOrElse(sym, Set.empty)
    superCallsMap.update(sym, old + calls)
  }

  private val entryPoints = new mutable.HashSet[String]()
  def registerEntryPoint(s: String): Unit = entryPoints += s

  private var _backendInterface: DottyBackendInterface = uninitialized
  def backendInterface(using ctx: Context): DottyBackendInterface = {
    if _backendInterface eq null then
      // Enforce usage of FreshContext so we would be able to modify compilation unit between runs
      val backendCtx = ctx match
        case fc: FreshContext => fc
        case ctx => ctx.fresh
      _backendInterface = DottyBackendInterface(superCallsMap)(using backendCtx)
    _backendInterface
  }

  private var _codeGen: CodeGen = uninitialized
  def codeGen(using Context): CodeGen = {
    if _codeGen eq null then
      val int = backendInterface
      val dottyPrimitives = new DottyPrimitives(ctx)
      _codeGen = new CodeGen(int, dottyPrimitives)(bTypes.asInstanceOf[BTypesFromSymbols[int.type]])
    _codeGen
  }

  private var _bTypes: BTypesFromSymbols[DottyBackendInterface] = uninitialized
  def bTypes(using Context): BTypesFromSymbols[DottyBackendInterface] = {
    if _bTypes eq null then
      _bTypes = BTypesFromSymbols(backendInterface, frontendAccess)
    _bTypes
  }

  private var _frontendAccess: PostProcessorFrontendAccess | Null = uninitialized
  def frontendAccess(using Context): PostProcessorFrontendAccess = {
    if _frontendAccess eq null then
      _frontendAccess = PostProcessorFrontendAccess.Impl(backendInterface, entryPoints)
    _frontendAccess.nn
  }

  private var _postProcessor: PostProcessor | Null = uninitialized
  def postProcessor(using Context): PostProcessor = {
    if _postProcessor eq null then
      _postProcessor = new PostProcessor(frontendAccess, bTypes)
    _postProcessor.nn
  }

  private var _generatedClassHandler: GeneratedClassHandler | Null = uninitialized
  def generatedClassHandler(using Context): GeneratedClassHandler = {
    if _generatedClassHandler eq null then
      _generatedClassHandler = GeneratedClassHandler(postProcessor)
    _generatedClassHandler.nn
  }

  override def run(using Context): Unit =
    frontendAccess.frontendSynchWithoutContext {
      backendInterface.ctx
      .asInstanceOf[FreshContext]
      .setCompilationUnit(ctx.compilationUnit)
    }
    codeGen.genUnit(ctx.compilationUnit)
    (ctx.compilerCallback: CompilerCallback | Null) match {
      case cb: CompilerCallback => cb.onSourceCompiled(ctx.source)
      case null => ()
    }

  override def runOn(units: List[CompilationUnit])(using ctx:Context): List[CompilationUnit] = {
    try
      val result = super.runOn(units)
      generatedClassHandler.complete()
      result
    finally
      // frontendAccess and postProcessor are created lazilly, clean them up only if they were initialized
      if _frontendAccess ne null then
        frontendAccess.compilerSettings.outputDirectory match {
          case jar: JarArchive =>
            if (ctx.run.nn.suspendedUnits.nonEmpty)
              // If we close the jar the next run will not be able to write on the jar.
              // But if we do not close it we cannot use it as part of the macro classpath of the suspended files.
              report.error("Can not suspend and output to a jar at the same time. See suspension with -Xprint-suspension.")

            jar.close()
          case _ => ()
        }
      if _postProcessor ne null then
        postProcessor.classfileWriter.close()
      generatedClassHandler.close()
  }
}

object GenBCode {
  val name: String = "genBCode"
  val description: String = "generate JVM bytecode"
}

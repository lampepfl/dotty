package dotty.tools.dotc

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

import java.io.File
import java.nio.file.{Files, Path}

import com.google.common.jimfs.Jimfs

import org.scalajs.linker.*
import org.scalajs.linker.interface.*
import org.scalajs.logging.*

object ScalaJSLink:
  private val compliantSemantics: Semantics =
    Semantics.Defaults
      .withAsInstanceOfs(CheckedBehavior.Compliant)
      .withArrayIndexOutOfBounds(CheckedBehavior.Compliant)
      .withModuleInit(CheckedBehavior.Compliant)
  end compliantSemantics

  def link(classPath: String, useCompliantSemantics: Boolean)(using ExecutionContext): Path =
    val cpEntries = classPath.split(File.pathSeparatorChar)

    val logger = new ScalaConsoleLogger(Level.Warn)

    val moduleInitializers = Seq(ModuleInitializer.mainMethodWithArgs(
        "Test", "main", Nil))

    val semantics = if useCompliantSemantics then compliantSemantics else Semantics.Defaults

    val linkerConfig = StandardConfig()
      .withCheckIR(true)
      .withSourceMap(false)
      .withBatchMode(true)
      .withSemantics(semantics)

    val linker = StandardImpl.linker(linkerConfig)

    val dir = Jimfs.newFileSystem().getPath("tmp")
    Files.createDirectory(dir)

    val cache = StandardImpl.irFileCache().newCache
    val result = PathIRContainer
      .fromClasspath(cpEntries.toSeq.map(entry => new File(entry).toPath()))
      .map(_._1)
      .flatMap(cache.cached)
      .flatMap(linker.link(_, moduleInitializers, PathOutputDirectory(dir), logger))

    val report = Await.result(result, Duration.Inf)

    if (report.publicModules.size != 1)
      throw new AssertionError(s"got other than 1 module: $report")

    dir.resolve(report.publicModules.head.jsFileName)
  end link
end ScalaJSLink

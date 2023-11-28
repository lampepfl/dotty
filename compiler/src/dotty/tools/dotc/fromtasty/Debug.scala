package dotty.tools
package dotc
package fromtasty

import scala.language.unsafeNulls

import scala.util.control.NonFatal

import dotty.tools.io.Directory

import java.io.{File => JFile}
import java.nio.file.{Files, Paths}

object Debug {
  def main(args: Array[String]): Unit = {
    // Preload scala.util.control.NonFatal. Otherwise, when trying to catch a StackOverflowError,
    // we may try to load it but fail with another StackOverflowError and lose the original exception,
    // see <https://groups.google.com/forum/#!topic/scala-user/kte6nak-zPM>.
    val _ = NonFatal

    assert(!args.contains("-d"))

    val outPath = Paths.get("out")
    Directory(outPath).createDirectory()

    val tmpOut = Files.createTempDirectory(outPath.toAbsolutePath, "from-tasty-tmp")

    val fromSourcesOut = Files.createDirectory(tmpOut.resolve("from-source"))

    println("Compiling from .scala sources")
    val compilation1 = dotc.Main.process("-d" +: fromSourcesOut.toString +: args)

    if (compilation1.hasErrors) {
      println("Failed compilation from sources")
      Directory(tmpOut).deleteRecursively()
      sys.exit(1)
    }

    val fromTastyOut = Files.createDirectory(tmpOut.resolve("from-tasty"))

    val tastyFiles =
      Directory(fromSourcesOut).walk
        .filter(x => x.isFile && x.ext.isTasty)
        .map(_.toString)
        .toList

    val fromTastyArgs =
      "-from-tasty" ::
      "-d" :: fromTastyOut.toString ::
      insertClasspathInArgs(args.filterNot(_.endsWith(".scala")).toList, fromSourcesOut.toString) :::
      tastyFiles

    println("Compiling from .tasty sources")
    val compilation2 = dotc.Main.process(fromTastyArgs.toArray)

    if (compilation2.hasErrors) {
      println("Failed compilation from TASTY")
      println("Compilation input: " + fromSourcesOut)
      // In this case we do not delete the generated class files to allow further debugging.
      // For example `dotc -decompile` on one of the intermediate class files.
      sys.exit(1)
    } else {
      println("Recompilation successful")
    }

    Directory(tmpOut).deleteRecursively()
  }

  private def insertClasspathInArgs(args: List[String], cp: String): List[String] = {
    val (beforeCp, fromCp) = args.span(_ != "-classpath")
    val classpath = fromCp.drop(1).headOption.fold(cp)(_ + JFile.pathSeparator + cp)
    "-classpath" :: classpath :: beforeCp ::: fromCp.drop(2)
  }
}

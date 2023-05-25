package dotty.tools.dotc
package core
package tasty

import scala.language.unsafeNulls
import java.nio.file.{Path as JPath, Files as JFiles}
import java.nio.channels.ClosedByInterruptException
import java.io.DataOutputStream
import dotty.tools.io.{File, PlainFile}
import dotty.tools.dotc.core.Contexts.Context

object BestEffortTastyWriter:

  def write(dir: JPath, units: List[CompilationUnit])(using Context): Unit =
    if JFiles.exists(dir) then JFiles.createDirectories(dir)

    units.foreach { unit =>
      unit.pickled.foreach { (clz, binary) =>
        val parts = clz.fullName.mangledString.split('.')
        val outPath = outputPath(parts.toList, dir)
        val outTastyFile = new PlainFile(new File(outPath))
        val outstream = new DataOutputStream(outTastyFile.bufferedOutput)
        try outstream.write(binary())
        catch case ex: ClosedByInterruptException =>
          try
            outTastyFile.delete() // don't leave an empty or half-written tastyfile around after an interrupt
          catch
            case _: Throwable =>
          throw ex
        finally outstream.close()
      }
    }

  def outputPath(parts: List[String], acc: JPath): JPath =
    parts match
      case Nil => throw new Exception("Invalid class name")
      case last :: Nil =>
        val name = last.stripSuffix("$")
        acc.resolve(s"$name.betasty")
      case pkg :: tail =>
        val next = acc.resolve(pkg)
        if !JFiles.exists(next) then JFiles.createDirectory(next)
        outputPath(tail, next)

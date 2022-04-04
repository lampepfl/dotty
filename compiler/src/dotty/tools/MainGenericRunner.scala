package dotty.tools


import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import java.net.URLClassLoader
import sys.process._
import java.io.File
import java.lang.Thread
import scala.annotation.internal.sharable
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.runner.ObjectRunner
import dotty.tools.dotc.config.Properties.envOrNone

enum ExecuteMode:
  case Guess
  case Script
  case Repl
  case Run

case class Settings(
  verbose: Boolean = false,
  classPath: List[String] = List.empty,
  executeMode: ExecuteMode = ExecuteMode.Guess,
  exitCode: Int = 0,
  javaArgs: List[String] = List.empty,
  scalaArgs: List[String] = List.empty,
  residualArgs: List[String] = List.empty,
  scriptArgs: List[String] = List.empty,
  targetScript: String = "",
  save: Boolean = false,
  modeShouldBeRun: Boolean = false,
) {
  def withExecuteMode(em: ExecuteMode): Settings = this.executeMode match
    case ExecuteMode.Guess =>
      this.copy(executeMode = em)
    case _ =>
      println(s"execute_mode==[$executeMode], attempted overwrite by [$em]")
      this.copy(exitCode = 1)
  end withExecuteMode

  def withScalaArgs(args: String*): Settings =
    this.copy(scalaArgs = scalaArgs.appendedAll(args.toList))

  def withJavaArgs(args: String*): Settings =
    this.copy(javaArgs = javaArgs.appendedAll(args.toList))

  def withResidualArgs(args: String*): Settings =
    this.copy(residualArgs = residualArgs.appendedAll(args.toList))

  def withScriptArgs(args: String*): Settings =
    this.copy(scriptArgs = scriptArgs.appendedAll(args.toList))

  def withTargetScript(file: String): Settings =
    Try(Source.fromFile(file)).toOption match
      case Some(_) => this.copy(targetScript = file)
      case None      =>
        println(s"not found $file")
        this.copy(exitCode = 2)
  end withTargetScript

  def withSave: Settings =
    this.copy(save = true)

  def withModeShouldBeRun: Settings =
    this.copy(modeShouldBeRun = true)
}

object MainGenericRunner {

  val classpathSeparator = File.pathSeparator

  @sharable val javaOption = raw"""-J(.*)""".r

  @tailrec
  def process(args: List[String], settings: Settings): Settings = args match
    case Nil =>
      settings
    case "-run" :: tail =>
      process(tail, settings.withExecuteMode(ExecuteMode.Run))
    case ("-cp" | "-classpath" | "--class-path") :: cp :: tail =>
      process(tail, settings.copy(classPath = settings.classPath.appended(cp)))
    case ("-version" | "--version") :: _ =>
      settings.copy(
        executeMode = ExecuteMode.Repl,
        residualArgs = List("-version")
      )
    case ("-v" | "-verbose" | "--verbose") :: tail =>
      process(
        tail,
        settings.copy(
          verbose = true,
          residualArgs = settings.residualArgs :+ "-verbose"
        )
      )
    case "-save" :: tail =>
      process(tail, settings.withSave)
    case (o @ javaOption(striped)) :: tail =>
      process(tail, settings.withJavaArgs(striped).withScalaArgs(o))
    case arg :: tail =>
      val line = Try(Source.fromFile(arg).getLines.toList).toOption.flatMap(_.headOption)
      if arg.endsWith(".scala") || arg.endsWith(".sc") || (line.nonEmpty && raw"#!.*scala".r.matches(line.get)) then
        settings
          .withExecuteMode(ExecuteMode.Script)
          .withTargetScript(arg)
          .withScriptArgs(tail*)
      else
        val newSettings = if arg.startsWith("-") then settings else settings.withModeShouldBeRun
        process(tail, newSettings.withResidualArgs(arg))

  def main(args: Array[String]): Unit =
    val scalaOpts = envOrNone("SCALA_OPTS").toArray.flatMap(_.split(" "))
    val allArgs = scalaOpts ++ args
    val settings = process(allArgs.toList, Settings())
    if settings.exitCode != 0 then System.exit(settings.exitCode)

    def run(mode: ExecuteMode): Unit = mode match
      case ExecuteMode.Repl =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
        repl.Main.main(properArgs.toArray)
      case ExecuteMode.Run =>
        val scalaClasspath = ClasspathFromClassloader(Thread.currentThread().getContextClassLoader).split(classpathSeparator)
        val newClasspath = (settings.classPath ++ scalaClasspath :+ ".").map(File(_).toURI.toURL)
        errorFn("", ObjectRunner.runAndCatch(newClasspath, settings.residualArgs.head, settings.residualArgs.drop(1)))
      case ExecuteMode.Script =>
        val properArgs =
          List("-classpath", settings.classPath.mkString(classpathSeparator)).filter(Function.const(settings.classPath.nonEmpty))
            ++ settings.residualArgs
            ++ (if settings.save then List("-save") else Nil)
            ++ List("-script", settings.targetScript)
            ++ settings.scalaArgs
            ++ settings.scriptArgs
        scripting.Main.main(properArgs.toArray)
      case ExecuteMode.Guess =>
        if settings.modeShouldBeRun then
          run(ExecuteMode.Run)
        else
          run(ExecuteMode.Repl)

    run(settings.executeMode)


  def errorFn(str: String, e: Option[Throwable] = None, isFailure: Boolean = true): Boolean = {
    if (str.nonEmpty) Console.err.println(str)
    e.foreach(_.printStackTrace())
    !isFailure
  }
}

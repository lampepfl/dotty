package dotty.tools
package repl

import scala.language.unsafeNulls

import vulpix.TestConfiguration
import vulpix.FileDiff

import java.lang.System.{lineSeparator => EOL}
import java.io.{ByteArrayOutputStream, File => JFile, PrintStream}
import java.nio.charset.StandardCharsets

import scala.io.Source
import scala.util.Using
import scala.collection.mutable.ArrayBuffer

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.MessageRendering
import org.junit.{After, Before}
import org.junit.Assert._

class ReplTest(options: Array[String] = ReplTest.defaultOptions, out: ByteArrayOutputStream = new ByteArrayOutputStream)
extends ReplDriver(options, new PrintStream(out, true, StandardCharsets.UTF_8.name)) with MessageRendering:
  /** Get the stored output from `out`, resetting the buffer */
  def storedOutput(): String = {
    val output = stripColor(out.toString(StandardCharsets.UTF_8.name))
    out.reset()
    output
  }

  /** Make sure the context is new before each test */
  @Before def init(): Unit =
    resetToInitial()

  /** Reset the stored output */
  @After def cleanup: Unit =
    storedOutput()

  def initially[A](op: State ?=> A): A = op(using initialState)

  def contextually[A](op: Context ?=> A): A = op(using initialState.context)

  /** Returns the `(<instance completions>, <companion completions>)`*/
  def tabComplete(src: String)(implicit state: State): List[String] =
    completionsWithSignatures(src.length, src, state).map(_.label).sorted.distinct

  extension [A](state: State)
    infix def andThen(op: State ?=> A): A = op(using state)

  def testFile(f: JFile): Unit = testScript(f.toString, readLines(f), Some(f))

  def testScript(name: => String, lines: List[String], scriptFile: Option[JFile] = None): Unit = {
    val prompt = "scala>"

    def evaluate(state: State, input: String) =
      try {
        val nstate = run(input.drop(prompt.length))(using state)
        val out = input + EOL + storedOutput()
        (out, nstate)
      }
      catch {
        case ex: Throwable =>
          System.err.println(s"failed while running script: $name, on:\n$input")
          throw ex
      }

    def filterEmpties(line: String): List[String] =
      line.replaceAll("""(?m)\s+$""", "") match {
        case "" => Nil
        case nonEmptyLine => nonEmptyLine :: Nil
      }
    def nonBlank(line: String): Boolean = line.exists(!Character.isWhitespace(_))

    val expectedOutput = lines.filter(nonBlank)
    val actualOutput = {
      val opts = toolArgsFor(ToolName.Scalac, scriptFile.map(_.toString))(lines.take(1))
      val (optsLine, inputLines) = if opts.isEmpty then ("", lines) else (lines.head, lines.drop(1))
      resetToInitial(opts)

      assert(inputLines.head.startsWith(prompt),
        s"""Each script must start with the prompt: "$prompt"""")
      val inputRes = inputLines.filter(_.startsWith(prompt))

      val buf = new ArrayBuffer[String]
      inputRes.foldLeft(initialState) { (state, input) =>
        val (out, nstate) = evaluate(state, input)
        out.linesIterator.foreach(buf.append)
        nstate
      }
      (optsLine :: buf.toList).filter(nonBlank)
    }

    if !FileDiff.matches(actualOutput, expectedOutput) then
      // Some tests aren't file-based but just pass a string, so can't update anything then
      // Also the files here are the copies in target/ not the original, so you need to vimdiff/mv them...
      if dotty.Properties.testsUpdateCheckfile && scriptFile != None then
        val checkFile = scriptFile.get
        FileDiff.dump(checkFile.toPath.toString, actualOutput)
        println(s"Wrote updated script file to $checkFile")
      else
        println("expected =========>")
        println(expectedOutput.mkString(EOL))
        println("actual ===========>")
        println(actualOutput.mkString(EOL))

        fail(s"Error in script $name, expected output did not match actual")
    end if
  }

object ReplTest:
  val commonOptions = Array("-color:never", "-pagewidth", "80")
  val defaultOptions = commonOptions ++ Array("-classpath", TestConfiguration.basicClasspath)
  lazy val withStagingOptions = commonOptions ++ Array("-classpath", TestConfiguration.withStagingClasspath)

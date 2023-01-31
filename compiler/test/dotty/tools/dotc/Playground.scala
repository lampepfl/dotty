package dotty.tools.dotc

import dotty.tools.vulpix._
import org.junit.Test
import org.junit.Ignore

class Playground:
  import TestConfiguration._
  import CompilationTests._

  implicit val testGroup: TestGroup = TestGroup("playground")

  @Test def example: Unit =
    compileFile("tests/playground/example.scala", defaultOptions).checkCompile()


  @Test def pos: Unit =
    compileFile("tests/pos/namedPatternMatching.scala", defaultOptions).checkCompile()

  @Test def negativeTests: Unit =
    compileFile("tests/neg/negNamedPatternMatching.scala", defaultOptions).checkExpectedErrors()
    compileFile("tests/neg/bad-unapplies.scala", defaultOptions).checkExpectedErrors()
    compileFile("tests/neg/i10757.scala", defaultOptions).checkExpectedErrors()


  @Test def executionTest: Unit =
    compileFile("tests/run/runNamedPatternMatching.scala", defaultOptions).checkRuns()
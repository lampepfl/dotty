package dotty.tools.dotc

import dotty.tools.vulpix._
import org.junit.Test
import org.junit.Ignore

class Playground:
  import TestConfiguration._
  import CompilationTests._

  @Test def example: Unit =
    implicit val testGroup: TestGroup = TestGroup("playground")
    compileFile("/Users/linyxus/Workspace/dotty/issues/real-try.scala", defaultOptions).checkCompile()

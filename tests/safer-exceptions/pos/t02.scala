package test.saferExceptions.pos

import language.experimental.saferExceptions
import language.experimental.captureChecking
import test.saferExceptions.pos.JavaTestFile

given a :CanThrow[Exception] = compiletime.erasedValue
//given b :CanThrow[Exception] = compiletime.erasedValue

@throws[Exception]
def checkJavaFunction() throws Exception: Unit =
  //given c :CanThrow[Exception] = compiletime.erasedValue
  JavaTestFile.javatest()
/*
-- Warning: tests\safer-exceptions\pos\t02.scala:6:48 --------------------------
6 |def checkJavaFunction : Unit = JavaTestFile.test()
  |                               ^^^^^^^^^^^^^^^^^^^
  | A Java function was called (test) in a context where safer exceptions is enabled.
  | This function might throw an exception.
  | Handling of Java method is yet to be implemented.

*/

def aa throws Exception: Unit =
  checkJavaFunction()

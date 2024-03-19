package org.scalajs.testsuite.jsinterop

import scala.language.dynamics

import org.junit.Assert.*
import org.junit.Test

import scala.scalajs.js
import scala.scalajs.js.annotation.*

class CustomDynamicTestScala3 {
  import CustomDynamicTestScala3.*

  @Test
  def testCustomDynamicClass_Issue19528(): Unit = {
    val obj = new CustomDynamicClass()

    assertEquals(false, obj.hasOwnProperty("foo"))
    obj.foo = "bar"
    assertEquals("bar", obj.foo)
    assertEquals(true, obj.hasOwnProperty("foo"))
  }

  @Test
  def testCustomDynamicTrait_Issue19528(): Unit = {
    val obj = new js.Object().asInstanceOf[CustomDynamicTrait]

    assertEquals(false, obj.hasOwnProperty("foo"))
    obj.foo = "bar"
    assertEquals("bar", obj.foo)
    assertEquals(true, obj.hasOwnProperty("foo"))
  }
}

object CustomDynamicTestScala3 {
  @js.native
  @JSGlobal("Object")
  class CustomDynamicClass extends js.Any with Dynamic {
    @JSBracketAccess
    def selectDynamic(name: String): js.Any = js.native
    @JSBracketAccess
    def updateDynamic(name: String)(value: js.Any): Unit = js.native

    @JSBracketCall
    def applyDynamic(name: String)(args: js.Any*): js.Any = js.native
  }

  @js.native
  trait CustomDynamicTrait extends js.Any with Dynamic {
    @JSBracketAccess
    def selectDynamic(name: String): js.Any = js.native
    @JSBracketAccess
    def updateDynamic(name: String)(value: js.Any): Unit = js.native

    @JSBracketCall
    def applyDynamic(name: String)(args: js.Any*): js.Any = js.native
  }
}

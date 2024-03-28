package dotty.tools.languageserver

import org.junit.Assert.{assertEquals, assertTrue, assertFalse}
import org.junit.Test
import org.eclipse.lsp4j.CompletionItemKind._

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.CodeTester
import dotty.tools.languageserver.util.actions.CodeCompletion

class CompletionTest {

  @Test def completion0: Unit = {
    code"class Foo { val xyz: Int = 0; def y: Int = xy${m1} }"
      .completion(("xyz", Field, "Int"))
  }

  @Test def completionFromScalaPredef: Unit = {
    code"class Foo { def foo: Unit = prin${m1} }"
      .completion(
        ("print", Method, "(x: Any): Unit"),
        ("printf", Method, "(text: String, xs: Any*): Unit"),
        ("println", Method, "(x: Any): Unit"),
        ("println", Method, "(): Unit"),
      )
  }

  @Test def completionFromNewScalaPredef: Unit = {
    code"class Foo { val foo = summ${m1} }"
      .completion(("summon", Method, "[T](using x: T): x.type"))
  }

  @Test def completionFromScalaPackage: Unit = {
    code"class Foo { val foo: Conv${m1} }"
      .completion(("Conversion", Class, "Conversion"))
  }

  @Test def implicitSearchCrash: Unit =
    code"""
          |object Test:
          |  trait Foo:
          |    def test(): String
          |  given Int = ???
          |  given (using ev: Int): Conversion[String, Foo] = ???
          |
          |  val test = {
          |    "".tes$m1
          |    1
          |  }"""
      .completion(("test", Method, "(): String"))

  @Test def completionFromScalaPackageObject: Unit = {
    code"class Foo { val foo: BigD${m1} }"
      .completion(
        ("BigDecimal", Field, "BigDecimal"),
        ("BigDecimal", Field, "scala.math.BigDecimal"),
      )
  }

  @Test def completionFromSyntheticPackageObject: Unit = {
    code"class Foo { val foo: IArr${m1} }"
      .completion(
        ("IArray", Module, "IArray"),
        ("IArray", Field, "IArray"),
      )
  }

  @Test def completionFromJavaDefaults: Unit = {
    code"class Foo { val foo: Runn${m1} }"
      .completion(
        ("Runnable", Class, "Runnable"),
        ("Runnable", Module, "Runnable"),
      )
  }

  @Test def completionWithImplicitConversion: Unit = {
    withSources(
      code"object Foo { implicit class WithBaz(bar: Bar) { def baz = 0 } }",
      code"class Bar",
      code"object Main { import Foo._; val bar: Bar = new Bar; bar.b${m1} }"
    ).completion(("baz", Method, "=> Int"))
  }

  // TODO: Also add tests with concrete classes, where the completion will
  // include the constructor proxy companion

  @Test def importCompleteClassWithPrefix: Unit = {
    withSources(
      code"""object Foo { abstract class MyClass }""",
      code"""import Foo.My${m1}"""
    ).completion(("MyClass", Class, "Foo.MyClass"))
  }

  @Test def importCompleteClassNoPrefix: Unit = {
    withSources(
      code"""object Foo { abstract class MyClass }""",
      code"""import Foo.${m1}"""
    ).completion(completionItems => {
      val results = CodeCompletion.simplifyResults(completionItems)
      val myClass = ("MyClass", Class, "Foo.MyClass")
      assertTrue(results.contains(("MyClass", Class, "Foo.MyClass")))

      // Verify that apart from `MyClass`, we only have the methods that exists on `Foo`
      assertTrue((results - myClass).forall { case (_, kind, _) => kind == Method })

      // Verify that we don't have things coming from an implicit conversion, such as ensuring
      assertFalse(results.exists { case (name, _, _) => name == "ensuring" })
    })
  }

  @Test def importCompleteFromPackage: Unit = {
    withSources(
      code"""|package a
             |abstract class MyClass""",
      code"""|package b
             |import a.My${m1}"""
    ).completion(("MyClass", Class, "a.MyClass"))
  }

  @Test def importCompleteFromClass: Unit = {
    withSources(
      code"""abstract class Foo { val x: Int = 0 }""",
      code"""import Foo.${m1}"""
    ).noCompletions()
  }

  @Test def importCompleteIncludesSynthetic: Unit = {
    code"""|case class MyCaseClass(foobar: Int)
           |object O {
           |  val x = MyCaseClass(0)
           |  import x.c${m1}
           |}"""
      .completion(
        ("copy", Method, "(foobar: Int): MyCaseClass"),
        ("canEqual", Method, "(that: Any): Boolean"),
      )
  }

  @Test def importCompleteIncludeModule: Unit = {
    withSources(
      code"""object O { object MyObject }""",
      code"""import O.My${m1}"""
    ).completion(("MyObject", Module, "O.MyObject"))
  }

  @Test def importCompleteWithClassAndCompanion: Unit = {
    withSources(
      code"""|package pkg0
             |class Foo
             |object Foo""",
      code"""|package pgk1
             |import pkg0.F${m1}"""
    ).completion(
        ("Foo", Class, "pkg0.Foo"),
        ("Foo", Module, "pkg0.Foo"),
      )
  }

  @Test def importCompleteIncludePackage: Unit = {
    withSources(
      code"""|package foo.bar
             |abstract classFizz""",
      code"""import foo.b${m1}"""
    ).completion(("bar", Module, "foo.bar"))
  }

  @Test def importCompleteIncludeMembers: Unit = {
    withSources(
      code"""|object MyObject {
             |  val myVal = 0
             |  def myDef = 0
             |  var myVar = 0
             |  object myObject
             |  abstract class myClass
             |  trait myTrait
             |}""",
      code"""import MyObject.my${m1}"""
    ).completion(
        ("myVal", Field, "Int"),
        ("myDef", Method, "=> Int"),
        ("myVar", Variable, "Int"),
        ("myObject", Module, "MyObject.myObject"),
        ("myClass", Class, "MyObject.myClass"),
        ("myTrait", Class, "MyObject.myTrait"),
      )
  }

  @Test def importJavaClass: Unit = {
    code"""import java.io.FileDesc${m1}"""
      .completion(
        ("FileDescriptor", Class, "java.io.FileDescriptor"),
        ("FileDescriptor", Module, "java.io.FileDescriptor"),
      )
  }

  @Test def importJavaStaticMethod: Unit = {
    code"""import java.lang.System.lineSep${m1}"""
      .completion(("lineSeparator", Method, "(): String"))
  }

  @Test def importJavaStaticField: Unit = {
    code"""import java.lang.System.ou${m1}"""
      .completion(("out", Field, "java.io.PrintStream"))
  }

  @Test def importFromExplicitAndSyntheticPackageObject: Unit = {
    withSources(
      code"package foo.bar; trait XXXX1",
      code"package foo; package object bar { trait XXXX2 }",
      code"object Main { import foo.bar.XX${m1} }"
    ).completion(
        ("XXXX1", Class, "foo.bar.XXXX1"),
        ("XXXX2", Class, "foo.bar.XXXX2"),
      )
  }

  @Test def completeJavaModuleClass: Unit = {
    code"""|object O {
           |  val out = java.io.FileDesc${m1}
           |}"""
      .completion(("FileDescriptor", Module, "java.io.FileDescriptor"))
  }

  @Test def importRename: Unit = {
    code"""import java.io.{FileDesc${m1} => Foo}"""
      .completion(
        ("FileDescriptor", Class, "java.io.FileDescriptor"),
        ("FileDescriptor", Module, "java.io.FileDescriptor"),
      )
  }

  @Test def noImportRename: Unit = {
    code"""import java.io.{FileDescriptor => Fo$m1}"""
      .noCompletions()
  }

  @Test def importGivenByType: Unit = {
    code"""|trait Foo
           |object Bar
           |import Bar.{given Fo$m1}"""
      .completion(("Foo", Class, "Foo"))
  }

  @Test def markDeprecatedSymbols: Unit = {
    code"""|object Foo {
           |  @deprecated
           |  val bar = 0
           |}
           |import Foo.ba${m1}"""
      .completion(results => {
        assertEquals(1, results.size)
        val result = results.head
        assertEquals("bar", result.getLabel)
        assertTrue("bar was not deprecated", result.getDeprecated)
      })
  }

  @Test def i4397: Unit = {
    code"""class Foo {
          |  .${m1}
          |}"""
      .noCompletions()
  }

  @Test def completeNoPrefix: Unit = {
    code"""class Foo { def foo = 0 }
          |object Bar {
          |  val foo = new Foo
          |  foo.${m1}
          |}"""
      .completion(results => assertTrue(results.nonEmpty))
  }

  @Test def completeErrorKnowsKind: Unit = {
    code"""object Bar {
          |  abstract class Zig
          |  val Zag: Int = 0
          |  val b = 3 + Bar.${m1}
          |}"""
      .completion(completionItems => {
        val results = CodeCompletion.simplifyResults(completionItems)
        assertTrue(results.contains(("Zag", Field, "Int")))
        assertFalse(results.exists((label, _, _) => label == "Zig"))
      })
  }

  @Test def typeCompletionShowsTerm: Unit = {
    code"""class Bar
          |object Foo {
          |  val bar = new Bar
          |  def baz = new Bar
          |  object bat
          |  val bizz: ba${m1}
          |}"""
      .completion(
        ("bar", Field, "Bar"),
        ("bat", Module, "Foo.bat"),
      )
  }

  @Test def completionOnRenamedImport: Unit = {
    code"""import java.io.{FileDescriptor => AwesomeStuff}
           trait Foo { val x: Awesom${m1}}"""
      .completion(
        ("AwesomeStuff", Class, "java.io.FileDescriptor"),
        ("AwesomeStuff", Module, "java.io.FileDescriptor"),
      )
  }

  @Test def completionOnRenamedImport2: Unit = {
    code"""|import java.util.{HashMap => MyImportedSymbol}
           |trait Foo {
           |  import java.io.{FileDescriptor => MyImportedSymbol}
           |  val x: MyImp${m1}
           |}"""
      .completion(
        ("MyImportedSymbol", Class, "java.io.FileDescriptor"),
        ("MyImportedSymbol", Module, "java.io.FileDescriptor"),
      )
  }

  @Test def completionRenamedAndOriginalNames: Unit = {
    code"""import java.util.HashMap
          |trait Foo {
          |  import java.util.{HashMap => HashMap2}
          |  val x: Hash${m1}
          |}"""
      .completion(
        ("HashMap", Class, "java.util.HashMap"),
        ("HashMap", Module, "java.util.HashMap"),
        ("HashMap2", Class, "java.util.HashMap"),
        ("HashMap2", Module, "java.util.HashMap"),
      )
  }

  @Test def completionRenamedThrice: Unit = {
    code"""import java.util.{HashMap => MyHashMap}
          |import java.util.{HashMap => MyHashMap2}
          |trait Foo {
          |  import java.util.{HashMap => MyHashMap3}
          |  val x: MyHash${m1}
          |}"""
      .completion(
        ("MyHashMap", Class, "java.util.HashMap"),
        ("MyHashMap", Module, "java.util.HashMap"),
        ("MyHashMap2", Class, "java.util.HashMap"),
        ("MyHashMap2", Module, "java.util.HashMap"),
        ("MyHashMap3", Class, "java.util.HashMap"),
        ("MyHashMap3", Module, "java.util.HashMap"),
      )
  }

  @Test def completeFromWildcardImports: Unit = {
    code"""object Foo {
          |  val fooFloat: Float = 1.0
          |  val fooLong: Long = 0L
          |  given fooInt: Int = 0
          |  given fooString: String = ""
          |}
          |object Test1 { import Foo.{fooFloat => _, _}; foo${m1} }
          |object Test2 { import Foo.given; foo${m2} }
          |object Test3 { import Foo.{given String}; foo${m3} }
          |object Test4 { import Foo.{_, given String}; foo${m4} }
          |object Test5 { import Foo.{fooFloat, given}; foo${m5} }
          |object Test6 { import Foo.{fooInt => _, fooString => fooStr, given}; foo${m6} }
          |object Test7 { import Foo.{fooLong => fooInt, given Int}; foo${m7} }
          """
      .completion(m1, ("fooLong", Field, "Long"))
      .completion(m2, ("fooInt", Field, "Int"), ("fooString", Field, "String"))
      .completion(m3, ("fooString", Field, "String"))
      .completion(m4, ("fooLong", Field, "Long"), ("fooFloat", Field, "Float"), ("fooString", Field, "String"))
      .completion(m5, ("fooFloat", Field, "Float"), ("fooInt", Field, "Int"), ("fooString", Field, "String"))
      .completion(m6, ("fooStr", Field, "String"))
      .completion(m7, ("fooInt", Field, "Long"))
  }

  @Test def dontCompleteFromAmbiguousImportsFromSameSite: Unit = {
    code"""object Foo {
          |  val i = 0
          |  val j = 1
          |}
          |object Test {
          |  import Foo.{i => xxxx, j => xxxx}
          |  val x = xx${m1}
          |}"""
      .noCompletions()
  }

  @Test def collectNamesImportedInNestedScopes: Unit = {
    code"""object Foo {
          |  val xxxx1 = 1
          |}
          |object Bar {
          |  val xxxx2 = 2
          |}
          |object Baz {
          |  val xxxx3 = 3
          |}
          |object Test {
          |  import Foo.xxxx1
          |  locally {
          |    import Bar.xxxx2
          |    locally {
          |      import Baz.xxxx3
          |      val x = xx${m1}
          |    }
          |  }
          |}"""
      .completion(
        ("xxxx1", Field, "Int"),
        ("xxxx2", Field, "Int"),
        ("xxxx3", Field, "Int"),
      )
  }

  @Test def completeEnclosingObject: Unit = {
    code"""object Test {
          |  def x = Tes${m1}
          |}"""
      .completion(("Test", Module, "Test"))
  }

  @Test def completeBothDefinitionsForEqualNestingLevels: Unit = {
    code"""trait Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |trait Bar {
          |  def xxxx(s: String): String = s
          |}
          |object Test extends Foo, Bar {
          |  val x = xx${m1}
          |}"""
      .completion(
        ("xxxx", Method, "(s: String): String"),
        ("xxxx", Method, "(i: Int): Int"),
      )
  }

  @Test def dontCompleteFromAmbiguousImportsForEqualNestingLevels: Unit = {
    code"""object Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |object Bar {
          |  def xxxx(s: String): String = s
          |}
          |object Test {
          |  import Foo.xxxx
          |  import Bar.xxxx
          |  val x = xx${m1}
          |}"""
      .noCompletions()
  }

  @Test def completeFromSameImportsForEqualNestingLevels: Unit = {
    code"""object Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |object Test {
          |  import Foo.xxxx
          |  import Foo.xxxx
          |  import Foo.xxxx
          |  val x = xx${m1}
          |}"""
      .completion(("xxxx", Method, "(i: Int): Int"))
  }

  @Test def preferLocalDefinitionToImportForEqualNestingLevels: Unit = {
    code"""object Foo {
          |  val xxxx = 1
          |}
          |object Test {
          |  def xxxx(s: String): String = s
          |  import Foo.xxxx
          |  val x = xx${m1}
          |}"""
      .completion(("xxxx", Method, "(s: String): String"))
  }

  @Test def preferMoreDeeplyNestedDefinition: Unit = {
    code"""object Test {
          |  def xxxx(i: Int): Int = i
          |  object Inner {
          |    def xxxx(s: String): String = s
          |    val x = xx${m1}
          |  }
          |}"""
      .completion(("xxxx", Method, "(s: String): String"))
  }

  @Test def preferMoreDeeplyNestedImport: Unit = {
    code"""object Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |object Bar {
          |  def xxxx(s: String): String = s
          |}
          |object Test {
          |  import Foo.xxxx
          |  locally {
          |    import Bar.xxxx
          |    val x: String = xx${m1}
          |  }
          |}"""
      .completion(("xxxx", Method, "(s: String): String"))
  }

  @Test def preferMoreDeeplyNestedLocalDefinitionToImport: Unit = {
    code"""object Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |object Test {
          |  import Foo.xxxx
          |  object Inner {
          |    def xxxx(s: String): String = s
          |    val x: String = xx${m1}
          |  }
          |}"""
      .completion(("xxxx", Method, "(s: String): String"))
  }

  @Test def dontCompleteLocalDefinitionShadowedByImport: Unit = {
    code"""object XXXX {
          |  val xxxx = 1
          |}
          |object Test {
          |  locally {
          |    val xxxx = ""
          |    locally {
          |      import XXXX.xxxx // import conflicts with val from outer scope
          |      val y = xx${m1}
          |    }
          |  }
          |}"""
      .noCompletions()
  }

  @Test def completeFromLocalDefinitionIgnoringLessDeeplyNestedAmbiguities: Unit = {
    code"""object XXXX {
          |  val xxxx = 1
          |}
          |object Test {
          |  locally {
          |    val xxxx = ""
          |    locally {
          |      import XXXX.xxxx // import conflicts with val from outer scope
          |      locally {
          |        val xxxx = 'a' // shadows both the import and the val from outer scope
          |        val y = xx${m1}
          |      }
          |    }
          |  }
          |}"""
      .completion(("xxxx", Field, "Char"))
  }

  @Test def completionClassAndMethod: Unit = {
    code"""object Foo {
          |  class bar
          |  def bar(i: Int) = 0
          |}
          |import Foo.b${m1}"""
      .completion(
        ("bar", Class, "Foo.bar"),
        ("bar", Method, "(i: Int): Int"),
      )
  }

  @Test def completionTypeAndLazyValue: Unit = {
    code"""object Foo {
          |  type bar = Int
          |  lazy val bar = 3
          |}
          |import Foo.b${m1}"""
      .completion(
        ("bar", Field, "Foo.bar"),
        ("bar", Field, "Int"),
      )
  }

  @Test def keepTrackOfTermsAndTypesSeparately: Unit = {
    code"""object XXXX {
          |  object YYYY
          |  type YYYY = YYYY.type
          |}
          |object Test {
          |  import XXXX._
          |  val YYYY = Int
          |  val ZZZZ = YY${m1}
          |  type ZZZZ = YY${m2}
          |}"""
      .completion(m1, ("YYYY", Field, "Int"))
      .completion(m2, ("YYYY", Field, "XXXX.YYYY"), ("YYYY", Field, "Int"))
  }

  @Test def completeRespectingAccessModifiers: Unit = {
    code"""trait Foo {
          |  def xxxx1 = ""
          |  protected def xxxx2 = ""
          |  private def xxxx3 = ""
          |}
          |object Test1 extends Foo {
          |  xx${m1}
          |}
          |object Test2 {
          |  val foo = new Foo {}
          |  foo.xx${m2}
          |}"""
      .completion(m1, ("xxxx1", Method, "=> String"), ("xxxx2", Method, "=> String"))
      .completion(m2, ("xxxx1", Method, "=> String"))
  }

  @Test def completeFromPackageObjectWithInheritance: Unit = {
    code"""package test
          |trait Foo[A] { def xxxx(a: A) = a }
          |package object foo extends Foo[Int] {}
          |object Test {
          |  foo.xx${m1}
          |}"""
      .completion(("xxxx", Method, "(a: Int): Int"))
  }

  @Test def completePrimaryConstructorParameter: Unit = {
    val expected = Set(("abc", Field, "Int"))

    code"""class Foo(abc: Int) {
          |  ab${m1}
          |  def method1: Int = {
          |    ab${m2}
          |    42
          |  }
          |  def method2: Int = {
          |    val smth = ab${m3}
          |    42
          |  }
          |}"""
      .completion(m1, expected)
      .completion(m2, expected)
      .completion(m3, expected)
  }

  @Test def completeExtensionReceiver: Unit = {
    code"""extension (string: String) def xxxx = str${m1}"""
      .completion(("string", Field, "String"))
  }

  @Test def completeExtensionMethodWithoutParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def completeExtensionMethodWithParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx(i: Int) = i
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "(i: Int): Int"))
  }

  @Test def completeExtensionMethodWithTypeParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx[A]: Int = 1
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "[A]: Int"))
  }

  @Test def completeExtensionMethodWithParameterAndTypeParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx[A](a: A) = a
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "[A](a: A): A"))
  }

  @Test def completeExtensionMethodFromExtensionWithTypeParameter: Unit = {
    code"""extension [A](a: A) def xxxx: A = a
          |object Main { "abc".xx${m1} }"""
      .completion(("xxxx", Method, "=> String"))
  }

  @Test def completeExtensionMethodWithResultTypeDependantOnReceiver: Unit = {
    code"""trait Foo { type Out; def get: Out}
          |object Bar extends Foo { type Out = String; def get: Out = "abc"}
          |extension (foo: Foo) def xxxx: foo.Out = foo.get
          |object Main { Bar.xx${m1} }"""
      .completion(("xxxx", Method, "=> String"))
  }

  @Test def completeExtensionMethodFromExtenionWithPrefixUsingSection: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (using Bar, Baz)(foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def completeExtensionMethodFromExtenionWithMultiplePrefixUsingSections: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (using Bar)(using Baz)(foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def dontCompleteExtensionMethodFromExtenionWithMissingImplicitFromPrefixUsingSection: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Baz with {}
          |extension (using Bar, Baz)(foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }"""
      .noCompletions()
  }

  @Test def completeExtensionMethodForReceiverOfTypeDependentOnLeadingImplicits: Unit = {
    code"""
          |trait Foo:
          |  type Out <: Bar
          |
          |given Foo with
          |  type Out = Baz
          |
          |trait Bar:
          |  type Out
          |
          |trait Baz extends Bar
          |
          |given Baz with
          |  type Out = Quux
          |
          |class Quux
          |
          |object Quux:
          |  extension (using foo: Foo)(using fooOut: foo.Out)(fooOutOut: fooOut.Out) def xxxx = "abc"
          |
          |object Main { (new Quux).xx${m1} }"""
      .completion(("xxxx", Method, "=> String"))
  }

  @Test def completeExtensionMethodWithResultTypeDependentOnLeadingImplicit: Unit = {
    code"""object Foo
          |trait Bar { type Out; def get: Out }
          |given Bar with { type Out = 123; def get: Out = 123 }
          |extension (using bar: Bar)(foo: Foo.type) def xxxx: bar.Out = bar.get
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> (123 : Int)"))
  }

  @Test def completeExtensionMethodFromExtenionWithPostfixUsingSection: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (foo: Foo.type)(using Bar, Baz) def xxxx = 1
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "(using x$2: Bar, x$3: Baz): Int"))
  }

  @Test def completeExtensionMethodFromExtenionWithMultiplePostfixUsingSections: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (foo: Foo.type)(using Bar)(using Baz) def xxxx = 1
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "(using x$2: Bar)(using x$3: Baz): Int"))
  }

  @Test def completeExtensionMethodWithTypeParameterFromExtenionWithTypeParametersAndPrefixAndPostfixUsingSections: Unit = {
    code"""trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension [A](using bar: Bar)(a: A)(using baz: Baz) def xxxx[B]: Either[A, B] = Left(a)
          |object Main { 123.xx${m1} }"""
      .completion(("xxxx", Method, "(using baz: Baz)[B]: Either[Int, B]"))
  }

  @Test def completeExtensionMethodWithTypeBounds: Unit = {
    code"""trait Foo
          |trait Bar extends Foo
          |given Bar with {}
          |extension [A >: Bar](a: A) def xxxx[B <: a.type]: Either[A, B] = Left(a)
          |val foo = new Foo {}
          |object Main { foo.xx${m1} }"""
          .completion(("xxxx", Method, "[B <: (foo : Foo)]: Either[Foo, B]"))
  }

  @Test def completeInheritedExtensionMethod: Unit = {
    code"""object Foo
          |trait FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main extends FooOps { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def completeExtensionMethodWithoutLosingTypeParametersFromGivenInstance: Unit = {
    code"""trait ListOps[A] {
          |  extension (xs: List[A]) def xxxx = xs
          |}
          |given ListOps[Int] with {}
          |object Main { List(1, 2, 3).xx${m1} }"""
      .completion(("xxxx", Method, "=> List[Int]"))
  }

  @Test def completeRenamedExtensionMethod: Unit = {
    code"""object Foo
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |import FooOps.{xxxx => yyyy}
          |object Main { Foo.yy${m1} }"""
      .completion(("yyyy", Method, "=> Int"))
  }

  @Test def completeExtensionMethodFromGivenInstanceDefinedInScope: Unit = {
    code"""object Foo
          |trait FooOps
          |given FooOps with {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def completeExtensionMethodFromImportedGivenInstance: Unit = {
    code"""object Foo
          |trait FooOps
          |object Bar {
          |  given FooOps with {
          |    extension (foo: Foo.type) def xxxx = 1
          |  }
          |}
          |import Bar.given
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def completeExtensionMethodFromImplicitScope: Unit = {
    code"""case class Foo(i: Int)
          |object Foo {
          |  extension (foo: Foo) def xxxx = foo.i
          |}
          |object Main { Foo(123).xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def completeExtensionMethodFromGivenInImplicitScope: Unit = {
    code"""trait Bar
          |case class Foo(i: Int)
          |object Foo {
          |  given Bar with {
          |    extension (foo: Foo) def xxxx = foo.i
          |  }
          |}
          |object Main { Foo(123).xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def completeExtensionMethodOnResultOfImplicitConversion: Unit = {
    code"""import scala.language.implicitConversions
          |case class Foo(i: Int)
          |extension (foo: Foo) def xxxx = foo.i
          |given Conversion[Int, Foo] = Foo(_)
          |object Main { 123.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def dontCompleteExtensionMethodWithMismatchedName: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object Main { Foo.yy${m1} }"""
      .noCompletions()
  }

  @Test def preferNormalMethodToExtensionMethod: Unit = {
    code"""object Foo {
          |  def xxxx = "abcd"
          |}
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> String"))
  }

  @Test def preferExtensionMethodFromExplicitScope: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = "abcd"
          |}
          |object Main { Foo.xx${m1} }"""
      .completion(("xxxx", Method, "=> Int"))
  }

  @Test def dontCompleteExtensionMethodWithMismatchedReceiverType: Unit = {
    code"""extension (i: Int) def xxxx = i
          |object Main { "abc".xx${m1} }"""
      .noCompletions()
  }

  @Test def i13365: Unit = {
    code"""import scala.quoted._
          |
          |object Test {
          |  def test(using Quotes)(str: String) = {
          |    import quotes.reflect._
          |    val msg = Expr(str)
          |    val printHello = '{ print("sdsd") }
          |    val tree = printHello.asTerm
          |    tree.sh${m1}
          |  }
          |}"""
      .completion(("show",Method, "(using x$2: x$1.reflect.Printer[x$1.reflect.Tree]): String"))
  }

  @Test def syntheticThis: Unit = {
    code"""class Y() {
          |  def bar: Unit =
          |    val argument: Int = ???
          |    arg${m1}
          |
          |  def arg: String = ???
          |}
          |"""
      .completion(
        ("arg", Method, "=> String"),
        ("argument", Field, "Int")
      )
  }

  @Test def concatMethodWithImplicits: Unit = {
    code"""object A {
          |  Array.concat${m1}
          |}"""
      .completion(("concat", Method, "[T](xss: Array[T]*)(implicit evidence$11: scala.reflect.ClassTag[T]): Array[T]"))
  }

  @Test def i12465_hkt: Unit =
    code"""???.asInstanceOf[scala.collection.Seq].${m1}"""
      .noCompletions()

  @Test def i12465_hkt_alias: Unit =
    code"""???.asInstanceOf[Seq].${m1}"""
      .noCompletions()

  @Test def i13624_annotType: Unit =
    val expected1 = Set(("MyAnnotation", Class, "MyAnnotation"))
    val expected2 = Set(("MyAnnotation", Class, "Foo.MyAnnotation"))
    code"""object Foo{
          |  class MyAnnotation extends annotation.StaticAnnotation
          |}
          |class MyAnnotation extends annotation.StaticAnnotation
          |class Annotation2(a: String) extends annotation.StaticAnnotation
          |val x = 1: @MyAnnot${m1}
          |type X = Int @MyAnnot${m2}
          |val y = 1: @Foo.MyAnnot${m3}
          |val z = 1: @Foo.MyAnnotation @MyAnno${m4}
          |type Y = Int @MyAnnotation @Foo.MyAnnota${m5}
          |val w = 1: @Annotation2("abc": @Foo.MyAnnot${m6})
          |"""
      .completion(m1, expected1)
      .completion(m2, expected1)
      .completion(m3, expected2)
      .completion(m4, expected1)
      .completion(m5, expected2)
      .completion(m6, expected2)

  @Test def i13624_annotation : Unit =
    code"""@annotation.implicitNot${m1}
          |@annotation.implicitNotFound @mai${m2}"""
      .completion(m1, ("implicitNotFound", Class, "scala.annotation.implicitNotFound"))
      .completion(m2, ("main", Class, "main"))

  @Test def i13623_annotation : Unit =
    code"""import annot${m1}"""
      .completion(("annotation", Module, "scala.annotation"))

  @Test def importAnnotationAfterImport : Unit =
    code"""import java.lang.annotation; import annot${m1}"""
      .completion(("annotation", Module, "scala.annotation"))

  @Test def completeTemplateConstrArgType: Unit = {
    code"""import scala.concurrent.Future
          |class Foo(x: Fut${m1})"""
      .completion(
        ("Future", Class, "scala.concurrent.Future"),
        ("Future", Module, "scala.concurrent.Future")
      )
  }

  @Test def completeTemplateParents: Unit = {
    code"""import scala.concurrent.Future
          |class Foo extends Futu${m1}"""
      .completion(
        ("Future", Class, "scala.concurrent.Future"),
        ("Future", Module, "scala.concurrent.Future")
      )
  }

  @Test def completeTemplateSelfType: Unit = {
    code"""import scala.concurrent.Future
          |class Foo[A]{ self: Futu${m1} => }"""
      .completion(
        ("Future", Class, "scala.concurrent.Future"),
        ("Future", Module, "scala.concurrent.Future")
      )
  }

  @Test def backticks: Unit = {
    code"""object Foo:
          |  enum Bar:
          |    case `back-tick`
          |    case `match`
          |
          |  val x = Bar.${m1}"""
      .completion(
        ("getClass", Method, "[X0 >: Foo.Bar.type](): Class[? <: X0]"),
        ("ensuring", Method, "(cond: Boolean): Foo.Bar.type"),
        ("##", Method, "=> Int"),
        ("nn", Method, "=> Foo.Bar.type"),
        ("==", Method, "(x$0: Any): Boolean"),
        ("ensuring", Method, "(cond: Boolean, msg: => Any): Foo.Bar.type"),
        ("ne", Method, "(x$0: Object): Boolean"),
        ("valueOf", Method, "($name: String): Foo.Bar"),
        ("equals", Method, "(x$0: Any): Boolean"),
        ("wait", Method, "(x$0: Long): Unit"),
        ("hashCode", Method, "(): Int"),
        ("notifyAll", Method, "(): Unit"),
        ("values", Method, "=> Array[Foo.Bar]"),
        ("→", Method, "[B](y: B): (Foo.Bar.type, B)"),
        ("!=", Method, "(x$0: Any): Boolean"),
        ("fromOrdinal", Method, "(ordinal: Int): Foo.Bar"),
        ("asInstanceOf", Method, "[X0]: X0"),
        ("->", Method, "[B](y: B): (Foo.Bar.type, B)"),
        ("wait", Method, "(x$0: Long, x$1: Int): Unit"),
        ("`back-tick`", Field, "Foo.Bar"),
        ("notify", Method, "(): Unit"),
        ("formatted", Method, "(fmtstr: String): String"),
        ("ensuring", Method, "(cond: Foo.Bar.type => Boolean, msg: => Any): Foo.Bar.type"),
        ("wait", Method, "(): Unit"),
        ("isInstanceOf", Method, "[X0]: Boolean"),
        ("`match`", Field, "Foo.Bar"),
        ("toString", Method, "(): String"),
        ("ensuring", Method, "(cond: Foo.Bar.type => Boolean): Foo.Bar.type"),
        ("eq", Method, "(x$0: Object): Boolean"),
        ("synchronized", Method, "[X0](x$0: X0): X0")
      )
  }

  @Test def backticksPrefix: Unit = {
    code"""object Foo:
          |  enum Bar:
          |    case `back-tick`
          |    case `match`
          |
          |  val x = Bar.`back${m1}"""
      .completion(("`back-tick`", Field, "Foo.Bar"))
  }

  @Test def backticksSpace: Unit = {
    code"""object Foo:
          |  enum Bar:
          |    case `has space`
          |
          |  val x = Bar.`has s${m1}"""
      .completion(("`has space`", Field, "Foo.Bar"))
  }

  @Test def backticksCompleteBoth: Unit = {
    code"""object Foo:
          |  object Bar:
          |    val foo = 1
          |    val `foo-bar` = 2
          |    val `bar` = 3
          |
          |  val x = Bar.fo${m1}"""
      .completion(
        ("formatted", Method, "(fmtstr: String): String"),
        ("`foo-bar`", Field, "Int"),
        ("foo", Field, "Int")
      )
  }

  @Test def backticksWhenNotNeeded: Unit = {
    code"""object Foo:
          |  object Bar:
          |    val foo = 1
          |    val `foo-bar` = 2
          |
          |  val x = Bar.`fo${m1}"""
      .completion(
        ("`formatted`", Method, "(fmtstr: String): String"),
        ("`foo-bar`", Field, "Int"),
        ("`foo`", Field, "Int")
      )
  }

  @Test def backticksImported: Unit = {
    code"import scala.util.chaining.`s${m1}"
      .completion(
        ("`scalaUtilChainingOps`", Method, "[A](a: A): scala.util.ChainingOps[A]"),
        ("`synchronized`", Method, "[X0](x$0: X0): X0")
      )
  }

  @Test def matchTypeCompletions: Unit = {
    code"""case class Foo(x: Int) {
          |  def fooTest(y: Int): Int = ???
          |}
          |type Elem[X] = X match {
          |  case Int => Foo
          |  case Any => X
          |}
          |def elem[X](x: X): Elem[X] = x match {
          |  case x: Int => Foo(x)
          |  case x: Any => x
          |}
          |object Test:
          |  elem(1).foo${m1}"""
      .completion(("fooTest", Method, "(y: Int): Int"))
  }

  @Test def higherKindedMatchTypeDeclaredCompletion: Unit = {
    code"""trait Foo[A] {
          |  def map[B](f: A => B): Foo[B] = ???
          |}
          |case class Bar[F[_]](bar: F[Int])
          |type M[T] = T match {
          |  case Int => Foo[Int]
          |}
          |object Test:
          |  val x = Bar[M](new Foo[Int]{})
          |  x.bar.m${m1}"""
      .completion(("map", Method, "[B](f: Int => B): Foo[B]"))
  }

  @Test def higherKindedMatchTypeLazyCompletion: Unit = {
    code"""trait Foo[A] {
          |  def map[B](f: A => B): Foo[B] = ???
          |}
          |case class Bar[F[_]](bar: F[Int])
          |type M[T] = T match {
          |  case Int => Foo[Int]
          |}
          |def foo(x: Bar[M]) = x.bar.m${m1}"""
      .completion(("map", Method, "[B](f: Int => B): Foo[B]"))
  }

  @Test def higherKindedMatchTypeImplicitConversionCompletion: Unit = {
    code"""import scala.language.implicitConversions
          |case class Foo[A](x: A) {
          |  def mapFoo[B](op: A => B): Foo[B] = ???
          |}
          |case class Boo[A](x: A) {
          |  def mapBoo[B](op: A => B): Boo[B] = ???
          |}
          |type M[A] = A match {
          |  case Int => Foo[Int]
          |}
          |implicit def fooToBoo[A](x: Foo[A]): Boo[A] = Boo(x.x)
          |case class Bar[F[_]](bar: F[Int])
          |def foo(x: Bar[M]) = x.bar.m${m1}"""
      .completion(
        ("mapBoo", Method, "[B](op: Int => B): Boo[B]"),
        ("mapFoo", Method, "[B](op: Int => B): Foo[B]")
      )
  }

  @Test def higherKindedTypeInferenceTest: Unit = {
    val expected = Set(
        ("fooTest", Method, "(x: Int): String")
      )
    code"""class Test[A, B] {
          |  def fooTest(x: A): B = ???
          |}
          |
          |object M:
          |  val test = new Test[Int, String] {}
          |  test.foo${m1}
          |  (new Test[Int, String] {}).foo${m2}"""
      .completion(m1, expected)
      .completion(m2, expected)
  }

  @Test def higherKindedImplicitConversionsCompletions: Unit = {
    val expected = Set(
      ("mapBoo", Method, "[B](f: Int => B): Boo[B]"),
      ("mapFoo", Method, "[B](f: Int => B): Foo[B]"),
    )
    code"""import scala.language.implicitConversions
          |case class Foo[A](x: A) {
          |  def mapFoo[B](f: A => B): Foo[B] = ???
          |}
          |case class Boo[C](x: C) {
          |  def mapBoo[B](f: C => B): Boo[B] = ???
          |}
          |implicit def fooToBoo[D](x: Foo[D]): Boo[D] = Boo(x.x)
          |object Test:
          |  val x = Foo(1)
          |  x.ma${m1}
          |  Foo(1).ma${m2}"""
      .completion(m1, expected)
      .completion(m2, expected)
  }

  @Test def higherKindedMatchTypeExtensionMethodCompletion: Unit = {
    code"""trait Foo[A] {
          |  def mapFoo[B](f: A => B): Foo[B] = ???
          |}
          |extension[A] (x: Foo[A]) {
          |  def mapExtensionMethod[B](f: A => B): Foo[B] = ???
          |}
          |case class Baz[F[_]](baz: F[Int])
          |type M[T] = T match {
          |  case Int => Foo[Int]
          |}
          |case class Bar[F[_]](bar: F[Int])
          |def foo(x: Bar[M]) = x.bar.ma${m1}"""
      .completion(
        ("mapFoo", Method, "[B](f: Int => B): Foo[B]"),
        ("mapExtensionMethod", Method, "[B](f: Int => B): Foo[B]"),
      )
  }

  @Test def packageCompletionsOutsideImport: Unit = {
    code"""object Foo { ja${m1}"""
      .completion(
        ("java", Module, "java"),
        ("javax", Module, "javax"),
      )
  }

  @Test def topLevelPackagesCompletionsOutsideImport: Unit = {
    code"""package example:
          |    def foo = ""
          |
          |def main = exa${m1}"""
      .completion(("example", Module, "example"))
  }

  @Test def typeAliasCompletions: Unit = {
    code"""class Test() {
          |  def fooTest(x: Int): Unit = ???
          |}
          |type TestAlias = Test
          |object O:
          |  val test: TestAlias = new Test()
          |  test.foo${m1}"""
      .completion(("fooTest", Method, "(x: Int): Unit"))
  }

  @Test def higherKindedTypeAliasesCompletions: Unit = {
    val expected = Set(("fooTest", Method, "(x: Int): Option[Unit]"))
    code"""trait Test[F[_]] {
          |  def fooTest(x: Int): F[Unit] = ???
          |}
          |
          |type TestAlias[M[_[_]]] = M[Option]
          |object O:
          |  val test: TestAlias[Test] = new Test[Option] {}
          |  val test2: Test[Option] = new Test[Option] {}
          |  test.foo${m1}
          |  test2.foo${m2}"""
      .completion(m1, expected)
      .completion(m2, expected)
  }

  @Test def `???`: Unit = {
    code"""object O:
          |  ???.${m1}"""
      .noCompletions()
  }

  @Test def singleDenotNoCompletions: Unit = {
    code"""class Test(val symbol: String)
          |class BetterTest(symbol: Int) extends Test(symbol.toString):
          |  symb$m1
          |object O:
          |  def t(test: BetterTest) = test.symb$m2"""
      .completion(m1, ("symbol", Field, "Int"))
      .completion(m2, ("symbol", Field, "String"))
  }

  @Test def refinedNonselectable: Unit = {
    code"""trait Foo
          |trait Bar extends Foo
          |
          |trait Quux:
          |  def aaa: Foo
          |  def bbb: Foo
          |  def ccc(s: String): String
          |  private def ddd(): Boolean = ???
          |
          |val quux = new Quux:
          |  def aaa: Foo = ???
          |  def bbb: Bar = ??? // overriden signature
          |  def ccc(s: String): String = ???
          |  def ccc(i: Int): Int = ??? // overloaded
          |  private def ddd(): Boolean = ???
          |  def eee(): Boolean = ???
          |  private def fff(): Boolean = ???
          |  val ggg: Int = ???
          |
          |val a = quux.aa${m1}
          |val b = quux.bb${m2}
          |val c = quux.cc${m3}
          |val d = quux.dd${m4}
          |val e = quux.ee${m5}
          |val f = quux.ff${m6}
          |val g = quux.gg${m7}
          |object imported:
          |  import quux.*
          |  val b = bb${m8}"""
            .completion(m1, ("aaa", Method, "=> Foo"))
            .completion(m2, ("bbb", Method, "=> Bar"))
            .completion(m3, ("ccc", Method, "(s: String): String"))
            .noCompletions(m4)
            .noCompletions(m5)
            .noCompletions(m6)
            .noCompletions(m7)
            .completion(m8, ("bbb", Method, "=> Bar"))
  }

  @Test def refinedSelectable: Unit = {
    code"""trait Foo
          |trait Bar extends Foo
          |
          |trait Quux extends Selectable:
          |  def aaa: Foo
          |  def bbb: Foo
          |  def ccc(s: String): String
          |  private def ddd(): Boolean = ???
          |
          |val quux = new Quux:
          |  def aaa: Foo = ???
          |  def bbb: Bar = ??? // overriden signature
          |  def ccc(s: String): String = ???
          |  def ccc(i: Int): Int = ??? // overloaded
          |  private def ddd(): Boolean = ???
          |  def eee(): Boolean = ???
          |  private def fff(): Boolean = ???
          |  val ggg: Int = ???
          |
          |val a = quux.aa${m1}
          |val b = quux.bb${m2}
          |val c = quux.cc${m3}
          |val d = quux.dd${m4}
          |val e = quux.ee${m5}
          |val f = quux.ff${m6}
          |val g = quux.gg${m7}
          |object imported:
          |  import quux.*
          |  val b = bb${m8}"""
            .completion(m1, ("aaa", Method, "=> Foo"))
            .completion(m2, ("bbb", Method, "=> Bar"))
            .completion(m3, ("ccc", Method, "(s: String): String"), ("ccc", Method, "(i: Int): Int"))
            .noCompletions(m4)
            .completion(m5, ("eee", Method, "(): Boolean"))
            .noCompletions(m6)
            .completion(m7, ("ggg", Field, "Int"))
            .completion(m8, ("bbb", Method, "=> Bar"))
  }

  @Test def refinedSelectableFromImplicitConversion: Unit = {
    code"""case class Wrapper[A](inner: A) extends Selectable
          |object Wrapper:
          |  implicit def refineWrapper[A](wrapper: Wrapper[A])(using refiner: Refiner[A]): refiner.Refined = ???
          |
          |trait Refiner[A]:
          |  type Refined
          |
          |case class Foo(name: String)
          |object Foo:
          |  given Refiner[Foo] with
          |    type Refined = Wrapper[Foo] { def name: String }
          |
          |def fooWrapper: Wrapper[Foo] = ???
          |def name: Wrapper[String] = fooWrapper.na${m1}"""
      .completion(m1, Set(("name", Method, "=> String")))
  }

  @Test def transparentMacro: Unit = {
    val p1 = Project.withSources(
      code"""package p1
            |import scala.quoted.*
            |
            |trait Foo:
            |  def xxxa = 0
            |
            |class Bar extends Foo:
            |  def xxxb = 1
            |
            |transparent inline def bar: Foo = $${ barImpl }
            |def barImpl(using Quotes) = '{ new Bar }
            |"""
    )
    val p2 = Project.dependingOn(p1).withSources(
      code"""package p2
            |val x = p1.bar.xx${m1}
            """
    )
    withProjects(p1, p2).completion(m1, Set(("xxxa", Method, "=> Int"), ("xxxb", Method, "=> Int")))
  }


  @Test def implicitlyRefinedWithTransparentMacro: Unit = {
    val p1 = Project.withSources(
      code"""package p1
            |import scala.quoted.*
            |import scala.language.implicitConversions
            |
            |case class Wrapper[A](inner: A) extends Selectable:
            |  def selectDynamic(name: String) = ???
            |object Wrapper:
            |  implicit def refineWrapper[A](wrapper: Wrapper[A])(using refiner: Refiner[A]): refiner.Refined = ???
            |
            |trait Refiner[A]:
            |  type Refined
            |
            |case class Foo(name: String)
            |object Foo:
            |  transparent inline given fooRefiner: Refiner[Foo] = $${ fooRefinerImpl }
            |
            |def fooRefinerImpl(using Quotes): Expr[Refiner[Foo]] = '{
            |  new Refiner[Foo] {
            |    type Refined = Wrapper[Foo] { def name: String }
            |  }
            |}
            |"""
    )
    val p2 = Project.dependingOn(p1).withSources(
      code"""package p2
            |import p1.*
            |def fooWrapper: Wrapper[Foo] = ???
            |def name = fooWrapper.na${m1}
            """
    )
    withProjects(p1, p2).completion(m1, Set(("name", Method, "=> String")))
  }

  @Test def generatedValDefCompletions: Unit = {
    val expected = ("testMethod", Method, "=> Unit")
    code"""case class Test(x: Int, y: Int)
          |def testMethod: Unit = ???
          |object M:
          |  val (x, y) =
          |    testMet$m1
          |    (1, 2)
          |  val Test(x, y) =
          |    testMet$m2
          |    Test(1, 2)
          """
      .completion(m1, expected)
      .completion(m2, expected)
  }

  @Test def exportCompletions: Unit = {
    code"""object Foo:
          |  def xDef = 1
          |  val xVal = 1
          |  class xClass()
          |  object xObject {}
          |object Test:
          |  export Foo.x${m1}
          """
      .completion(m1, Set(
        ("xDef", Method, "=> Int"),
        ("xVal", Field, "Int"),
        ("xObject", Module, "Foo.xObject"),
        ("xClass", Class, "Foo.xClass")))
  }

  @Test def patternGuardCompletions: Unit = {
    code"""object Foo:
          |  1 match { case foo if fo${m1} => }
          |  1 match { case foo => fo${m2} }
          """
      .completion(m1, Set(("foo", Field, "Int")))
      .completion(m2, Set(("foo", Field, "Int")))
  }

  @Test def patternGuardCompletionsUnApply: Unit = {
    code"""object Foo:
          |  Some(1) match { case Some(foo) if fo${m1} => }
          |  Some(1) match { case Some(foo) => fo${m2} }
          """
      .completion(m1, Set(("foo", Field, "Int")))
      .completion(m2, Set(("foo", Field, "Int")))
  }

  @Test def patternGuardCompletionsNested: Unit = {
    code"""object Foo:
          |  ((1, 2), 3) match { case ((foo1, foo2), foo3) if fo${m1} => }
          |  ((1, 2), 3) match { case ((foo1, foo2), foo3) => fo${m2} }
          """
      .completion(m1, Set(("foo1", Field, "Int"), ("foo2", Field, "Int"), ("foo3", Field, "Int")))
      .completion(m2, Set(("foo1", Field, "Int"), ("foo2", Field, "Int"), ("foo3", Field, "Int")))
  }

  @Test def patternGuardCompletionsSeq: Unit = {
    code"""object Foo:
          |  Seq(1, 2) match { case foo1 :: foo2 if fo${m1} => }
          |  Seq(1, 2) match { case foo1 :: foo2 => fo${m2} }
          """
      .completion(m1, Set(("foo1", Field, "Int"), ("foo2", Field, "List[Int]")))
      .completion(m2, Set(("foo1", Field, "Int"), ("foo2", Field, "List[Int]")))
  }

  @Test def noCompletionsInsidePatternBind: Unit = {
    code"""object Foo:
          |  (1, 2) match { case (foo, fo${m1}
          """
      .noCompletions()
  }

  @Test def badTypeCompletions: Unit = {
    code"""trait Foo
          |object Test:
          |  def foo: ArrayBuffer[Fo${m1}] = ???
          """
      .completion(m1, Set(("Foo",Class,"Foo")))
  }

  @Test def extensionDefinitionCompletions: Unit =
    code"""|trait Foo
           |object T:
           |  extension (x: Fo$m1)
           |"""
      .completion(m1, Set(("Foo",Class,"Foo")))

  @Test def extensionDefinitionCompletionsSelect: Unit =
    code"""|object Test:
           |  class TestSelect()
           |object T:
           |  extension (x: Test.TestSel$m1)
           |"""
      .completion(m1, Set(("TestSelect", Class, "Test.TestSelect")))

  @Test def extensionDefinitionCompletionsSelectNested: Unit =
    code"""|object Test:
           |  object Test2:
           |    class TestSelect()
           |object T:
           |  extension (x: Test.Test2.TestSel$m1)
           |"""
      .completion(m1, Set(("TestSelect", Class, "Test.Test2.TestSelect")))

  @Test def extensionDefinitionCompletionsSelectInside: Unit =
    code"""|object Test:
           |  object Test2:
           |    class TestSelect()
           |object T:
           |  extension (x: Test.Te$m1.TestSelect)
           |"""
      .completion(m1, Set(("Test2", Module, "Test.Test2")))

  @Test def extensionDefinitionCompletionsTypeParam: Unit =
    code"""|object T:
           |  extension [TypeParam](x: TypePar$m1)
           |"""
      .completion(m1, Set(("TypeParam", Field, "T.TypeParam")))


  @Test def typeParamCompletions: Unit =
    code"""|object T:
           |  def xxx[TTT](x: TT$m1)
           |"""
      .completion(m1, Set(("TTT", Field, "T.TTT")))

  @Test def properTypeVariable: Unit =
    code"""|object M:
           |  List(1,2,3).filterNo$m1
           |"""
      .completion(m1, Set(("filterNot", Method, "(p: Int => Boolean): List[Int]")))

  @Test def properTypeVariableForExtensionMethods: Unit =
    code"""|object M:
           |  extension [T](x: List[T]) def test(aaa: T): T = ???
           |  List(1,2,3).tes$m1
           |
           |"""
      .completion(m1, Set(("test", Method, "(aaa: Int): Int")))

  @Test def properTypeVariableForExtensionMethodsByName: Unit =
    code"""|object M:
           |  extension [T](xs: List[T]) def test(p: T => Boolean): List[T] = ???
           |  List(1,2,3).tes$m1
           |"""
      .completion(m1, Set(("test", Method, "(p: Int => Boolean): List[Int]")))

  @Test def genericExtensionTypeParameterInference: Unit =
    code"""|object M:
           |  extension [T](xs: T) def test(p: T): T = ???
           |  3.tes$m1
           |"""
      .completion(m1, Set(("test", Method, "(p: Int): Int")))

  @Test def genericExtensionTypeParameterInferenceByName: Unit =
    code"""|object M:
           |  extension [T](xs: T) def test(p: T => Boolean): T = ???
           |  3.tes$m1
           |"""
      .completion(m1, Set(("test", Method, "(p: Int => Boolean): Int")))

  @Test def properTypeVariableForImplicitDefs: Unit =
    code"""|object M:
           |  implicit class ListUtils[T](xs: List[T]) {
           |    def test(p: T => Boolean): List[T] = ???
           |  }
           |  List(1,2,3).tes$m1
           |"""
      .completion(m1, Set(("test", Method, "(p: Int => Boolean): List[Int]")))

  @Test def properTypeParameterForImplicitDefs: Unit =
    code"""|object M:
           |  implicit class ListUtils[T](xs: T) {
           |    def test(p: T => Boolean): T = ???
           |  }
           |  new ListUtils(1).tes$m1
           |  1.tes$m2
           |"""
      .completion(m1, Set(("test", Method, "(p: Int => Boolean): Int")))
      .completion(m2, Set(("test", Method, "(p: Int => Boolean): Int")))

  @Test def selectDynamic: Unit =
    code"""|import scala.language.dynamics
           |class Foo extends Dynamic {
           |  def banana: Int = 42
           |  def selectDynamic(field: String): Foo = this
           |  def applyDynamicNamed(name: String)(arg: (String, Int)): Foo = this
           |  def updateDynamic(name: String)(value: Int): Foo = this
           |}
           |object Test:
           |  val x = new Foo()
           |  x.sele$m1
           |  x.bana$m2
           |"""
      .completion(m1, Set(("selectDynamic", Method, "(field: String): Foo")))
      .completion(m2, Set(("banana", Method, "=> Int")))

  @Test def shadowedImport: Unit =
    code"""|
           |import Matches.*
           |object Matches {
           |  val Number = "".r
           |}
           |object Main {
           |  Num$m1
           |}
           |""".completion(m1, Set(
             ("Number", Field, "scala.util.matching.Regex"),
             ("NumberFormatException", Module, "NumberFormatException"),
             ("Numeric", Field, "scala.math.Numeric")
           ))

  @Test def shadowedImportType: Unit =
    code"""|
           |import Matches.*
           |object Matches {
           |  val Number = "".r
           |}
           |object Main {
           |  val x: Num$m1
           |}
           |""".completion(m1, Set(
             ("Number", Class, "Number"),
             ("Number", Field, "scala.util.matching.Regex"),
             ("NumberFormatException", Module, "NumberFormatException"),
             ("NumberFormatException", Field, "NumberFormatException"),
             ("Numeric", Field, "Numeric"),
             ("Numeric", Field, "scala.math.Numeric")
           ))

  @Test def `empty-import-selector`: Unit =
    code"""|import java.$m1
           |"""
      .completion(results => {
        val interestingResults = results.filter(_.getLabel().startsWith("util"))
        assertEquals(1, interestingResults.size)
      })

  @Test def `empty-export-selector`: Unit =
    code"""|export java.$m1
           |"""
    .completion(results => {
      val interestingResults = results.filter(_.getLabel().startsWith("util"))
      assertEquals(1, interestingResults.size)
    })

  @Test def methodsWithInstantiatedTypeVars: Unit =
    code"""|object M:
           |  Map.empty[Int, String].getOrEls$m1
           |"""
     .completion(m1, Set(
       ("getOrElse", Method, "[V1 >: String](key: Int, default: => V1): V1"),
     ))
}

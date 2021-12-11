package dotty.tools.languageserver

import org.junit.Assert.{assertEquals, assertTrue, assertFalse}
import org.junit.Test
import org.eclipse.lsp4j.CompletionItemKind._

import dotty.tools.languageserver.util.Code._
import dotty.tools.languageserver.util.actions.CodeCompletion

class CompletionTest {

  @Test def completion0: Unit = {
    code"class Foo { val xyz: Int = 0; def y: Int = xy${m1} }".withSource
      .completion(m1, Set(("xyz", Field, "Int")))
  }

  @Test def completionFromScalaPredef: Unit = {
    code"class Foo { def foo: Unit = prin${m1} }".withSource
      .completion(m1, Set(
        ("print", Method, "(x: Any): Unit"),
        ("printf", Method, "(text: String, xs: Any*): Unit"),
        ("println", Method, "(x: Any): Unit"),
        ("println", Method, "(): Unit")
      ))
  }

  @Test def completionFromNewScalaPredef: Unit = {
    code"class Foo { val foo = summ${m1} }".withSource
      .completion(m1, Set(("summon", Method, "[T](using x: T): x.type")))
  }

  @Test def completionFromScalaPackage: Unit = {
    code"class Foo { val foo: Conv${m1} }".withSource
      .completion(m1, Set(("Conversion", Class, "scala.Conversion")))
  }

  @Test def completionFromScalaPackageObject: Unit = {
    code"class Foo { val foo: BigD${m1} }".withSource
      .completion(m1, Set(("BigDecimal", Field, "scala.BigDecimal"),
                          ("BigDecimal", Method, "=> math.BigDecimal.type")))
  }

  @Test def completionFromSyntheticPackageObject: Unit = {
    code"class Foo { val foo: IArr${m1} }".withSource
      .completion(m1, Set(("IArray", Module, "IArray"),
                          ("IArray", Field, "scala.IArray")))
  }

  @Test def completionFromJavaDefaults: Unit = {
    code"class Foo { val foo: Runn${m1} }".withSource
      .completion(m1, Set(
        ("Runnable", Class, "java.lang.Runnable"),
        ("Runnable", Module, "Runnable")
      ))
  }

  @Test def completionWithImplicitConversion: Unit = {
    withSources(
      code"object Foo { implicit class WithBaz(bar: Bar) { def baz = 0 } }",
      code"class Bar",
      code"object Main { import Foo._; val bar: Bar = new Bar; bar.b${m1} }"
    ) .completion(m1, Set(("baz", Method, "=> Int")))
  }

  // TODO: Also add tests with concrete classes, where the completion will
  // include the constructor proxy companion

  @Test def importCompleteClassWithPrefix: Unit = {
    withSources(
      code"""object Foo { abstract class MyClass }""",
      code"""import Foo.My${m1}"""
    ).completion(m1, Set(("MyClass", Class, "Foo.MyClass")))
  }

  @Test def importCompleteClassNoPrefix: Unit = {
    withSources(
      code"""object Foo { abstract class MyClass }""",
      code"""import Foo.${m1}"""
    ).completion(m1, completionItems => {
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
      code"""package a
             abstract class MyClass""",
      code"""package b
             import a.My${m1}"""
    ).completion(m1, Set(("MyClass", Class, "a.MyClass")))
  }

  @Test def importCompleteFromClass: Unit = {
    withSources(
      code"""abstract class Foo { val x: Int = 0 }""",
      code"""import Foo.${m1}"""
    ).completion(m1, Set())
  }

  @Test def importCompleteIncludesSynthetic: Unit = {
    code"""case class MyCaseClass(foobar: Int)
           object O {
             val x = MyCaseClass(0)
             import x.c${m1}
           }""".withSource
      .completion(
        m1,
        Set(("copy", Method, "(foobar: Int): MyCaseClass"),
            ("canEqual", Method, "(that: Any): Boolean")))
  }

  @Test def importCompleteIncludeModule: Unit = {
    withSources(
      code"""object O { object MyObject }""",
      code"""import O.My${m1}"""
    ).completion(m1, Set(("MyObject", Module, "O.MyObject")))
  }

  @Test def importCompleteWithClassAndCompanion: Unit = {
    withSources(
      code"""package pkg0
             class Foo
             object Foo""",
      code"""package pgk1
             import pkg0.F${m1}"""
    ).completion(m1, Set(("Foo", Class, "pkg0.Foo"),
                         ("Foo", Module, "pkg0.Foo")))
  }

  @Test def importCompleteIncludePackage: Unit = {
    withSources(
      code"""package foo.bar
             abstract classFizz""",
      code"""import foo.b${m1}"""
    ).completion(m1, Set(("bar", Module, "foo.bar")))
  }

  @Test def importCompleteIncludeMembers: Unit = {
    withSources(
      code"""object MyObject {
               val myVal = 0
               def myDef = 0
               var myVar = 0
               object myObject
               abstract class myClass
               trait myTrait
             }""",
      code"""import MyObject.my${m1}"""
    ).completion(m1, Set(("myVal", Field, "Int"),
                         ("myDef", Method, "=> Int"),
                         ("myVar", Variable, "Int"),
                         ("myObject", Module, "MyObject.myObject"),
                         ("myClass", Class, "MyObject.myClass"),
                         ("myTrait", Class, "MyObject.myTrait")))
  }

  @Test def importJavaClass: Unit = {
    code"""import java.io.FileDesc${m1}""".withSource
      .completion(m1, Set(("FileDescriptor", Class, "java.io.FileDescriptor"),
                          ("FileDescriptor", Module, "java.io.FileDescriptor")))
  }

  @Test def importJavaStaticMethod: Unit = {
    code"""import java.lang.System.lineSep${m1}""".withSource
      .completion(m1, Set(("lineSeparator", Method, "(): String")))
  }

  @Test def importJavaStaticField: Unit = {
    code"""import java.lang.System.ou${m1}""".withSource
      .completion(m1, Set(("out", Field, "java.io.PrintStream")))
  }

  @Test def importFromExplicitAndSyntheticPackageObject: Unit = {
    withSources(
      code"package foo.bar; trait XXXX1",
      code"package foo; package object bar { trait XXXX2 }",
      code"object Main { import foo.bar.XX${m1} }"
    ) .completion(m1, Set(("XXXX1", Class, "foo.bar.XXXX1"), ("XXXX2", Class, "foo.bar.XXXX2")))
  }

  @Test def completeJavaModuleClass: Unit = {
    code"""object O {
             val out = java.io.FileDesc${m1}
           }""".withSource
      .completion(m1, Set(("FileDescriptor", Module, "java.io.FileDescriptor")))
  }

  @Test def importRename: Unit = {
    code"""import java.io.{FileDesc${m1} => Foo}""".withSource
      .completion(m1, Set(("FileDescriptor", Class, "java.io.FileDescriptor"),
                          ("FileDescriptor", Module, "java.io.FileDescriptor")))
  }

  @Test def importGivenByType: Unit = {
    code"""trait Foo
           object Bar
           import Bar.{given Fo$m1}""".withSource
      .completion(m1, Set(("Foo", Class, "Foo")))
  }

  @Test def markDeprecatedSymbols: Unit = {
    code"""object Foo {
             @deprecated
             val bar = 0
           }
           import Foo.ba${m1}""".withSource
      .completion(m1, results => {
        assertEquals(1, results.size)
        val result = results.head
        assertEquals("bar", result.getLabel)
        assertTrue("bar was not deprecated", result.getDeprecated)
      })
  }

  @Test def i4397: Unit = {
    code"""class Foo {
          |  .${m1}
          |}""".withSource
      .completion(m1, Set())
  }

  @Test def completeNoPrefix: Unit = {
    code"""class Foo { def foo = 0 }
          |object Bar {
          |  val foo = new Foo
          |  foo.${m1}
          |}""".withSource
      .completion(m1, results => assertTrue(results.nonEmpty))
  }

  @Test def completeErrorKnowsKind: Unit = {
    code"""object Bar {
          |  abstract class Zig
          |  val Zag: Int = 0
          |  val b = 3 + Bar.${m1}
          |}""".withSource
      .completion(m1, completionItems => {
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
          |}""".withSource
      .completion(m1, Set(("bar", Field, "Bar"), ("bat", Module, "Foo.bat")))
  }

  @Test def completionOnRenamedImport: Unit = {
    code"""import java.io.{FileDescriptor => AwesomeStuff}
           trait Foo { val x: Awesom$m1 }""".withSource
      .completion(m1, Set(("AwesomeStuff", Class, "java.io.FileDescriptor"),
                          ("AwesomeStuff", Module, "java.io.FileDescriptor")))
  }

  @Test def completionOnRenamedImport2: Unit = {
    code"""import java.util.{HashMap => MyImportedSymbol}
           trait Foo {
             import java.io.{FileDescriptor => MyImportedSymbol}
             val x: MyImp$m1
           }""".withSource
      .completion(m1, Set(("MyImportedSymbol", Class, "java.io.FileDescriptor"),
                          ("MyImportedSymbol", Module, "java.io.FileDescriptor")))
  }

  @Test def completionRenamedAndOriginalNames: Unit = {
    code"""import java.util.HashMap
          |trait Foo {
          |  import java.util.{HashMap => HashMap2}
          |  val x: Hash$m1
          |}""".withSource
      .completion(m1, Set(("HashMap", Class, "java.util.HashMap"),
                          ("HashMap", Module, "java.util.HashMap"),
                          ("HashMap2", Class, "java.util.HashMap"),
                          ("HashMap2", Module, "java.util.HashMap")))
  }

  @Test def completionRenamedThrice: Unit = {
    code"""import java.util.{HashMap => MyHashMap}
          |import java.util.{HashMap => MyHashMap2}
          |trait Foo {
          |  import java.util.{HashMap => MyHashMap3}
          |  val x: MyHash$m1
          |}""".withSource
      .completion(m1, Set(("MyHashMap", Class, "java.util.HashMap"),
                          ("MyHashMap", Module, "java.util.HashMap"),
                          ("MyHashMap2", Class, "java.util.HashMap"),
                          ("MyHashMap2", Module, "java.util.HashMap"),
                          ("MyHashMap3", Class, "java.util.HashMap"),
                          ("MyHashMap3", Module, "java.util.HashMap")))
  }

  @Test def completeFromWildcardImports: Unit = {
    code"""object Foo {
          |  val fooFloat: Float = 1.0
          |  val fooLong: Long = 0L
          |  given fooInt: Int = 0
          |  given fooString: String = ""
          |}
          |object Test1 { import Foo.{fooFloat => _, _}; foo$m1 }
          |object Test2 { import Foo.given; foo$m2 }
          |object Test3 { import Foo.{given String}; foo$m3 }
          |object Test4 { import Foo.{_, given String}; foo$m4 }
          |object Test5 { import Foo.{fooFloat, given}; foo$m5 }
          |object Test6 { import Foo.{fooInt => _, fooString => fooStr, given}; foo$m6 }
          |object Test7 { import Foo.{fooLong => fooInt, given Int}; foo$m7 }
          """.withSource
      .completion(m1, Set(("fooLong", Field, "Long")))
      .completion(m2, Set(("fooInt", Field, "Int"),
                          ("fooString", Field, "String")))
      .completion(m3, Set(("fooString", Field, "String")))
      .completion(m4, Set(("fooLong", Field, "Long"),
                          ("fooFloat", Field, "Float"),
                          ("fooString", Field, "String")))
      .completion(m5, Set(("fooFloat", Field, "Float"),
                          ("fooInt", Field, "Int"),
                          ("fooString", Field, "String")))
      .completion(m6, Set(("fooStr", Field, "String")))
      .completion(m7, Set(("fooInt", Field, "Long")))
  }

  @Test def dontCompleteFromAmbiguousImportsFromSameSite: Unit = {
    code"""object Foo {
          |  val i = 0
          |  val j = 1
          |}
          |object Test {
          |  import Foo.{i => xxxx, j => xxxx}
          |  val x = xx$m1
          |}""".withSource
      .completion(m1, Set())
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
          |      val x = xx$m1
          |    }
          |  }
          |}""".withSource
      .completion(m1, Set(("xxxx1", Field, "Int"), ("xxxx2", Field, "Int"), ("xxxx3", Field, "Int")))
  }

  @Test def completeEnclosingObject: Unit = {
    code"""object Test {
          |  def x = Tes$m1
          |}""".withSource
      .completion(m1, Set(("Test", Module, "Test")))
  }

  @Test def completeBothDefinitionsForEqualNestingLevels: Unit = {
    code"""trait Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |trait Bar {
          |  def xxxx(s: String): String = s
          |}
          |object Test extends Foo, Bar {
          |  val x = xx$m1
          |}""".withSource
      .completion(m1, Set(("xxxx", Method, "(s: String): String"),
                          ("xxxx", Method, "(i: Int): Int")))
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
          |  val x = xx$m1
          |}""".withSource
      .completion(m1, Set())
  }

  @Test def completeFromSameImportsForEqualNestingLevels: Unit = {
    code"""object Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |object Test {
          |  import Foo.xxxx
          |  import Foo.xxxx
          |  import Foo.xxxx
          |  val x = xx$m1
          |}""".withSource
      .completion(m1, Set(("xxxx", Method, "(i: Int): Int")))
  }

  @Test def preferLocalDefinitionToImportForEqualNestingLevels: Unit = {
    code"""object Foo {
          |  val xxxx = 1
          |}
          |object Test {
          |  def xxxx(s: String): String = s
          |  import Foo.xxxx
          |  val x = xx$m1
          |}""".withSource
      .completion(m1, Set(("xxxx", Method, "(s: String): String")))
  }

  @Test def preferMoreDeeplyNestedDefinition: Unit = {
    code"""object Test {
          |  def xxxx(i: Int): Int = i
          |  object Inner {
          |    def xxxx(s: String): String = s
          |    val x = xx$m1
          |  }
          |}""".withSource
      .completion(m1, Set(("xxxx", Method, "(s: String): String")))
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
          |    val x: String = xx$m1
          |  }
          |}""".withSource
      .completion(m1, Set(("xxxx", Method, "(s: String): String")))
  }

  @Test def preferMoreDeeplyNestedLocalDefinitionToImport: Unit = {
    code"""object Foo {
          |  def xxxx(i: Int): Int = i
          |}
          |object Test {
          |  import Foo.xxxx
          |  object Inner {
          |    def xxxx(s: String): String = s
          |    val x: String = xx$m1
          |  }
          |}""".withSource
      .completion(m1, Set(("xxxx", Method, "(s: String): String")))
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
          |      val y = xx$m1
          |    }
          |  }
          |}""".withSource
      .completion(m1, Set())
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
          |        val y = xx$m1
          |      }
          |    }
          |  }
          |}""".withSource
      .completion(m1, Set(("xxxx", Field, "Char")))
  }

  @Test def completionClassAndMethod: Unit = {
    code"""object Foo {
          |  class bar
          |  def bar(i: Int) = 0
          |}
          |import Foo.b$m1""".withSource
      .completion(m1, Set(("bar", Class, "Foo.bar"),
                          ("bar", Method, "(i: Int): Int")))
  }

  @Test def completionTypeAndLazyValue: Unit = {
    code"""object Foo {
          |  type bar = Int
          |  lazy val bar = 3
          |}
          |import Foo.b$m1""".withSource
      .completion(m1, Set(("bar", Field, "Foo.bar"),
                          ("bar", Field, "Int")))
  }

  @Test def keepTrackOfTermsAndTypesSeparately: Unit = {
    code"""object XXXX {
          |  object YYYY
          |  type YYYY = YYYY.type
          |}
          |object Test {
          |  import XXXX._
          |  val YYYY = Int
          |  val ZZZZ = YY$m1
          |  type ZZZZ = YY$m2
          |}""".withSource
      .completion(m1, Set(("YYYY", Field, "Int")))
      .completion(m2, Set(("YYYY", Field, "XXXX.YYYY"),
                          ("YYYY", Field, "Int")))
  }

  @Test def completeRespectingAccessModifiers: Unit = {
    code"""trait Foo {
          |  def xxxx1 = ""
          |  protected def xxxx2 = ""
          |  private def xxxx3 = ""
          |}
          |object Test1 extends Foo {
          |  xx$m1
          |}
          |object Test2 {
          |  val foo = new Foo {}
          |  foo.xx$m2
          |}""".withSource
      .completion(m1, Set(("xxxx1", Method, "=> String"), ("xxxx2", Method, "=> String")))
      .completion(m2, Set(("xxxx1", Method, "=> String")))
  }

  @Test def completeFromPackageObjectWithInheritance: Unit = {
    code"""package test
          |trait Foo[A] { def xxxx(a: A) = a }
          |package object foo extends Foo[Int] {}
          |object Test {
          |  foo.xx$m1
          |}""".withSource
      .completion(m1, Set(("xxxx", Method, "(a: Int): Int")))
  }

  @Test def completePrimaryConstructorParameter: Unit = {
    code"""class Foo(abc: Int) {
          |  ab$m1
          |  def method1: Int = {
          |    ab$m2
          |    42
          |  }
          |  def method2: Int = {
          |    val smth = ab$m3
          |    42
          |  }
          |}""".withSource
      .completion(m1, Set(("abc", Field, "Int")))
      .completion(m2, Set(("abc", Field, "Int")))
      .completion(m2, Set(("abc", Field, "Int")))
  }

  @Test def completeExtensionReceiver: Unit = {
    code"""extension (string: String) def xxxx = str$m1"""
      .withSource
      .completion(m1, Set(("string", Field, "String")))
  }

  @Test def completeExtensionMethodWithoutParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodWithParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx(i: Int) = i
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "(i: Int): Int")))
  }

  @Test def completeExtensionMethodWithTypeParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx[A]: Int = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "[A] => Int")))
  }

  @Test def completeExtensionMethodWithParameterAndTypeParameter: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx[A](a: A) = a
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "[A](a: A): A")))
  }

  @Test def completeExtensionMethodFromExtensionWithTypeParameter: Unit = {
    code"""extension [A](a: A) def xxxx: A = a
          |object Main { "abc".xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> String")))
  }

  @Test def completeExtensionMethodWithResultTypeDependantOnReceiver: Unit = {
    code"""trait Foo { type Out; def get: Out}
          |object Bar extends Foo { type Out = String; def get: Out = "abc"}
          |extension (foo: Foo) def xxxx: foo.Out = foo.get
          |object Main { Bar.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> String")))
  }

  @Test def completeExtensionMethodFromExtenionWithPrefixUsingSection: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (using Bar, Baz)(foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromExtenionWithMultiplePrefixUsingSections: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (using Bar)(using Baz)(foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def dontCompleteExtensionMethodFromExtenionWithMissingImplicitFromPrefixUsingSection: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Baz with {}
          |extension (using Bar, Baz)(foo: Foo.type) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set())
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
          |object Main { (new Quux).xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> String")))
  }

  @Test def completeExtensionMethodWithResultTypeDependentOnLeadingImplicit: Unit = {
    code"""object Foo
          |trait Bar { type Out; def get: Out }
          |given Bar with { type Out = 123; def get: Out = 123 }
          |extension (using bar: Bar)(foo: Foo.type) def xxxx: bar.Out = bar.get
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> (123 : Int)")))
  }

  @Test def completeExtensionMethodFromExtenionWithPostfixUsingSection: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (foo: Foo.type)(using Bar, Baz) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "(using x$2: Bar, x$3: Baz): Int")))
  }

  @Test def completeExtensionMethodFromExtenionWithMultiplePostfixUsingSections: Unit = {
    code"""object Foo
          |trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension (foo: Foo.type)(using Bar)(using Baz) def xxxx = 1
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "(using x$2: Bar)(using x$3: Baz): Int")))
  }

  @Test def completeExtensionMethodWithTypeParameterFromExtenionWithTypeParametersAndPrefixAndPostfixUsingSections: Unit = {
    code"""trait Bar
          |trait Baz
          |given Bar with {}
          |given Baz with {}
          |extension [A](using bar: Bar)(a: A)(using baz: Baz) def xxxx[B]: Either[A, B] = Left(a)
          |object Main { 123.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "(using baz: Baz): [B] => Either[Int, B]")))
  }

  @Test def completeExtensionMethodWithTypeBounds: Unit = {
    code"""trait Foo
          |trait Bar extends Foo
          |given Bar with {}
          |extension [A >: Bar](a: A) def xxxx[B <: a.type]: Either[A, B] = Left(a)
          |val foo = new Foo {}
          |object Main { foo.xx${m1} }""".withSource
          .completion(m1, Set(("xxxx", Method, "[B <: (foo : Foo)] => Either[Foo, B]")))
  }

  @Test def completeInheritedExtensionMethod: Unit = {
    code"""object Foo
          |trait FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main extends FooOps { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodWithoutLosingTypeParametersFromGivenInstance: Unit = {
    code"""trait ListOps[A] {
          |  extension (xs: List[A]) def xxxx = xs
          |}
          |given ListOps[Int] with {}
          |object Main { List(1, 2, 3).xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> List[Int]")))
  }

  @Test def completeRenamedExtensionMethod: Unit = {
    code"""object Foo
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |import FooOps.{xxxx => yyyy}
          |object Main { Foo.yy${m1} }""".withSource
      .completion(m1, Set(("yyyy", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromGivenInstanceDefinedInScope: Unit = {
    code"""object Foo
          |trait FooOps
          |given FooOps with {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
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
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromImplicitScope: Unit = {
    code"""case class Foo(i: Int)
          |object Foo {
          |  extension (foo: Foo) def xxxx = foo.i
          |}
          |object Main { Foo(123).xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodFromGivenInImplicitScope: Unit = {
    code"""trait Bar
          |case class Foo(i: Int)
          |object Foo {
          |  given Bar with {
          |    extension (foo: Foo) def xxxx = foo.i
          |  }
          |}
          |object Main { Foo(123).xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def completeExtensionMethodOnResultOfImplicitConversion: Unit = {
    code"""import scala.language.implicitConversions
          |case class Foo(i: Int)
          |extension (foo: Foo) def xxxx = foo.i
          |given Conversion[Int, Foo] = Foo(_)
          |object Main { 123.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def dontCompleteExtensionMethodWithMismatchedName: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object Main { Foo.yy${m1} }""".withSource
      .completion(m1, Set())
  }

  @Test def preferNormalMethodToExtensionMethod: Unit = {
    code"""object Foo {
          |  def xxxx = "abcd"
          |}
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = 1
          |}
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> String")))
  }

  @Test def preferExtensionMethodFromExplicitScope: Unit = {
    code"""object Foo
          |extension (foo: Foo.type) def xxxx = 1
          |object FooOps {
          |  extension (foo: Foo.type) def xxxx = "abcd"
          |}
          |object Main { Foo.xx${m1} }""".withSource
      .completion(m1, Set(("xxxx", Method, "=> Int")))
  }

  @Test def dontCompleteExtensionMethodWithMismatchedReceiverType: Unit = {
    code"""extension (i: Int) def xxxx = i
          |object Main { "abc".xx${m1} }""".withSource
      .completion(m1, Set())
  }

  @Test def i13365: Unit = {
    code"""|import scala.quoted._
        |
        |object Test {
        |  def test(using Quotes)(str: String) = {
        |    import quotes.reflect._
        |    val msg = Expr(str)
        |    val printHello = '{ print("sdsd") }
        |    val tree = printHello.asTerm
        |    tree.sh${m1}
        |  }
        |}""".withSource
      .completion(m1, Set(("show",Method, "(using x$2: x$1.reflect.Printer[x$1.reflect.Tree]): String")))
  }

  @Test def syntheticThis: Unit = {
    code"""|class Y() {
           |  def bar: Unit =
           |    val argument: Int = ???
           |    arg${m1}
           |
           |  def arg: String = ???
           |}
           |""".withSource
      .completion(m1, Set(("arg", Method, "=> String"),
                          ("argument", Field, "Int")))
  }

  @Test def concatMethodWithImplicits: Unit = {
    code"""|object A {
           |  Array.concat${m1}
           |}""".withSource
      .completion(
          m1,
          Set(
            (
              "concat",
              Method,
              "[T](xss: Array[T]*)(implicit evidence$11: scala.reflect.ClassTag[T]): Array[T]"
            )
          )
        )
  }

  @Test def i12465_hkt: Unit =
    code"""???.asInstanceOf[scala.collection.Seq].${m1}""".withSource
      .completion(m1, Set())

  @Test def i12465_hkt_alias: Unit =
    code"""???.asInstanceOf[Seq].${m1}""".withSource
      .completion(m1, Set())

  @Test def i13624_annotType: Unit =
    code"""|object Foo{
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
           |""".withSource
      .completion(
        m1,
        Set(
          ("MyAnnotation", Class, "MyAnnotation"),
          ("MyAnnotation", Module, "MyAnnotation")
        )
      ).completion(
        m2,
        Set(
          ("MyAnnotation", Class, "MyAnnotation"),
          ("MyAnnotation", Module, "MyAnnotation")
        )
      ).completion(
        m3,
        Set(
          ("MyAnnotation", Class, "Foo.MyAnnotation"),
          ("MyAnnotation", Module, "Foo.MyAnnotation")
        )
      ).completion(
        m4,
        Set(
          ("MyAnnotation", Class, "MyAnnotation"),
          ("MyAnnotation", Module, "MyAnnotation")
        )
      ).completion(
        m5,
        Set(
          ("MyAnnotation", Class, "Foo.MyAnnotation"),
          ("MyAnnotation", Module, "Foo.MyAnnotation")
        )
      ).completion(
        m6,
        Set(
          ("MyAnnotation", Class, "Foo.MyAnnotation"),
          ("MyAnnotation", Module, "Foo.MyAnnotation")
        )
      )

  @Test def i13624_annotation : Unit =
    code"""@annotation.implicitNot${m1}
          |@annotation.implicitNotFound @mai${m2}"""
          .withSource
          .completion(m1,
            Set(
              ("implicitNotFound", Class, "scala.annotation.implicitNotFound"),
              ("implicitNotFound", Module, "scala.annotation.implicitNotFound")
            )
          )
          .completion(m2,
            Set(
              ("main", Class, "scala.main"),
              ("main", Module, "main")
            )
          )

  @Test def singleConversionInstancePerMethod : Unit =
    code"""val a = Array(1,2).fil${m1}"""
          .withSource
          .completion(m1,
            Set(
              ("filterNot", Method, "(p: Int => Boolean): Array[Int]"),
              ("filter", Method, "(p: Int => Boolean): Array[Int]")
            )
          )
}

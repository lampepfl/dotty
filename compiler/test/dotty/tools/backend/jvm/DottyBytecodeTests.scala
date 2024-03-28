package dotty.tools.backend.jvm

import scala.language.unsafeNulls

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm
import asm._
import asm.tree._

import scala.tools.asm.Opcodes
import scala.jdk.CollectionConverters._
import Opcodes._

class DottyBytecodeTests extends DottyBytecodeTest {
  import ASMConverters._
  @Test def nullChecks = {
    val source = """
                 |class Foo {
                 |  def foo(x: AnyRef): Int = {
                 |    val bool = x == null
                 |    if (x != null) 1
                 |    else 0
                 |  }
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val methodNode = getMethod(clsNode, "foo")
      correctNumberOfNullChecks(2, methodNode.instructions)
    }
  }

  @Test def byNameParameters = {
    val source = """
                   |class Foo {
                   |  def byNameParam(str: => String): Unit = {}
                   |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val methodNode: MethodNode = getMethod(clsNode, "byNameParam")

      assert(methodNode.signature == "(Lscala/Function0<Ljava/lang/String;>;)V")
    }
  }

  /** This test verifies that simple matches are transformed if possible
   *  despite no annotation
   */
  @Test def basicTransformNonAnnotated = {
    val source = """
                 |object Foo {
                 |  def foo(i: Int) = i match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |    case 0 => println(0)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  /** This test verifies that simple matches with `@switch` annotations are
   *  indeed transformed to a switch
   */
  @Test def basicSwitch = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Int) = (i: @switch) match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |    case 0 => println(0)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def switchWithAlternatives = {
    val source =
      """
        |object Foo {
        |  import scala.annotation.switch
        |  def foo(i: Int) = (i: @switch) match {
        |    case 2 => println(2)
        |    case 1 | 3 | 5 => println(1)
        |    case 0 => println(0)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def switchWithGuards = {
    val source =
      """
        |object Foo {
        |  import scala.annotation.switch
        |  def foo(i: Int, b: Boolean) = (i: @switch) match {
        |    case 2 => println(3)
        |    case 1 if b => println(2)
        |    case 1 => println(1)
        |    case 0 => println(0)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def switchOnStrings = {
    val source =
      """
        |object Foo {
        |  import scala.annotation.switch
        |  def foo(s: String) = s match {
        |    case "AaAa" => println(3)
        |    case "BBBB" | "c" => println(2)
        |    case "D" | "E" => println(1)
        |    case _ => println(0)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def matchWithDefaultNoThrowMatchError = {
    val source =
      """class Test {
        |  def test(s: String) = s match {
        |    case "Hello" => 1
        |    case _       => 2
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn = dir.lookupName("Test.class", directory = false)
      val clsNode = loadClassNode(clsIn.input)
      val method = getMethod(clsNode, "test")
      val throwMatchError = instructionsFromMethod(method).exists {
        case Op(Opcodes.ATHROW) => true
        case _ => false
      }
      assertFalse(throwMatchError)
    }
  }

  @Test def failTransform = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Any) = (i: @switch) match {
                 |    case x: String => println("string!")
                 |    case x :: xs   => println("list!")
                 |  }
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")

      assert(verifySwitch(methodNode, shouldFail = true))
    }
  }

  /** Make sure that creating multidim arrays reduces to "multinewarray"
   *  instruction
   */
  @Test def multidimArraysFromOfDim = {
    val source = """
                 |object Arr {
                 |  def arr = Array.ofDim[Int](2, 1)
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method     = getMethod(moduleNode, "arr")

      val hadCorrectInstr =
        instructionsFromMethod(method)
        .collect {
          case x @ NewArray(op, _, dims)
            if op == Opcode.multianewarray && dims == 2 => x
        }
        .length > 0

      assert(hadCorrectInstr,
             "Did not contain \"multianewarray\" instruction in:\n" +
             instructionsFromMethod(method).mkString("\n"))
    }
  }

  @Test def arraysFromOfDim = {
    val source = """
                 |object Arr {
                 |  def arr1 = Array.ofDim[Int](2)
                 |  def arr2 = Array.ofDim[Unit](2)
                 |  def arr3 = Array.ofDim[String](2)
                 |  def arr4 = Array.ofDim[Map[String, String]](2)
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val arr1       = getMethod(moduleNode, "arr1")
      val arr2       = getMethod(moduleNode, "arr2")
      val arr3       = getMethod(moduleNode, "arr3")

      val arr1CorrectInstr =
        instructionsFromMethod(arr1)
        .collect {
          case x @ IntOp(op, oprnd)
            if op == Opcode.newarray && oprnd == Opcode.int => x
        }
        .length > 0

      assert(arr1CorrectInstr,
             "Did not contain \"multianewarray\" instruction in:\n" +
             instructionsFromMethod(arr1).mkString("\n"))

      val arr2CorrectInstr =
        instructionsFromMethod(arr2)
        .collect {
          case x @ TypeOp(op, oprnd)
            if op == Opcode.anewarray && oprnd == Opcode.boxedUnit => x
        }
        .length > 0

      assert(arr2CorrectInstr,
             "arr2 bytecode did not contain correct `anewarray` instruction:\n" +
             instructionsFromMethod(arr2)mkString("\n"))

      val arr3CorrectInstr =
        instructionsFromMethod(arr3)
        .collect {
          case x @ TypeOp(op, oprnd)
            if op == Opcode.anewarray && oprnd == Opcode.javaString => x
        }
        .length > 0

      assert(arr3CorrectInstr,
             "arr3 bytecode did not contain correct `anewarray` instruction:\n" +
             instructionsFromMethod(arr3).mkString("\n"))
    }
  }

  @Test def arraysFromDimAndFromNewEqual = {
    val source = """
                 |object Arr {
                 |  def arr1 = Array.ofDim[Int](2)
                 |  def arr2 = new Array[Int](2)
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val arr1       = getMethod(moduleNode, "arr1")
      val arr2       = getMethod(moduleNode, "arr2")

      // First two instructions of `arr1` fetch the static reference to `Array`
      val instructions1 = instructionsFromMethod(arr1).drop(2)
      val instructions2 = instructionsFromMethod(arr2)

      assert(instructions1 == instructions2,
        "Creating arrays using `Array.ofDim[Int](2)` did not equal bytecode for `new Array[Int](2)`\n" +
        diffInstructions(instructions1, instructions2))
    }
  }

  /** Verifies that arrays are not unnecessarily wrapped when passed to Java varargs methods */
  @Test def dontWrapArraysInJavaVarargs = {
    val source =
      """
        |import java.nio.file._
        |class Test {
        |  def test(xs: Array[String]) = {
        |     val p4 = Paths.get("Hello", xs*)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val arrayWrapped = instructionsFromMethod(method).exists {
        case inv: Invoke => inv.name.contains("wrapRefArray")
        case _ => false
      }

      assert(!arrayWrapped, "Arrays should not be wrapped when passed to a Java varargs method\n")
    }
  }

  @Test def efficientTryCases = {
    val source =
      """
        |class Test {
        |  def test =
        |    try print("foo")
        |    catch {
        |      case _: scala.runtime.NonLocalReturnControl[_] => ()
        |    }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val hasInstanceof = instructionsFromMethod(method).exists {
        case TypeOp(Opcodes.INSTANCEOF, _) => true
        case _ => false
      }

      assert(!hasInstanceof, "Try case should not issue INSTANCEOF opcode\n")
    }
  }

  @Test def noBoxingInSyntheticEquals = {
    val source =
      """
        |case class Case(x: Long)
        |class Value(val x: Long) extends AnyVal
      """.stripMargin

    checkBCode(source) { dir =>
      for ((clsName, methodName) <- List(("Case", "equals"), ("Value$", "equals$extension"))) {
        val moduleIn = dir.lookupName(s"$clsName.class", directory = false)
        val moduleNode = loadClassNode(moduleIn.input)
        val equalsMethod = getMethod(moduleNode, methodName)

        val callsEquals = instructionsFromMethod(equalsMethod).exists {
            case i @ Invoke(_, _, "equals", _, _) => true
            case i => false
        }

        assert(!callsEquals, s"equals method should not be called in the definition of $clsName#$methodName\n")
      }
    }
  }

  // See #4430
  @Test def javaBridgesAreNotVisible = {
    val source =
      """
        |class Test {
        |  def test = (new java.lang.StringBuilder()).append(Array[Char](), 0, 0)
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      // We check the method call signature to make sure we don't call a Java bridge
      val clsIn = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val testMethod = getMethod(clsNode, "test")
      val instructions = instructionsFromMethod(testMethod)
      val containsExpectedCall = instructions.exists {
        case Invoke(_, "java/lang/StringBuilder", "append", "([CII)Ljava/lang/StringBuilder;", _) => true
        case _ => false
      }
      assertTrue(containsExpectedCall)
    }
  }

  @Test def partialFunctions = {
    val source =
      """object Foo {
        |  def magic(x: Int) = x
        |  val foo: PartialFunction[Int, Int] = { case x => magic(x) }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      // We test that the anonymous class generated for the partial function
      // holds the method implementations and does not use forwarders
      val clsIn = dir.lookupName("Foo$$anon$1.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val applyOrElse = getMethod(clsNode, "applyOrElse")
      val instructions = instructionsFromMethod(applyOrElse)
      val callMagic = instructions.exists {
        case Invoke(_, _, "magic", _, _) => true
        case _ => false
      }
      assertTrue(callMagic)
    }
  }

  @Test def i4172 = {
    val source =
      """class Test {
        |  inline def foo(first: Int*)(second: String = "") = {}
        |
        |  def test = {
        |    foo(1)()
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val fooInvoke = instructionsFromMethod(method).exists {
        case inv: Invoke => inv.name == "foo"
        case _ => false
      }

      assert(!fooInvoke, "foo should not be called\n")
    }
  }

  @Test def returnThrowInPatternMatch = {
    val source =
      """class Test {
        |  def test(a: Any): Int = {
        |    a match {
        |      case _: Test => ???
        |    }
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val instructions = instructionsFromMethod(method)
      val hasReturn = instructions.exists {
        case Op(Opcodes.RETURN) => true
        case _ => false
      }
      assertFalse(hasReturn)
    }
  }

  /** Test that type lambda applications are properly dealias */
  @Test def i5090 = {
    val source =
      """class Test {
        |  type T[X] = X
        |
        |  def test(i: T[Int]): T[Int] = i
        |  def ref(i: Int): Int = i
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val test    = getMethod(clsNode, "test")
      val ref     = getMethod(clsNode, "ref")

      val testInstructions = instructionsFromMethod(test)
      val refInstructions  = instructionsFromMethod(ref)

      assert(testInstructions == refInstructions,
        "`T[Int]` was not properly dealias" +
        diffInstructions(testInstructions, refInstructions))
    }
  }

  /** Test that the receiver of a call to a method with varargs is not unnecessarily lifted */
  @Test def i5191 = {
    val source =
      """class Test {
        |  def foo(args: String*): String = ""
        |  def self = this
        |
        |  def test = self.foo()
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val liftReceiver = instructionsFromMethod(method).exists {
        case VarOp(Opcodes.ASTORE, _) => true // receiver lifted in local val
        case _ => false
      }
      assertFalse("Receiver of a call to a method with varargs is unnecessarily lifted",
        liftReceiver)
    }
  }

  /** Test that the size of the lazy val initialiazer is under a certain threshold
   *
   *  - Fix to #5340 reduced the size from 39 instructions to 34
   *  - Fix to #505  reduced the size from 34 instructions to 32
   */
  @Test def i5340 = {
    val source =
      """class Test {
        |  def test = {
        |    lazy val x = 1
        |    x
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "x$lzyINIT1$1")
      assertEquals(32, instructionsFromMethod(method).size)
    }
  }

  /** Test that synchronize blocks don't box */
  @Test def i505 = {
    val source =
      """class Test {
        |  def test: Int = synchronized(1)
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val doBox = instructionsFromMethod(method).exists {
        case Invoke(_, _, name, _, _) =>
          name == "boxToInteger" || name == "unboxToInt"
        case _ =>
          false
      }
      assertFalse(doBox)
    }
  }

  /** Test that the size of lazy field accesors is under a certain threshold
   */
  @Test def lazyFields = {
    val sourceUnsafe =
      """import scala.annotation.threadUnsafe
        |
        |class Test {
        |  @threadUnsafe lazy val test = 1
        |}
      """.stripMargin

    val sourceSafe =
      """class Test {
        |  lazy val test = 1
        |}
      """.stripMargin

    checkBCode(sourceUnsafe) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")
      assertEquals(14, instructionsFromMethod(method).size)
    }

    checkBCode(sourceSafe) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")
      assertEquals(23, instructionsFromMethod(method).size)
    }
  }

  /* Test that vals in traits cause appropriate `releaseFence()` calls to be emitted. */

  private def checkReleaseFence(releaseFenceExpected: Boolean, outputClassName: String, source: String): Unit = {
    checkBCode(source) { dir =>
      val clsIn = dir.lookupName(outputClassName, directory = false)
      val clsNode = loadClassNode(clsIn.input)
      val method = getMethod(clsNode, "<init>")

      val hasReleaseFence = instructionsFromMethod(method).exists {
        case Invoke(_, _, "releaseFence", _, _) => true
        case _ => false
      }

      assertEquals(source, releaseFenceExpected, hasReleaseFence)
    }
  }

  @Test def testInsertReleaseFence(): Unit = {
    // An empty trait does not cause a releaseFence.
    checkReleaseFence(false, "Bar.class",
      """trait Foo {
        |}
        |class Bar extends Foo
      """.stripMargin)

    // A val in a class does not cause a releaseFence.
    checkReleaseFence(false, "Bar.class",
      """trait Foo {
        |}
        |class Bar extends Foo {
        |  val x: Int = 5
        |}
      """.stripMargin)

    // A val in a trait causes a releaseFence.
    checkReleaseFence(true, "Bar.class",
      """trait Foo {
        |  val x: Int = 5
        |}
        |class Bar extends Foo
      """.stripMargin)

    // The presence of a var in the trait does not invalidate the need for a releaseFence.
    // Also, indirect mixin.
    checkReleaseFence(true, "Bar.class",
      """trait Parent {
        |  val x: Int = 5
        |  var y: Int = 6
        |}
        |trait Foo extends Parent
        |class Bar extends Foo
      """.stripMargin)

    // The presence of a var in the class does not invalidate the need for a releaseFence.
    checkReleaseFence(true, "Bar.class",
      """trait Foo {
        |  val x: Int = 5
        |}
        |class Bar extends Foo {
        |  var y: Int = 6
        |}
      """.stripMargin)

    // When inheriting trait vals through a superclass, no releaseFence is inserted.
    checkReleaseFence(false, "Bar.class",
      """trait Parent {
        |  val x: Int = 5
        |  var y: Int = 6
        |}
        |class Foo extends Parent // releaseFence in Foo, but not in Bar
        |class Bar extends Foo
      """.stripMargin)

    // Various other stuff that do not cause a releaseFence.
    checkReleaseFence(false, "Bar.class",
      """trait Foo {
        |  var w: Int = 1
        |  final val x = 2
        |  def y: Int = 3
        |  lazy val z: Int = 4
        |
        |  def add(a: Int, b: Int): Int = a + b
        |}
        |class Bar extends Foo
      """.stripMargin)
  }

  /* Test that objects compile to *final* classes. */

  private def checkFinalClass(outputClassName: String, source: String) = {
    checkBCode(source) {
      dir =>
        val moduleIn   = dir.lookupName(outputClassName, directory = false)
        val moduleNode = loadClassNode(moduleIn.input)
        assert((moduleNode.access & Opcodes.ACC_FINAL) != 0)
    }
  }

  @Test def objectsAreFinal =
    checkFinalClass("Foo$.class", "object Foo")

  @Test def objectsInClassAreFinal =
    checkFinalClass("Test$Foo$.class",
      """class Test {
        |  object Foo
        |}
      """.stripMargin)

  @Test def objectsInObjsAreFinal =
    checkFinalClass("Test$Foo$.class",
      """object Test {
        |  object Foo
        |}
      """.stripMargin)

  @Test def objectsInObjDefAreFinal =
    checkFinalClass("Test$Foo$2$.class",
      """
        |object Test {
        |  def bar() = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def objectsInClassDefAreFinal =
    checkFinalClass("Test$Foo$2$.class",
      """
        |class Test {
        |  def bar() = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def objectsInObjValAreFinal =
    checkFinalClass("Test$Foo$2$.class",
      """
        |class Test {
        |  val bar = {
        |    object Foo
        |  }
        |}
      """.stripMargin)

  @Test def i5750 = {
    val source =
      """class Test {
        |  def foo: String = ""
        |  def test(cond: Boolean): Int = {
        |    if (cond) foo
        |    1
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val boxUnit = instructionsFromMethod(method).exists {
        case Field(Opcodes.GETSTATIC, "scala/runtime/BoxedUnit", _, _) =>
          true
        case _ =>
          false
      }
      assertFalse(boxUnit)
    }
  }

  @Test def i3271 = {
    val source =
      """class Test {
        |  def test = {
        |    var x = 0
        |    while(x <= 5) {
        |      println(x)
        |      x += 1
        |    }
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Test.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val method  = getMethod(clsNode, "test")

      val instructions = instructionsFromMethod(method)

      val expected = List(
        Op(Opcodes.ICONST_0),
        VarOp(Opcodes.ISTORE, 1),
        Label(2),
        FrameEntry(1, List(1), List()),
        VarOp(Opcodes.ILOAD, 1),
        Op(Opcodes.ICONST_5),
        Jump(Opcodes.IF_ICMPGT, Label(13)),
        Field(Opcodes.GETSTATIC, "scala/Predef$", "MODULE$", "Lscala/Predef$;"),
        VarOp(Opcodes.ILOAD, 1),
        Invoke(Opcodes.INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false),
        Invoke(Opcodes.INVOKEVIRTUAL, "scala/Predef$", "println", "(Ljava/lang/Object;)V", false),
        Incr(Opcodes.IINC, 1, 1),
        Jump(Opcodes.GOTO, Label(2)),
        Label(13),
        FrameEntry(3, List(), List()),
        Op(Opcodes.RETURN))

      assert(instructions == expected,
        "`test` was not properly generated\n" + diffInstructions(instructions, expected))
    }
  }

  @Test def i5924b = {
    val source =
      """|import scala.annotation.static
         |trait Base
         |
         |object Base {
         |  @static val x = 10
         |  @static final val y = 10
         |  @static def f: Int = 30
         |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Base.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val f = getMethod(clsNode, "f")
      val x = getField(clsNode, "x")
      val y = getField(clsNode, "y")
      assert((f.access & Opcodes.ACC_STATIC) != 0)
      List(x, y).foreach { node =>
        assert((node.access & Opcodes.ACC_STATIC) != 0)
        assert((node.access & Opcodes.ACC_FINAL) != 0)
      }
    }
  }

  @Test def i5924c = {
    val source =
      """|import scala.annotation.static
         |class Base
         |
         |object Base {
         |  @static val x = 10
         |  @static final val y = 10
         |  @static var a = 10
         |  @static final var b = 10
         |  @static def f: Int = 30
         |}
      """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Base.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val f = getMethod(clsNode, "f")
      val x = getField(clsNode, "x")
      val y = getField(clsNode, "y")
      val a = getField(clsNode, "a")
      val b = getField(clsNode, "b")
      assert((f.access & Opcodes.ACC_STATIC) != 0)
      List(x, y).foreach { node =>
        assert((node.access & Opcodes.ACC_STATIC) != 0)
        assert((node.access & Opcodes.ACC_FINAL) != 0)
      }
      List(a, b).foreach { node =>
        assert((node.access & Opcodes.ACC_STATIC) != 0)
      }
    }
  }

  @Test def stableNames = {
    val sourceA =
      """|class A {
         |  def a1[T: Ordering]: Unit = {}
         |  def a2[T: Ordering]: Unit = {}
         |}
      """.stripMargin
    val sourceB =
      """|class B {
         |  def b1[T: Ordering]: Unit = {}
         |  def b2[T: Ordering]: Unit = {}
         |}
      """.stripMargin

    checkBCode(List(sourceA, sourceB)) { dir =>
      val clsNodeA = loadClassNode(dir.lookupName("A.class", directory = false).input, skipDebugInfo = false)
      val clsNodeB = loadClassNode(dir.lookupName("B.class", directory = false).input, skipDebugInfo = false)
      val a1 = getMethod(clsNodeA, "a1")
      val a2 = getMethod(clsNodeA, "a2")
      val b1 = getMethod(clsNodeB, "b1")
      val b2 = getMethod(clsNodeB, "b2")

      def assertParamName(mn: MethodNode, expectedName: String) = {
        val actualName = mn.localVariables.get(1).name
        assert(actualName == expectedName,
          s"Method ${mn.name} has parameter $actualName but expected $expectedName")
      }

      // Each definition should get the same names since there's no possible clashes.
      assertParamName(a1, "evidence$1")
      assertParamName(a2, "evidence$1")
      assertParamName(b1, "evidence$1")
      assertParamName(b2, "evidence$1")
    }
  }


  @Test // wrong local variable table for methods containing while loops
  def t9179(): Unit = {
    val code =
      """class C {
        |  def t(): Unit = {
        |    var x = ""
        |    while (x != null) {
        |      foo()
        |      x = null
        |    }
        |    bar()
        |  }
        |  def foo(): Unit = ()
        |  def bar(): Unit = ()
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)
      val t = getMethod(c, "t")
      val instructions = instructionsFromMethod(t)
      val isFrameLine = (x: Instruction) => x.isInstanceOf[FrameEntry] || x.isInstanceOf[LineNumber]
      // TODO: The same test in scalac uses different labels because their LineNumberTable isn't the same as ours,
      // this should be investigated.
      assertSameCode(instructions.filterNot(isFrameLine), List(
        Label(0), Ldc(LDC, ""), VarOp(ASTORE, 1),
        Label(5), VarOp(ALOAD, 1), Jump(IFNULL, Label(19)),
        Label(10), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "foo", "()V", false), Label(14), Op(ACONST_NULL), VarOp(ASTORE, 1), Jump(GOTO, Label(5)),
        Label(19), VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C", "bar", "()V", false), Op(RETURN), Label(25)))
      val labels = instructions collect { case l: Label => l }
      val x = convertMethod(t).localVars.find(_.name == "x").get
      assertEquals(x.start, labels(1))
      assertEquals(x.end, labels(5))
    }
  }

  @Test def i14773TailRecReuseParamSlots(): Unit = {
    val source =
      s"""class Foo {
         |  @scala.annotation.tailrec // explicit @tailrec here
         |  final def fact(n: Int, acc: Int): Int =
         |    if n == 0 then acc
         |    else fact(n - 1, acc * n)
         |}
         |
         |class IntList(head: Int, tail: IntList | Null) {
         |  // implicit @tailrec
         |  final def sum(acc: Int): Int =
         |    val t = tail
         |    if t == null then acc + head
         |    else t.sum(acc + head)
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      // The mutable local vars for n and acc reuse the slots of the params n and acc

      val fooClass = loadClassNode(dir.lookupName("Foo.class", directory = false).input)
      val factMeth = getMethod(fooClass, "fact")

      assertSameCode(factMeth, List(
        Label(0),
        VarOp(ILOAD, 1),
        Op(ICONST_0),
        Jump(IF_ICMPNE, Label(7)),
        VarOp(ILOAD, 2),
        Op(IRETURN),
        Label(7),
        VarOp(ILOAD, 1),
        Op(ICONST_1),
        Op(ISUB),
        VarOp(ISTORE, 3),
        VarOp(ILOAD, 2),
        VarOp(ILOAD, 1),
        Op(IMUL),
        VarOp(ISTORE, 4),
        VarOp(ILOAD, 3),
        VarOp(ISTORE, 1),
        VarOp(ILOAD, 4),
        VarOp(ISTORE, 2),
        Jump(GOTO, Label(0)),
      ))

      // The mutable local vars for this and acc reuse the slots of `this` and of the param acc

      val intListClass = loadClassNode(dir.lookupName("IntList.class", directory = false).input)
      val sumMeth = getMethod(intListClass, "sum")

      assertSameCode(sumMeth, List(
        Label(0),
        VarOp(ALOAD, 0),
        Field(GETFIELD, "IntList", "tail", "LIntList;"),
        VarOp(ASTORE, 2),
        VarOp(ALOAD, 2),
        Jump(IFNONNULL, Label(12)),
        VarOp(ILOAD, 1),
        VarOp(ALOAD, 0),
        Field(GETFIELD, "IntList", "head", "I"),
        Op(IADD),
        Op(IRETURN),
        Label(12),
        VarOp(ALOAD, 2),
        VarOp(ASTORE, 3),
        VarOp(ILOAD, 1),
        VarOp(ALOAD, 0),
        Field(GETFIELD, "IntList", "head", "I"),
        Op(IADD),
        VarOp(ISTORE, 4),
        VarOp(ALOAD, 3),
        VarOp(ASTORE, 0),
        VarOp(ILOAD, 4),
        VarOp(ISTORE, 1),
        Jump(GOTO, Label(0)),
      ))
    }
  }

  @Test def patmatControlFlow(): Unit = {
    val source =
      s"""class Foo {
         |  def m1(xs: List[Int]): Int = xs match
         |    case x :: xr => x
         |    case Nil     => 20
         |
         |  def m2(xs: List[Int]): Int = xs match
         |    case (1 | 2) :: xr => 10
         |    case x :: xr => x
         |    case _ => 20
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val fooClass = loadClassNode(dir.lookupName("Foo.class", directory = false).input)

      // ---------------

      val m1Meth = getMethod(fooClass, "m1")

      assertSameCode(m1Meth, List(
        VarOp(ALOAD, 1),
        VarOp(ASTORE, 2),
        VarOp(ALOAD, 2),
        TypeOp(INSTANCEOF, "scala/collection/immutable/$colon$colon"),
        Jump(IFEQ, Label(19)),
        VarOp(ALOAD, 2),
        TypeOp(CHECKCAST, "scala/collection/immutable/$colon$colon"),
        VarOp(ASTORE, 3),
        VarOp(ALOAD, 3),
        Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "next", "()Lscala/collection/immutable/List;", false),
        VarOp(ASTORE, 4),
        VarOp(ALOAD, 3),
        Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;", false),
        Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false),
        VarOp(ISTORE, 5),
        VarOp(ALOAD, 4),
        VarOp(ASTORE, 6),
        VarOp(ILOAD, 5),
        Op(IRETURN),
        Label(19),
        Field(GETSTATIC, "scala/package$", "MODULE$", "Lscala/package$;"),
        Invoke(INVOKEVIRTUAL, "scala/package$", "Nil", "()Lscala/collection/immutable/Nil$;", false),
        VarOp(ALOAD, 2),
        VarOp(ASTORE, 7),
        Op(DUP),
        Jump(IFNONNULL, Label(31)),
        Op(POP),
        VarOp(ALOAD, 7),
        Jump(IFNULL, Label(36)),
        Jump(GOTO, Label(40)),
        Label(31),
        VarOp(ALOAD, 7),
        Invoke(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false),
        Jump(IFEQ, Label(40)),
        Label(36),
        IntOp(BIPUSH, 20),
        Op(IRETURN),
        Label(40),
        TypeOp(NEW, "scala/MatchError"),
        Op(DUP),
        VarOp(ALOAD, 2),
        Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", false),
        Op(ATHROW),
      ))

      // ---------------

      val m2Meth = getMethod(fooClass, "m2")

      assertSameCode(m2Meth, List(
        VarOp(ALOAD, 1),
        VarOp(ASTORE, 2),
        VarOp(ALOAD, 2),
        TypeOp(INSTANCEOF, "scala/collection/immutable/$colon$colon"),
        Jump(IFEQ, Label(42)),
        VarOp(ALOAD, 2),
        TypeOp(CHECKCAST, "scala/collection/immutable/$colon$colon"),
        VarOp(ASTORE, 3),
        VarOp(ALOAD, 3),
        Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "head", "()Ljava/lang/Object;", false),
        Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", "(Ljava/lang/Object;)I", false),
        VarOp(ISTORE, 4),
        VarOp(ALOAD, 3),
        Invoke(INVOKEVIRTUAL, "scala/collection/immutable/$colon$colon", "next", "()Lscala/collection/immutable/List;", false),
        VarOp(ASTORE, 5),
        Op(ICONST_1),
        VarOp(ILOAD, 4),
        Jump(IF_ICMPNE, Label(19)),
        Jump(GOTO, Label(28)),
        Label(19),
        Op(ICONST_2),
        VarOp(ILOAD, 4),
        Jump(IF_ICMPNE, Label(25)),
        Jump(GOTO, Label(28)),
        Label(25),
        Jump(GOTO, Label(34)),
        Label(28),
        VarOp(ALOAD, 5),
        VarOp(ASTORE, 6),
        IntOp(BIPUSH, 10),
        Op(IRETURN),
        Label(34),
        VarOp(ILOAD, 4),
        VarOp(ISTORE, 7),
        VarOp(ALOAD, 5),
        VarOp(ASTORE, 8),
        VarOp(ILOAD, 7),
        Op(IRETURN),
        Label(42),
        IntOp(BIPUSH, 20),
        Op(IRETURN),
      ))
    }
  }

  @Test def switchControlFlow(): Unit = {
    val source =
      s"""import scala.annotation.switch
         |
         |class Foo {
         |  def m1(x: Int): Int = (x: @switch) match
         |    case 1 => 10
         |    case 7 => 20
         |    case 8 => 30
         |    case 9 => 40
         |    case _ => x
         |
         |  def m2(x: Int): Int = (x: @switch) match
         |    case (1 | 2) => 10
         |    case 7       => 20
         |    case 8       => 30
         |    case c if c > 100 => 20
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val fooClass = loadClassNode(dir.lookupName("Foo.class", directory = false).input)

      // ---------------

      val m1Meth = getMethod(fooClass, "m1")

      assertSameCode(m1Meth, List(
        VarOp(ILOAD, 1),
        VarOp(ISTORE, 2),
        VarOp(ILOAD, 2),
        LookupSwitch(LOOKUPSWITCH, Label(20), List(1, 7, 8, 9), List(Label(4), Label(8), Label(12), Label(16))),
        Label(4),
        IntOp(BIPUSH, 10),
        Op(IRETURN),
        Label(8),
        IntOp(BIPUSH, 20),
        Op(IRETURN),
        Label(12),
        IntOp(BIPUSH, 30),
        Op(IRETURN),
        Label(16),
        IntOp(BIPUSH, 40),
        Op(IRETURN),
        Label(20),
        VarOp(ILOAD, 1),
        Op(IRETURN),
      ))

      // ---------------

      val m2Meth = getMethod(fooClass, "m2")

      assertSameCode(m2Meth, List(
        VarOp(ILOAD, 1),
        VarOp(ISTORE, 2),
        VarOp(ILOAD, 2),
        LookupSwitch(LOOKUPSWITCH, Label(16), List(1, 2, 7, 8), List(Label(4), Label(4), Label(8), Label(12))),
        Label(4),
        IntOp(BIPUSH, 10),
        Op(IRETURN),
        Label(8),
        IntOp(BIPUSH, 20),
        Op(IRETURN),
        Label(12),
        IntOp(BIPUSH, 30),
        Op(IRETURN),
        Label(16),
        VarOp(ILOAD, 2),
        VarOp(ISTORE, 3),
        VarOp(ILOAD, 3),
        IntOp(BIPUSH, 100),
        Jump(IF_ICMPLE, Label(25)),
        IntOp(BIPUSH, 20),
        Op(IRETURN),
        Label(25),
        TypeOp(NEW, "scala/MatchError"),
        Op(DUP),
        VarOp(ILOAD, 2),
        Invoke(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", "(I)Ljava/lang/Integer;", false),
        Invoke(INVOKESPECIAL, "scala/MatchError", "<init>", "(Ljava/lang/Object;)V", false),
        Op(ATHROW),
      ))
    }
  }

  @Test def ifThenElseControlFlow(): Unit = {
    /* This is a test case coming from the Scala.js linker, where in Scala 2 we
     * had to introduce a "useless" `return` to make the bytecode size smaller,
     * measurably increasing performance (!).
     * In dotc, with or without the explicit `return`, the generated code is the same.
     */

    val source =
      s"""import java.io.Writer
         |
         |final class SourceMapWriter(out: Writer) {
         |  private val Base64Map =
         |      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
         |      "abcdefghijklmnopqrstuvwxyz" +
         |      "0123456789+/"
         |
         |  private final val VLQBaseShift = 5
         |  private final val VLQBase = 1 << VLQBaseShift
         |  private final val VLQBaseMask = VLQBase - 1
         |  private final val VLQContinuationBit = VLQBase
         |
         |  def entryPoint(value: Int): Unit = writeBase64VLQ(value)
         |
         |  private def writeBase64VLQ(value0: Int): Unit = {
         |    val signExtended = value0 >> 31
         |    val value = (((value0 ^ signExtended) - signExtended) << 1) | (signExtended & 1)
         |    if (value < 26) {
         |      out.write('A' + value) // was `return out...`
         |    } else {
         |      def writeBase64VLQSlowPath(value0: Int): Unit = {
         |        var value = value0
         |        while ({
         |        // do {
         |          var digit = value & VLQBaseMask
         |          value = value >>> VLQBaseShift
         |          if (value != 0)
         |            digit |= VLQContinuationBit
         |          out.write(Base64Map.charAt(digit))
         |        // } while (
         |          value != 0
         |        // )
         |        }) ()
         |      }
         |      writeBase64VLQSlowPath(value)
         |    }
         |  }
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val sourceMapWriterClass = loadClassNode(dir.lookupName("SourceMapWriter.class", directory = false).input)

      // ---------------

      val writeBase64VLQMeth = getMethod(sourceMapWriterClass, "writeBase64VLQ")

      assertSameCode(writeBase64VLQMeth, List(
        VarOp(ILOAD, 1),
        IntOp(BIPUSH, 31),
        Op(ISHR),
        VarOp(ISTORE, 2),
        VarOp(ILOAD, 1),
        VarOp(ILOAD, 2),
        Op(IXOR),
        VarOp(ILOAD, 2),
        Op(ISUB),
        Op(ICONST_1),
        Op(ISHL),
        VarOp(ILOAD, 2),
        Op(ICONST_1),
        Op(IAND),
        Op(IOR),
        VarOp(ISTORE, 3),
        VarOp(ILOAD, 3),
        IntOp(BIPUSH, 26),
        Jump(IF_ICMPGE, Label(26)),
        VarOp(ALOAD, 0),
        Field(GETFIELD, "SourceMapWriter", "out", "Ljava/io/Writer;"),
        IntOp(BIPUSH, 65),
        VarOp(ILOAD, 3),
        Op(IADD),
        Invoke(INVOKEVIRTUAL, "java/io/Writer", "write", "(I)V", false),
        Op(RETURN),
        Label(26),
        VarOp(ALOAD, 0),
        VarOp(ILOAD, 3),
        Invoke(INVOKESPECIAL, "SourceMapWriter", "writeBase64VLQSlowPath$1", "(I)V", false),
        Op(RETURN),
      ))

      // ---------------

      val writeBase64VLQSlowPathMeth = getMethod(sourceMapWriterClass, "writeBase64VLQSlowPath$1")

      assertSameCode(writeBase64VLQSlowPathMeth, List(
        VarOp(ILOAD, 1),
        VarOp(ISTORE, 2),
        Label(2),
        VarOp(ILOAD, 2),
        IntOp(BIPUSH, 31),
        Op(IAND),
        VarOp(ISTORE, 3),
        VarOp(ILOAD, 2),
        Op(ICONST_5),
        Op(IUSHR),
        VarOp(ISTORE, 2),
        VarOp(ILOAD, 2),
        Op(ICONST_0),
        Jump(IF_ICMPEQ, Label(19)),
        VarOp(ILOAD, 3),
        IntOp(BIPUSH, 32),
        Op(IOR),
        VarOp(ISTORE, 3),
        Label(19),
        VarOp(ALOAD, 0),
        Field(GETFIELD, "SourceMapWriter", "out", "Ljava/io/Writer;"),
        Field(GETSTATIC, "scala/Char$", "MODULE$", "Lscala/Char$;"),
        VarOp(ALOAD, 0),
        Field(GETFIELD, "SourceMapWriter", "Base64Map", "Ljava/lang/String;"),
        VarOp(ILOAD, 3),
        Invoke(INVOKEVIRTUAL, "java/lang/String", "charAt", "(I)C", false),
        Invoke(INVOKEVIRTUAL, "scala/Char$", "char2int", "(C)I", false),
        Invoke(INVOKEVIRTUAL, "java/io/Writer", "write", "(I)V", false),
        VarOp(ILOAD, 2),
        Op(ICONST_0),
        Jump(IF_ICMPNE, Label(2)),
        Op(RETURN),
      ))
    }
  }

  @Test
  def getClazz: Unit = {
    val source = """
                 |class Foo {
                 |  def sideEffect(): Int = { println("hi"); 1 }
                 |  def before1: Class[Int] = sideEffect().getClass
                 |  def before2: Class[Int] = sideEffect().getClass[Int]
                 |  def after: Class[Int] = { sideEffect(); classOf[Int] }
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val before1    = instructionsFromMethod(getMethod(clsNode, "before1"))
      val before2    = instructionsFromMethod(getMethod(clsNode, "before2"))
      val after      = instructionsFromMethod(getMethod(clsNode, "after"))

      assert(before1 == after,
        "`before1` was not translated to the same bytecode as `after`\n" +
        diffInstructions(before1, after))
      assert(before2 == after,
        "`before2` was not translated to the same bytecode as `after`\n" +
        diffInstructions(before2, after))
    }
  }

  @Test
  def invocationReceivers(): Unit = {
    import Opcodes.*

    checkBCode(List(invocationReceiversTestCode.definitions("Object"))) { dir =>
      val c1 = loadClassNode(dir.lookupName("C1.class", directory = false).input)
      val c2 = loadClassNode(dir.lookupName("C2.class", directory = false).input)
      assertSameCode(getMethod(c1, "clone"), List(VarOp(ALOAD, 0), Invoke(INVOKESTATIC, "T", "clone$", "(LT;)Ljava/lang/Object;", true), Op(ARETURN)))
      assertInvoke(getMethod(c1, "f1"), "T", "clone")
      assertInvoke(getMethod(c1, "f2"), "T", "clone")
      assertInvoke(getMethod(c1, "f3"), "C1", "clone")
      assertInvoke(getMethod(c2, "f1"), "T", "clone")
      assertInvoke(getMethod(c2, "f2"), "T", "clone")
      assertInvoke(getMethod(c2, "f3"), "C1", "clone")
    }
    checkBCode(List(invocationReceiversTestCode.definitions("String"))) { dir =>
      val c1b = loadClassNode(dir.lookupName("C1.class", directory = false).input)
      val c2b = loadClassNode(dir.lookupName("C2.class", directory = false).input)
      val tb = loadClassNode(dir.lookupName("T.class", directory = false).input)
      val ub = loadClassNode(dir.lookupName("U.class", directory = false).input)

      def ms(c: ClassNode, n: String) = c.methods.asScala.toList.filter(_.name == n)
      assert(ms(tb, "clone").length == 1)
      assert(ms(ub, "clone").isEmpty)
      val List(c1Clone) = ms(c1b, "clone").filter(_.desc == "()Ljava/lang/Object;")
      assert((c1Clone.access | Opcodes.ACC_BRIDGE) != 0)
      assertSameCode(c1Clone, List(VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "C1", "clone", "()Ljava/lang/String;", false), Op(ARETURN)))

      def iv(m: MethodNode) = getInstructions(c1b, "f1").collect({case i: Invoke => i})
      assertSameCode(iv(getMethod(c1b, "f1")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
      assertSameCode(iv(getMethod(c1b, "f2")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
      // invokeinterface T.clone in C1 is OK here because it is not an override of Object.clone (different signature)
      assertSameCode(iv(getMethod(c1b, "f3")), List(Invoke(INVOKEINTERFACE, "T", "clone", "()Ljava/lang/String;", true)))
    }
  }

  @Test
  def invocationReceiversProtected(): Unit = {
    // http://lrytz.github.io/scala-aladdin-bugtracker/displayItem.do%3Fid=455.html / 9954eaf
    // also https://github.com/scala/bug/issues/1430 / 0bea2ab (same but with interfaces)
    val aC =
      """package a;
        |/*package private*/ abstract class A {
        |  public int f() { return 1; }
        |  public int t;
        |}
      """.stripMargin
    val bC =
      """package a;
        |public class B extends A { }
      """.stripMargin
    val iC =
      """package a;
        |/*package private*/ interface I { int f(); }
      """.stripMargin
    val jC =
      """package a;
        |public interface J extends I { }
      """.stripMargin
    val cC =
      """package b
        |class C {
        |  def f1(b: a.B) = b.f
        |  def f2(b: a.B) = { b.t = b.t + 1 }
        |  def f3(j: a.J) = j.f
        |}
      """.stripMargin

    checkBCode(scalaSources = List(cC), javaSources = List(aC, bC, iC, jC)) { dir =>
      val clsIn   = dir.subdirectoryNamed("b").lookupName("C.class", directory = false).input
      val c = loadClassNode(clsIn)

      assertInvoke(getMethod(c, "f1"), "a/B", "f") // receiver needs to be B (A is not accessible in class C, package b)
      assertInvoke(getMethod(c, "f3"), "a/J", "f") // receiver needs to be J
    }
  }

  @Test
  def specialInvocationReceivers(): Unit = {
    val code =
      """class C {
        |  def f1(a: Array[String]) = a.clone()
        |  def f2(a: Array[Int]) = a.hashCode()
        |  def f3(n: Nothing) = n.hashCode()
        |  def f4(n: Null) = n.toString()
        |
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val c = loadClassNode(dir.lookupName("C.class", directory = false).input)

      assertInvoke(getMethod(c, "f1"), "[Ljava/lang/String;", "clone") // array descriptor as receiver
      assertInvoke(getMethod(c, "f2"), "java/lang/Object", "hashCode") // object receiver
      assertInvoke(getMethod(c, "f3"), "java/lang/Object", "hashCode")
      assertInvoke(getMethod(c, "f4"), "java/lang/Object", "toString")
    }
  }

  @Test
  def deprecation(): Unit = {
    val code =
      """@deprecated
        |class Test {
        |  @deprecated
        |  val v = 0
        |
        |  @deprecated
        |  var x = 0
        |
        |  @deprecated("do not use this function!")
        |  def f(): Unit = ()
        |}
      """.stripMargin

    checkBCode(code) { dir =>
      val c = loadClassNode(dir.lookupName("Test.class", directory = false).input)
      assert((c.access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "f").access & Opcodes.ACC_DEPRECATED) != 0)

      assert((getField(c, "v").access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "v").access & Opcodes.ACC_DEPRECATED) != 0)

      assert((getField(c, "x").access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "x").access & Opcodes.ACC_DEPRECATED) != 0)
      assert((getMethod(c, "x_$eq").access & Opcodes.ACC_DEPRECATED) != 0)
    }
  }

  @Test def vcElideAllocations = {
    val source =
      s"""class ApproxState(val bits: Int) extends AnyVal
         |class Foo {
         |  val FreshApprox: ApproxState = new ApproxState(4)
         |  var approx: ApproxState = FreshApprox
         |  def meth1: Boolean = approx == FreshApprox
         |  def meth2: Boolean = (new ApproxState(4): ApproxState) == FreshApprox
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val meth1      = getMethod(clsNode, "meth1")
      val meth2      = getMethod(clsNode, "meth2")
      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      val isFrameLine = (x: Instruction) => x.isInstanceOf[FrameEntry] || x.isInstanceOf[LineNumber]

      // No allocations of ApproxState

      assertSameCode(instructions1.filterNot(isFrameLine), List(
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "Foo", "approx", "()I", false),
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "Foo", "FreshApprox", "()I", false),
        Jump(IF_ICMPNE, Label(7)), Op(ICONST_1),
        Jump(GOTO, Label(10)),
        Label(7), Op(ICONST_0),
        Label(10), Op(IRETURN)))

      assertSameCode(instructions2.filterNot(isFrameLine), List(
        Op(ICONST_4),
        VarOp(ALOAD, 0), Invoke(INVOKEVIRTUAL, "Foo", "FreshApprox", "()I", false),
        Jump(IF_ICMPNE, Label(6)), Op(ICONST_1),
        Jump(GOTO, Label(9)),
        Label(6), Op(ICONST_0),
        Label(9), Op(IRETURN)))
    }
  }

  /** Check that final mutable var accessors are final */
  @Test def i10835 = {
    val source =
      s"""class A {
        |  final var x = 1
        |}
        |""".stripMargin
    checkBCode(source){dir =>
      val clsIn      = dir.lookupName("A.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      def isFinal(access: Int) = (access & Opcodes.ACC_FINAL) != 0

      val field = clsNode.fields.asScala.find(_.name == "x").map(_.access)
      assertTrue("field is not final", field.exists(!isFinal(_)))
      assertTrue("field is private", field.exists(acc => (acc & Opcodes.ACC_PRIVATE) != 0))

      val methods = clsNode.methods.asScala
      def methodAccess(name: String) = methods.find(_.name == name).map(_.access)
      assertTrue("getter is final", methodAccess("x").exists(isFinal))
      assertTrue("setter is final", methodAccess("x_$eq").exists(isFinal))
    }
  }

  /** Check that erasure if `Int | Nothing` is `int` */
  @Test def i14970 = {
    val source =
      s"""class Foo {
         |  def foo: Int | Nothing = 1
         |  def bar: Nothing | Int = 1
         |}
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      def testSig(methodName: String, expectedSignature: String) = {
        val signature = clsNode.methods.asScala.filter(_.name == methodName).map(_.signature)
        assertEquals(List(expectedSignature), signature)
      }
      testSig("foo", "()I")
      testSig("bar", "()I")
    }
  }

  @Test def i15535 = {
    // primary goal of this test is to check that `LineNumber` have correct numbers
    val source =
      """object Main {
        |  def m(x: Int): Unit = {
        |    x match {
        |      case y =>
        |        println(y)
        |        println(y)
        |    }
        |  }
        |}
        """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Main$.class", directory = false).input
      val clsNode = loadClassNode(clsIn, skipDebugInfo = false)
      val method  = getMethod(clsNode, "m")
      val instructions = instructionsFromMethod(method).filter(_.isInstanceOf[LineNumber])

      val expected = List(
        LineNumber(3, Label(0)),
        LineNumber(4, Label(5)), // case y =>
        LineNumber(5, Label(9)),
        LineNumber(6, Label(15)),
      )

      assertSameCode(instructions, expected)
    }
  }

  @Test def i15535_2 = {
    // primary goal of this test is to check that `LineNumber` have correct numbers
    val source =
      """object Main {
        |  def m(x: Matchable): Unit = {
        |    x match {
        |      case a if a == 3 =>
        |        println(a)
        |        println(a)
        |      case b: Int =>
        |        println(b)
        |        println(b)
        |      case c @ Left(l) =>
        |        println(l)
        |        println(c)
        |      case d =>
        |        println(d)
        |        println(d)
        |        println(d)
        |    }
        |  }
        |}
        """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Main$.class", directory = false).input
      val clsNode = loadClassNode(clsIn, skipDebugInfo = false)
      val method  = getMethod(clsNode, "m")
      val instructions = instructionsFromMethod(method).filter(_.isInstanceOf[LineNumber])

      val expected = List(
        LineNumber(3, Label(0)),
        LineNumber(4, Label(5)), // case a if a == 3 =>
        LineNumber(5, Label(15)),
        LineNumber(6, Label(20)),
        LineNumber(7, Label(26)), // case b: Int =>
        LineNumber(8, Label(35)),
        LineNumber(9, Label(41)),
        LineNumber(10, Label(48)), // case c @ Left(l) =>
        LineNumber(11, Label(63)),
        LineNumber(12, Label(68)),
        LineNumber(13, Label(74)), // case d =>
        LineNumber(14, Label(79)),
        LineNumber(15, Label(84)),
        LineNumber(16, Label(89)),
      )

      assertSameCode(instructions, expected)
    }
  }

  @Test def i18320(): Unit = {
    val c1 =
      """class C {
        |  def m: Unit = {
        |    val x = 1
        |  }
        |}
        |""".stripMargin
    checkBCode(c1) {dir =>
      val clsIn = dir.lookupName("C.class", directory = false).input
      val clsNode = loadClassNode(clsIn, skipDebugInfo = false)
      val method = getMethod(clsNode, "m")
      val instructions = instructionsFromMethod(method).filter(_.isInstanceOf[LineNumber])
      val expected = List(LineNumber(3, Label(0)))
      assertSameCode(instructions, expected)

    }
  }

  @Test def i18816 = {
    // The primary goal of this test is to check that `LineNumber` have correct numbers
    val source =
      """trait Context
        |
        |class A(x: Context) extends AnyVal:
        |  given [T]: Context = x
        |
        |  def m1 =
        |    println(m3)
        |    def m2 =
        |      m3 // line 9
        |    println(m2)
        |
        |  def m3(using Context): String = ""
        """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("A$.class", directory = false).input
      val clsNode = loadClassNode(clsIn, skipDebugInfo = false)
      val method  = getMethod(clsNode, "m2$1")
      val instructions = instructionsFromMethod(method).filter(_.isInstanceOf[LineNumber])

      // There used to be references to line 7 here
      val expected = List(
        LineNumber(9, Label(0)),
      )

      assertSameCode(instructions, expected)
    }
  }

  @Test def newInPrefixesOfDefaultParam = {
    val source =
      s"""class A:
         |  def f(x: Int = 1): Int = x
         |
         |class Test:
         | def meth1() = (new A).f()
         | def meth2() = { val a = new A; a.f() }
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val meth1      = getMethod(clsNode, "meth1")
      val meth2      = getMethod(clsNode, "meth2")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "`assert` was not properly inlined in `meth1`\n" +
        diffInstructions(instructions1, instructions2))
    }
  }

  @Test def newInDependentOfDefaultParam = {
    val source =
      s"""class A:
         |  def i: Int = 1
         |
         |class Test:
         |  def f(a: A)(x: Int = a.i): Int = x
         |  def meth1() = f(new A)()
         |  def meth2() = { val a = new A; f(a)() }
         """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Test.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val meth1      = getMethod(clsNode, "meth1")
      val meth2      = getMethod(clsNode, "meth2")

      val instructions1 = instructionsFromMethod(meth1)
      val instructions2 = instructionsFromMethod(meth2)

      assert(instructions1 == instructions2,
        "`assert` was not properly inlined in `meth1`\n" +
        diffInstructions(instructions1, instructions2))
    }
  }


  @Test def i15098 = {
    val source =
      """object Main {
        |  def main(args: Array[String]): Unit = {
        |    Array(1).foreach { n =>
        |      val x = 123
        |      println(n)
        |    }
        |  }
        |}
        """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Main$.class", directory = false).input
      val clsNode = loadClassNode(clsIn, skipDebugInfo = false)
      val method  = getMethod(clsNode, "main")
      val instructions = instructionsFromMethod(method).filter(_.isInstanceOf[LineNumber])

      val expected = List(
        LineNumber(3, Label(0)),
      )

      assertSameCode(instructions, expected)
    }
  }

  @Test def i15098_2 = {
    val source =
      """object Main {
        |  def main(args: Array[String]): Unit = {
        |    Array(1).map { n =>
        |      val x = 123
        |      x + n
        |    }.foreach { n =>
        |      println(n)
        |      println(n)
        |    }
        |  }
        |}
        """.stripMargin

    checkBCode(source) { dir =>
      val clsIn   = dir.lookupName("Main$.class", directory = false).input
      val clsNode = loadClassNode(clsIn, skipDebugInfo = false)
      val method  = getMethod(clsNode, "main")
      val instructions = instructionsFromMethod(method).filter(_.isInstanceOf[LineNumber])

      val expected = List(
        LineNumber(3, Label(0)),
        LineNumber(6, Label(15)),
        LineNumber(3, Label(24)),
        LineNumber(6, Label(27)),
      )

      assertSameCode(instructions, expected)
    }
  }
}

object invocationReceiversTestCode {
  // if cloneType is more specific than Object (e.g., String), a bridge method is generated.
  def definitions(cloneType: String): String =
    s"""trait T { override def clone(): $cloneType = "hi" }
        |trait U extends T
        |class C1 extends U with Cloneable {
        |  // The comments below are true when `cloneType` is Object.
        |  // C1 gets a forwarder for clone that invokes T.clone. this is needed because JVM method
        |  // resolution always prefers class members, so it would resolve to Object.clone, even if
        |  // C1 is a subtype of the interface T which has an overriding default method for clone.
        |
        |  // invokeinterface T.clone
        |  def f1 = (this: T).clone()
        |
        |  // cannot invokeinterface U.clone (NoSuchMethodError). Object.clone would work here, but
        |  // not in the example in C2 (illegal access to protected). T.clone works in all cases and
        |  // resolves correctly.
        |  def f2 = (this: U).clone()
        |
        |  // invokevirtual C1.clone()
        |  def f3 = (this: C1).clone()
        |}
        |
        |class C2 {
        |  def f1(t: T) = t.clone()  // invokeinterface T.clone
        |  def f2(t: U) = t.clone()  // invokeinterface T.clone -- Object.clone would be illegal (protected, explained in C1)
        |  def f3(t: C1) = t.clone() // invokevirtual C1.clone -- Object.clone would be illegal
        |}
    """.stripMargin
}

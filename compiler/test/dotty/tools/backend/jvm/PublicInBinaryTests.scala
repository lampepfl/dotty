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

class PublicInBinaryTests extends DottyBytecodeTest {
  import ASMConverters._

  private def privateOrProtectedOpcode = Opcodes.ACC_PRIVATE | Opcodes.ACC_PROTECTED

  private def checkPublicMethod(classNode: ClassNode, methodName: String, desc: String): Unit =
    val method = getMethod(classNode, methodName)
    assert(method.desc == desc)
    assert((method.access & privateOrProtectedOpcode) == 0)

  private def checkPrivateMethod(classNode: ClassNode, methodName: String, desc: String): Unit =
    val method = getMethod(classNode, methodName)
    assert(method.desc == desc)
    assert((method.access & Opcodes.ACC_PRIVATE) == Opcodes.ACC_PRIVATE)

  private def checkPublicField(classNode: ClassNode, fliedName: String): Unit =
    val method = getField(classNode, fliedName)
    assert((method.access & privateOrProtectedOpcode) == 0)

  private def checkPrivateField(classNode: ClassNode, fliedName: String): Unit =
    val method = getField(classNode, fliedName)
    assert((method.access & Opcodes.ACC_PRIVATE) == Opcodes.ACC_PRIVATE)

  private def checkPublicClass(classNode: ClassNode): Unit =
    assert((classNode.access & privateOrProtectedOpcode) == 0)

  override def initCtx =
    val ctx0 = super.initCtx
    ctx0.setSetting(ctx0.settings.experimental, true)

  @Test
  def publicInBinaryDef(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |class C:
        |  @publicInBinary private[C] def packagePrivateMethod: Int = 1
        |  @publicInBinary protected def protectedMethod: Int = 1
        |  inline def inlined = packagePrivateMethod + protectedMethod
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPublicMethod(cClass, "packagePrivateMethod", "()I")
      checkPublicMethod(cClass, "protectedMethod", "()I")

      // Check that the @publicInBinary annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateMethod", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedMethod", "()I", false),
      ))
    }
  }

  @Test
  def publicInBinaryVal(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |class C:
        |  @publicInBinary private[C] val packagePrivateVal: Int = 1
        |  @publicInBinary protected val protectedVal: Int = 1
        |  @publicInBinary private[C] lazy val lazyPackagePrivateVal: Int = 1
        |  @publicInBinary protected lazy val lazyProtectedVal: Int = 1
        |  inline def inlined = packagePrivateVal + protectedVal + lazyPackagePrivateVal + lazyProtectedVal
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPublicMethod(cClass, "packagePrivateVal", "()I")
      checkPublicMethod(cClass, "protectedVal", "()I")

      checkPublicMethod(cClass, "lazyPackagePrivateVal", "()I")
      checkPublicMethod(cClass, "lazyProtectedVal", "()I")

      // Check that the @publicInBinary annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateVal", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedVal", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "lazyPackagePrivateVal", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "lazyProtectedVal", "()I", false),
      ))
    }
  }

  @Test
  def publicInBinaryVar(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |class C:
        |  @publicInBinary private[C] var packagePrivateVar: Int = 1
        |  @publicInBinary protected var protectedVar: Int = 1
        |  inline def inlined =
        |    packagePrivateVar = 1
        |    protectedVar = 1
        |    packagePrivateVar + protectedVar
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPublicMethod(cClass, "packagePrivateVar", "()I")
      checkPublicMethod(cClass, "packagePrivateVar_$eq", "(I)V")
      checkPublicMethod(cClass, "protectedVar", "()I")
      checkPublicMethod(cClass, "protectedVar_$eq", "(I)V")

      // Check that the @publicInBinary annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateVar_$eq", "(I)V", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedVar_$eq", "(I)V", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateVar", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedVar", "()I", false),
      ))
    }
  }

  @Test
  def publicInBinaryGiven(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |class C:
        |  @publicInBinary private[C] given packagePrivateGiven1: Int = 1
        |  @publicInBinary protected given protectedGiven1: Int = 1
        |  @publicInBinary private[C] given packagePrivateGiven2(using Int): Int = 1
        |  @publicInBinary protected given protectedGiven2(using Int): Int = 1
        |  inline def inlined =
        |    packagePrivateGiven1 + protectedGiven1 + packagePrivateGiven2(using 1) + protectedGiven2(using 1)
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)
      checkPublicMethod(cClass, "packagePrivateGiven1", "()I")
      checkPublicMethod(cClass, "protectedGiven1", "()I")

      checkPublicMethod(cClass, "packagePrivateGiven2", "(I)I")
      checkPublicMethod(cClass, "protectedGiven2", "(I)I")

      // Check that the @publicInBinary annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateGiven1", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedGiven1", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateGiven2", "(I)I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedGiven2", "(I)I", false),
      ))
    }
  }

  @Test
  def publicInBinaryClassParam(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |class C(
        |  @publicInBinary private[C] val packagePrivateVal: Int = 1,
        |  @publicInBinary protected val protectedVal: Int = 1,
        |) {
        |  inline def inlined =
        |    packagePrivateVal + protectedVal
        |  def testInlined = inlined
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val cClass = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)
      checkPublicMethod(cClass, "packagePrivateVal", "()I")
      checkPublicMethod(cClass, "protectedVal", "()I")

      // Check that the @publicInBinary annotated method is called
      val testInlined = getMethod(cClass, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEVIRTUAL, "C", "packagePrivateVal", "()I", false),
        Invoke(INVOKEVIRTUAL, "C", "protectedVal", "()I", false),
      ))
    }
  }

  @Test
  def publicInBinaryObject(): Unit = {
    val code =
      """package foo
        |import scala.annotation.publicInBinary
        |private object PrivateObject
        |@publicInBinary private[foo] object PackagePrivateObject
        |@publicInBinary protected object ProtectedObject
      """.stripMargin
    checkBCode(code) { dir =>
      val privateObject = loadClassNode(dir.subdirectoryNamed("foo").lookupName("PrivateObject$.class", directory = false).input, skipDebugInfo = false)
      checkPublicClass(privateObject)
      checkPublicField(privateObject, "MODULE$")

      val packagePrivateObject = loadClassNode(dir.subdirectoryNamed("foo").lookupName("PackagePrivateObject$.class", directory = false).input, skipDebugInfo = false)
      checkPublicClass(packagePrivateObject)
      checkPublicField(packagePrivateObject, "MODULE$")

      val protectedObject = loadClassNode(dir.subdirectoryNamed("foo").lookupName("ProtectedObject$.class", directory = false).input, skipDebugInfo = false)
      checkPublicClass(protectedObject)
      checkPublicField(protectedObject, "MODULE$")
    }
  }

  @Test
  def publicInBinaryTraitDefs(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |trait C:
        |  @publicInBinary private[C] val packagePrivateVal: Int = 1
        |  @publicInBinary protected val protectedVal: Int = 1
        |  @publicInBinary private[C] lazy val packagePrivateLazyVal: Int = 1
        |  @publicInBinary protected lazy val protectedLazyVal: Int = 1
        |  @publicInBinary private[C] var packagePrivateVar: Int = 1
        |  @publicInBinary protected var protectedVar: Int = 1
        |  @publicInBinary private[C] def packagePrivateDef: Int = 1
        |  @publicInBinary protected def protectedDef: Int = 1
        |  inline def inlined =
        |    packagePrivateVar = 1
        |    protectedVar = 1
        |    packagePrivateVal +
        |    protectedVal +
        |    packagePrivateLazyVal +
        |    protectedLazyVal +
        |    packagePrivateVar +
        |    protectedVar +
        |    packagePrivateDef +
        |    protectedDef
        |  def testInlined = inlined
      """.stripMargin
    checkBCode(code) { dir =>
      val cTrait = loadClassNode(dir.lookupName("C.class", directory = false).input, skipDebugInfo = false)

      checkPublicMethod(cTrait, "packagePrivateVal", "()I")
      checkPublicMethod(cTrait, "protectedVal", "()I")
      checkPublicMethod(cTrait, "packagePrivateLazyVal", "()I")
      checkPublicMethod(cTrait, "protectedLazyVal", "()I")
      checkPublicMethod(cTrait, "packagePrivateVar", "()I")
      checkPublicMethod(cTrait, "packagePrivateVar_$eq", "(I)V")
      checkPublicMethod(cTrait, "protectedVar", "()I")
      checkPublicMethod(cTrait, "protectedVar_$eq", "(I)V")
      checkPublicMethod(cTrait, "packagePrivateDef", "()I")
      checkPublicMethod(cTrait, "protectedDef", "()I")

      // Check that the @publicInBinary annotated method is called
      val testInlined = getMethod(cTrait, "testInlined")
      val testInlinedInstructions = instructionsFromMethod(testInlined).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInlinedInstructions, List(
        Invoke(INVOKEINTERFACE, "C", "packagePrivateVar_$eq", "(I)V", true),
        Invoke(INVOKEINTERFACE, "C", "protectedVar_$eq", "(I)V", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateVal", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedVal", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateLazyVal", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedLazyVal", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateVar", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedVar", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "packagePrivateDef", "()I", true),
        Invoke(INVOKEINTERFACE, "C", "protectedDef", "()I", true)
      ))
    }
  }

  @Test
  def i13215(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |package foo:
        |  trait Bar:
        |    inline def baz = Baz
        |    def testInlined = baz
        |  @publicInBinary private[foo] object Baz
      """.stripMargin
    checkBCode(code) { dir =>
      val barClass = loadClassNode(dir.subdirectoryNamed("foo").lookupName("Bar.class", directory = false).input, skipDebugInfo = false)
      checkPublicMethod(barClass, "testInlined", "()Lfoo/Baz$;")
    }
  }

  @Test
  def i13215b(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |package foo:
        |  trait Bar:
        |    inline def baz = Baz
        |    def testInlined = baz
        |  @publicInBinary private object Baz
      """.stripMargin
    checkBCode(code) { dir =>
      val barClass = loadClassNode(dir.subdirectoryNamed("foo").lookupName("Bar.class", directory = false).input, skipDebugInfo = false)
      checkPublicMethod(barClass, "testInlined", "()Lfoo/Baz$;")
    }
  }

  @Test
  def i15413(): Unit = {
    val code =
      """import scala.quoted.*
        |import scala.annotation.publicInBinary
        |class Macro:
        |  inline def foo = Macro.fooImpl
        |  def test = foo
        |object Macro:
        |  @publicInBinary private[Macro] def fooImpl = {}
      """.stripMargin
    checkBCode(code) { dir =>
      val macroClass = loadClassNode(dir.lookupName("Macro.class", directory = false).input, skipDebugInfo = false)
      val testMethod = getMethod(macroClass, "test")
      val testInstructions = instructionsFromMethod(testMethod).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInstructions, List(
        Invoke(INVOKEVIRTUAL, "Macro$", "fooImpl", "()V", false)))
    }
  }

  @Test
  def i15413b(): Unit = {
    val code =
      """package foo
        |import scala.annotation.publicInBinary
        |class C:
        |  inline def baz = D.bazImpl
        |  def test = baz
        |object D:
        |  @publicInBinary private[foo] def bazImpl = {}
      """.stripMargin
    checkBCode(code) { dir =>
      val barClass = loadClassNode(dir.subdirectoryNamed("foo").lookupName("C.class", directory = false).input, skipDebugInfo = false)
      val testMethod = getMethod(barClass, "test")
      val testInstructions = instructionsFromMethod(testMethod).filter(_.isInstanceOf[Invoke])
      assertSameCode(testInstructions, List(
        Invoke(INVOKEVIRTUAL, "foo/D$", "bazImpl", "()V", false)))
    }
  }

  @Test
  def noProtectedAccessorsForBinaryInPublic(): Unit = {
    val code =
      """import scala.annotation.publicInBinary
        |package p {
        |  class A {
        |    protected def a(): Int = 1
        |    @publicInBinary protected def b(): Int = 1
        |  }
        |}
        |package q {
        |  class B extends p.A {
        |    trait BInner {
        |      def test1() = a() // protected accessor for `a`
        |      def test2() = b() // no protected accessor for `b`
        |    }
        |  }
        |}
      """.stripMargin
    checkBCode(code) { dir =>
      val bClass = loadClassNode(dir.subdirectoryNamed("q").lookupName("B.class", directory = false).input, skipDebugInfo = false)
      assert(bClass.methods.asScala.exists(_.name == "protected$a"))
      assert(bClass.methods.asScala.forall(_.name != "protected$b"))

      val bInnerClass = loadClassNode(dir.subdirectoryNamed("q").lookupName("B$BInner.class", directory = false).input, skipDebugInfo = false)

      val test1Method = getMethod(bInnerClass, "test1")
      val test1Instructions = instructionsFromMethod(test1Method).filter(_.isInstanceOf[Invoke])
      assertSameCode(test1Instructions, List(
        Invoke(INVOKEINTERFACE, "q/B$BInner", "q$B$BInner$$$outer", "()Lq/B;", true),
        Invoke(INVOKEVIRTUAL, "q/B", "protected$a", "()I", false)))

      val test2Method = getMethod(bInnerClass, "test2")
      val test2Instructions = instructionsFromMethod(test2Method).filter(_.isInstanceOf[Invoke])
      assertSameCode(test2Instructions, List(
        Invoke(INVOKEINTERFACE, "q/B$BInner", "q$B$BInner$$$outer", "()Lq/B;", true),
        Invoke(INVOKEVIRTUAL, "q/B", "b", "()I", false) ))
    }
  }
}

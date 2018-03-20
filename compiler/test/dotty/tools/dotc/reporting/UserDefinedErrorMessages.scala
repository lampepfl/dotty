package dotty.tools
package dotc
package reporting

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.diagnostic.messages._
import org.junit.Assert._
import org.junit.Test

class UserDefinedErrorMessages extends ErrorMessagesTest {
  @Test def userDefinedImplicitAmbiguous1 =
    checkMessagesAfter("frontend") {
      """
        |object Test {
        |  trait =!=[C, D]
        |
        |  implicit def neq[E, F] : E =!= F = null
        |
        |  @annotation.implicitAmbiguous("Could not prove ${J} =!= ${J}")
        |  implicit def neqAmbig1[G, H, J] : J =!= J = null
        |  implicit def neqAmbig2[I] : I =!= I = null
        |
        |  implicitly[Int =!= Int]
        |}

      """.stripMargin
    }.expect { (itcx, messages) =>
      import diagnostic.NoExplanation
      implicit val ctx: Context = itcx

      assertMessageCount(1, messages)
      val (m: NoExplanation) :: Nil = messages

      assertEquals(m.msg, "Could not prove Int =!= Int")
    }

  @Test def userDefinedImplicitAmbiguous2 =
    checkMessagesAfter("frontend") {
      """
        |object Test {
        |  trait =!=[C, D]
        |
        |  implicit def neq[E, F] : E =!= F = null
        |
        |  implicit def neqAmbig1[G, H, J] : J =!= J = null
        |  @annotation.implicitAmbiguous("Could not prove ${I} =!= ${I}")
        |  implicit def neqAmbig2[I] : I =!= I = null
        |
        |  implicitly[Int =!= Int]
        |}

      """.stripMargin
    }.expect { (itcx, messages) =>
      import diagnostic.NoExplanation
      implicit val ctx: Context = itcx

      assertMessageCount(1, messages)
      val (m: NoExplanation) :: Nil = messages

      assertEquals(m.msg, "Could not prove Int =!= Int")
    }

  @Test def userDefinedImplicitAmbiguous3 =
    checkMessagesAfter("frontend") {
      """
        |object Test {
        |  trait =!=[C, D]
        |
        |  implicit def neq[E, F] : E =!= F = null
        |
        |  @annotation.implicitAmbiguous("Could not prove ${J} =!= ${J}")
        |  implicit def neqAmbig1[G, H, J] : J =!= J = null
        |  @annotation.implicitAmbiguous("Could not prove ${I} =!= ${I}")
        |  implicit def neqAmbig2[I] : I =!= I = null
        |
        |  implicitly[Int =!= Int]
        |}

      """.stripMargin
    }.expect { (itcx, messages) =>
      import diagnostic.NoExplanation
      implicit val ctx: Context = itcx

      assertMessageCount(1, messages)
      val (m: NoExplanation) :: Nil = messages

      assertEquals(m.msg, "Could not prove Int =!= Int")
    }

  @Test def userDefinedImplicitAmbiguous4 =
    checkMessagesAfter("frontend") {
      """
        |class C {
        |  @annotation.implicitAmbiguous("msg A=${A}")
        |  implicit def f[A](x: Int): String = "f was here"
        |  implicit def g(x: Int): String = "f was here"
        |  def test: Unit = {
        |    implicitly[Int => String]
        |  }
        |}

      """.stripMargin
    }.expect { (itcx, messages) =>
      import diagnostic.NoExplanation
      implicit val ctx: Context = itcx

      assertMessageCount(1, messages)
      val (m: NoExplanation) :: Nil = messages

      assertEquals(m.msg, "msg A=Any")
    }
}

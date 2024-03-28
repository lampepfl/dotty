import scala.language.experimental.clauseInterleaving

object chainedParams{

  trait Chain{
      type Tail <: Chain
  }

  def f[C1 <: Chain](c1: C1)[C2 <: c1.Tail](c2: C2)[C3 <: c2.Tail](c3: C3): c3.Tail = ???

  val self = new Chain{ type Tail = this.type }
  val res: self.type = f(self)(self)(self)

  type C <: Chain

  val c3 = new Chain{ type Tail = C }
  val c2 = new Chain{ type Tail = c3.type }
  val c1 = new Chain{ type Tail = c2.type }
  val u: C = f(c1)(c2)(c3)
}

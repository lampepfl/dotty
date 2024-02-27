case class Prod[+A](name: A, num: Int)
class      Get[+B] { def isEmpty: Boolean = ???; def get: B = ??? }
class      Named[+C] { def _1: C = ???; def _2: Int = ??? }
class      Xs[+D] { def apply(i: Int): D = ???; def length: Int = ???; def drop(n: Int): Seq[D] = ???; def toSeq: Seq[D] = ??? }
case class ProdXs[+E](name: String, num: Int, us: Xs[E])

class In[T]
object Nn extends Get[Nothing]
object Nl extends Xs[Nothing]

object IsBool { def unapply[A](in: In[A]): Boolean                        = false                }
object IsTrue { def unapply[AT](in: In[AT]): true                         = true                 }
object IsFals { def unapply[AF](in: In[AF]): false                        = false                }
object IsProd { def unapply[B](in: In[B]): Prod[B]                        = Prod(???, 42)        }
object IsTup1 { def unapply[T](in: In[T]): T *: Int *: EmptyTuple         = (???, 2)             }
object IsSing { def unapply[S](in: In[S]): Get[S]                         = Nn                   }
object IsName { def unapply[N](in: In[N]): Get[Named[N]]                  = Nn                   }
object IsGTup { def unapply[GT](in: In[GT]): Get[GT *: Int *: EmptyTuple] = Nn                   }
object IsSeq1 { def unapplySeq[E](in: In[E]): Xs[E]                       = Nl                   }
object IsSeq2 { def unapplySeq(any: Any): Xs[Int]                         = Nl                   }
object IsGSeq { def unapplySeq[GE](in: In[GE]): Get[Xs[GE]]               = Nn                   }
object IsPSeq { def unapplySeq[PE](in: In[PE]): ProdXs[PE]                = ProdXs("jim", 1, Nl) }
object IsGPSq { def unapplySeq[GPE](in: In[GPE]): Get[ProdXs[GPE]]        = Nn                   }

object Test:
  def t1(any: Any) =
    any match { case IsBool() => }
    any match { case IsTrue() => }
    any match { case IsFals() => }
    any match { case IsProd(w, n) => }
    any match { case IsTup1(w, n) => }
    any match { case IsSing(str) => }
    any match { case IsName(h, t) => }
    any match { case IsGTup(w, n) => }
    any match { case IsSeq1(a, b, cs*) => }
    any match { case IsSeq2(x, y, zs*) => }
    any match { case IsGSeq(x, y, zs*) => }
    any match { case IsPSeq(w, n, a, b, cs*) => }
    any match { case IsGPSq(w, n, x, y, zs*) => }

  def t2(prod1: Prod[String]) =
    val Prod(name1, num1) = prod1
    println(s"name1=$name1 num1=$num1")

  def main(args: Array[String]): Unit =
    t2(Prod("doug", 42))

class lambdaAnnot(g: () => Int) extends annotation.StaticAnnotation
def f(x: Int): Int @lambdaAnnot(() => x) = x
object Test:
  val y: Int = ???
  val z /*: Int @lambdaAnnot(() => y) */ = f(y)

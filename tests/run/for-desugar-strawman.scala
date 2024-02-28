
@main def Test =
  println:
    for
      x <- List(1, 2, 3)
      y = x + x
      if x >= 2
      i <- List.range(0, y)
      z = i * i
      if z % 2 == 0
    yield
      i * x

  println:
    val xs = List(1, 2, 3)
    xs.flatMapDefined: x =>
      val y = x + x
      xs.applyFilter(x >= 2):
        val is = List.range(0, y)
        is.mapDefined: i =>
          val z = i * i
          is.applyFilter(z % 2 == 0):
            i * x

extension [A](as: List[A])

  def applyFilter[B](p: => Boolean)(b: => B) =
    if p then Some(b) else None

  def flatMapDefined[B](f: A => Option[IterableOnce[B]]): List[B] =
    as.flatMap: x =>
      f(x).getOrElse(Nil)

  def mapDefined[B](f: A => Option[B]): List[B] =
    as.flatMap(f)

object UNDEFINED

extension [A](as: Vector[A])

  def applyFilter[B](p: => Boolean)(b: => B) =
    if p then b else UNDEFINED

  def flatMapDefined[B](f: A => IterableOnce[B] | UNDEFINED.type): Vector[B] =
    as.flatMap: x =>
      f(x) match
        case UNDEFINED => Nil
        case y: IterableOnce[B] => y

  def mapDefined[B](f: A => B | UNDEFINED.type): Vector[B] =
    as.flatMap: x =>
      f(x) match
        case UNDEFINED => Nil
        case y: B => y :: Nil

/*
F ::= val x = E; F
      x <- E; G
G ::= []
      val x = E; G
      if E; G
      x <- E; G

Translation scheme:

{ for F yield E }c    where c = undefined
{ for G yield E }c    where c is a reference to the generator preceding the G sequence

{ for [] yield E }c              =  E
{ for p = Ep; G yield E }c       =  val p = Ep; { for G yield E }c
{ for if Ep; G yield E}c         =  c.applyFilter(Ep)({ for G yield E }c)
{ for p <- Ep; G yield E }c      =  val c1 = Ep; c1.BIND{ case p => { for G yield E }c1 }    (c1 fresh)

      where BIND = flatMapDefined if isGen(G), isFilter(G)
                 = mapDefined     if !isGen(G), isFilter(G)
                 = flatMap        if isGen(G), !isFilter(G)
                 = map            if !isGen(G), !isFilter(G)

{ for case p <- Ep; G yield E }c =  { for $x <- Ep; if $x match case p => true case _ => false; p = $x@RuntimeChecked; G yield E }c
{ for case p = Ep; G yield E }c  =  { for $x = Ep; if $x match case p => true case _ => false; p = $x@RuntimeChecked; G yield E}c

isFilter(if E; S)
isFilter(val x = E; S)  if  isFilter(S)

isGen(x <- E; S)
isGen(val x = E; S)     if isGen(S)
isGen(if E; S)          if isGen(S)

*/

val foo = 1

def main2 =
  foo
  ???
  ??? match { case _ => 0 }
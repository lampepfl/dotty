val a: (1, 2, 3) = (1, 2, 3)
val b: (4, 5, 6) = (4, 5, 6)
val c: (7, 8) = (7, 8)
val d: EmptyTuple = Tuple()

// Zip
val r1: ((1, 4), (2, 5), (3, 6)) = a.zip(b)
val r2: ((1, 7), (2, 8)) = a.zip(c)
val r3: ((7, 1), (8, 2)) = c.zip(a)
val r4: Unit = d.zip(a)
val r5: Unit = a.zip(d)

// Map
case class Foo[X](x: X)

val r6: (Int, Int, Int) = a.map[[t] =>> Int](x => x * x)

val r7: ((1, Foo[1]), (2, Foo[2]), (3, Foo[3])) =
  a.map[[t] =>> (t, Foo[t])](x => (x, Foo(x)))
  // NOTE might not need give type param if had `precise` modifier for x

// More Zip
val t1: Int *: Long *: Tuple = (1, 2l, 100, 200)
val t2: Int *: Char *: Tuple = (1, 'c', 33, 42)
val t3: (Int, Int) *: (Long, Char) *: Tuple = t1.zip(t2)

val t4: Unit = d.zip(d)

@main def Test =
  List(r1, r2, r3, r4, r5, r6, r7, t3, t4).foreach(println)

def println(x: Any): Unit =
  Console.println(if x == () then "()" else x) // portable on Scala.js

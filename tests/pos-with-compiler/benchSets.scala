type Elem = Object

def newElem() = new Object()

val MissFactor = 2

val Runs = 100          // number of runs to warm-up, and then number of runs to test
val ItersPerRun = 1000
val elems = Array.fill(1024 * MissFactor)(newElem())

def testJava =
  val set = java.util.HashMap[Elem, Elem]()
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set.put(e, e)
      i += 1
    while i > 0 do
      i -= 1
      val v = set.get(elems(i))
      if v != null then
        count += 1
    iter += 1
  count

def testJavaId =
  val set = java.util.IdentityHashMap[Elem, Elem]()
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set.put(e, e)
      i += 1
    while i > 0 do
      i -= 1
      val v = set.get(elems(i))
      if v != null then
        count += 1
    iter += 1
  count

def testScalaMap =
  val set = scala.collection.mutable.HashMap[Elem, Elem]()
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set.update(e, e)
      i += 1
    while i > 0 do
      i -= 1
      set.get(elems(i)) match
        case Some(_) => count += 1
        case None =>
    iter += 1
  count

def testAnyRefMap =
  val set = scala.collection.mutable.AnyRefMap[Elem, Elem]()
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set.update(e, e)
      i += 1
    while i > 0 do
      i -= 1
      val v: Elem | Null = set.getOrNull(elems(i))
      if v != null then
        count += 1
    iter += 1
  count

def testDottyMap =
  val set = dotty.tools.dotc.util.HashMap[Elem, Elem](128)
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set.update(e, e)
      i += 1
    while i > 0 do
      i -= 1
      val v = set.lookup(elems(i))
      if v != null then
        count += 1
    iter += 1
  count

def testDottySet =
  val set = dotty.tools.dotc.util.HashSet[Elem](64)
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set += e
      i += 1
    while i > 0 do
      i -= 1
      if set.contains(elems(i)) then
        count += 1
    iter += 1
  count

def testScalaSet =
  val set = scala.collection.mutable.HashSet[Elem]()
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set += e
      i += 1
    while i > 0 do
      i -= 1
      if set.contains(elems(i)) then
        count += 1
    iter += 1
  count

def testLinearSet =
  var set = dotty.tools.dotc.util.LinearSet.empty[Elem]
  var count = 0
  var iter = 0
  while iter < ItersPerRun do
    var i = 0
    while i < elems.length do
      val e = elems(i)
      if i % MissFactor == 0 then
        set += e
      i += 1
    while i > 0 do
      i -= 1
      if set.contains(elems(i)) then
        count += 1
    iter += 1
  count

val expected = (elems.size / MissFactor) * ItersPerRun

def profile(name: String, op: => Int) =
  System.gc()
  for i <- 0 until 100 do assert(op == expected)
  val start = System.nanoTime()
  var count = 0
  for i <- 0 until 100 do count += op
  //println(count)
  assert(count == expected * Runs)
  println(s"$name took ${(System.nanoTime().toDouble - start)/1_000_000} ms")

@main def Test =

  profile("dotty.tools.dotc.LinearSet", testLinearSet)
  profile("dotty.tools.dotc.HashSet  ", testDottySet)
  profile("dotty.tools.dotc.HashMap  ", testDottyMap)
  profile("scala.collection.HashSet  ", testScalaSet)
  profile("scala.collection.AnyRefMap", testAnyRefMap)
  profile("scala.collection.HashMap  ", testScalaMap)
  profile("java.util.IdentityHashMap ", testJavaId)
  profile("java.util.HashMap         ", testJava)

  profile("java.util.HashMap         ", testJava)
  profile("java.util.IdentityHashMap ", testJavaId)
  profile("scala.collection.HashMap  ", testScalaMap)
  profile("scala.collection.AnyRefMap", testAnyRefMap)
  profile("scala.collection.HashSet  ", testScalaSet)
  profile("dotty.tools.dotc.HashMap  ", testDottyMap)
  profile("dotty.tools.dotc.HashSet  ", testDottySet)
  profile("dotty.tools.dotc.LinearSet", testLinearSet)



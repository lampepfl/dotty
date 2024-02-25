class FibonacciIterator() extends Iterator[Int]:

  private var a: Int = 0
  private var b: Int = 1

  def hasNext = true
  def next() =
    val r = a
    (a, b) = (b, a + b)
    r

@main def fib() =
  val i = FibonacciIterator()
  for _ <- 0 until 10 do
    println(i.next())

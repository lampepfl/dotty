class F extends (Int => Unit) {
  def apply(x: Int): Unit = ()
}

def test =
  val x1 = new (Int => Unit) {
    def apply(x: Int): Unit = ()
  }

  val x2: Int -> Unit = new (Int => Unit) { // error
    def apply(x: Int): Unit = ()
  }

  val y1: Int => Unit = new F
  val y2: Int -> Unit = new F // error

  val z1 = () => ()
  val z2: () -> Unit = () => ()

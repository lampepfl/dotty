object Testi4675 {
  def T(x: String) = {
    x.foreach {
      case 's' => println("s")
      case c: Char => println(c) // error
    }
  }
  T("test")
}
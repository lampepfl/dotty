package object p extends p.U {
  def b: Int = 0
  trait Y
}

package p {
  trait U {
    def a: Int = 0
    trait X
  }

  object c
  def c1 = 0 // top-level def
  trait Z
  trait T {
    def a = 1
    def b = 1
    def c = 1
    def c1 = 1

    trait X
    trait Y
    trait Z
  }

  trait RR extends T {
    def m1 = a // ok
    def m2 = b // ok
    def m3 = c // error
    def m4 = c1 // error

    def n1: X // ok
    def n2: Y // ok
    def n3: Z // error
  }
}

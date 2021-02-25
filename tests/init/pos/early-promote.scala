class Y {
 class X {
   class B {
     def g = f
     def g2 = n
   }
   val f = 42
   val b = new B  // warm(B, X.this)
 }

 val n = 10
 val x = new X
 List(x.b)      // unsafe promotion

}

class A { // checking A
  class B {
    def bf = 42
    class C {
      def x = bf // uses outer[C], but never outer[B]
    }
    List((new C).x)
    def c = new C
  }
  val b = new B()
  List(b) // Direct promotion works here
  val af = 42
}

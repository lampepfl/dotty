
package p:
  trait T:
    def t(i: Int) = i + 1

  package object q extends T:
    override def t(i: Int) = i + 2
    def t(s: String) = s + "_2"

  package q:

    class C extends T:
      def c = t(42) // OK


package p:
  trait T:
    def t(i: Int) = i + 1
    def t(s: String) = s + "_1"

  package object q extends T

  package q:

    class C extends T:
      def c = t(42) // OK

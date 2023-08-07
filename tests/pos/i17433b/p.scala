
package p:
  trait T:
    def t(i: Int) = i + 1
    def t(s: String) = s + "_1"

  package object q extends T

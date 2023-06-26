inline trait A:
  enum Inner:
    case A, B, C

class B extends A:
  def f = Inner.B
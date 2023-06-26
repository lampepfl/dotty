inline trait Cov[+T]:
  def f: T = ???

inline trait Contr[-T]:
  def f(x: T) = ???

class A extends Cov[AnyVal]
class B extends Contr[Int]
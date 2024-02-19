object o { u =>
  opaque type T = String

  def st = summon[String =:= T]
  def su = summon[String =:= u.T]
  def so = summon[String =:= o.T]

  def ts = summon[T =:= String]
  def tu = summon[T =:= u.T]
  def to = summon[T =:= o.T]

  def us = summon[u.T =:= String]
  def ut = summon[u.T =:= T]
  def uo = summon[u.T =:= o.T]

  def os = summon[o.T =:= String]
  def ot = summon[o.T =:= T]
  def ou = summon[o.T =:= u.T]

  def ms(x: String): Int = x.length // ok
  def mt(x:      T): Int = x.length // ok
  def mu(x:    u.T): Int = x.length // ok
  def mo(x:    o.T): Int = x.length // was: error: value length is not a member of o.T
}

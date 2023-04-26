// Testing relaxed overriding check for explicit nulls.
// The relaxed check is only enabled if one of the members is Java defined.

import java.util.Comparator

class C1[T <: AnyRef] extends Ordering[T]:
  override def compare(o1: T, o2: T): Int = 0

// The following overriding is not allowed, because `compare`
// has already been declared in Scala class `Ordering`.
// class C2[T <: AnyRef] extends Ordering[T]:
//   override def compare(o1: T | Null, o2: T | Null): Int = 0

class D1[T <: AnyRef] extends Comparator[T]:
  override def compare(o1: T, o2: T): Int = 0

class D2[T <: AnyRef] extends Comparator[T]:
  override def compare(o1: T | Null, o2: T | Null): Int = 0

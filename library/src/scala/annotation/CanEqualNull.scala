package scala.annotation

/** An annotation makes reference types comparable to `null` in explicit nulls.
 * `CanEqualNull` is a special refining annotation. An annotated type is equivalent to the original type.
 *
 *  For example:
 *  ```scala
 *  val s1: String = ???
 *  s1 == null // error
 *  val s2: String @CanEqualNull = ???
 *  s2 == null // ok
 *
 *  // String =:= String @CanEqualNull
 *  val s3: String = s2
 *  val s4: String @CanEqualNull = s1
 *
 *  val ss: Array[String @CanEqualNull] = ???
 *  ss.map(_ == null)
 *  ```
 */
@experimental
final class CanEqualNull extends RefiningAnnotation

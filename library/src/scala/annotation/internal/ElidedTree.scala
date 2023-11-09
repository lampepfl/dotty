package scala.annotation.internal

import scala.annotation.Annotation

/** An annotation produced by outline typing to indicate that a
 *  tree was elided.
 *
 *  e.g.
 *  ```
 *  val foo: Int = {...}
 *  ```
 *  will be transformed to
 *  ```
 *  val foo: Int = (_ : @ElidedTree)
 *  ```
 */
final class ElidedTree() extends Annotation

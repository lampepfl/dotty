/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package mutable

import language.experimental.captureChecking

/** This trait forms part of collections that can be augmented
  * using a `+=` operator and that can be cleared of all elements using
  * a `clear` method.
  *
  * @define coll growable collection
  * @define Coll `Growable`
  * @define add add
  * @define Add Add
  */
trait Growable[-A] extends Clearable {

  /** ${Add}s a single element to this $coll.
   *
   *  @param elem  the element to $add.
   *  @return the $coll itself
   */
  def addOne(elem: A): this.type

  /** Alias for `addOne` */
  @`inline` final def += (elem: A): this.type = addOne(elem)

  //TODO This causes a conflict in StringBuilder; looks like a compiler bug
  //@deprecated("Use addOne or += instead of append", "2.13.0")
  //@`inline` final def append(elem: A): Unit = addOne(elem)

  /** ${Add}s two or more elements to this $coll.
   *
   *  @param elem1 the first element to $add.
   *  @param elem2 the second element to $add.
   *  @param elems the remaining elements to $add.
   *  @return the $coll itself
   */
  @deprecated("Use `++=` aka `addAll` instead of varargs `+=`; infix operations with an operand of multiple args will be deprecated", "2.13.0")
  @`inline` final def += (elem1: A, elem2: A, elems: A*): this.type = this += elem1 += elem2 ++= (elems: IterableOnce[A])

  /** ${Add}s all elements produced by an IterableOnce to this $coll.
   *
   *  @param xs   the IterableOnce producing the elements to $add.
   *  @return  the $coll itself.
   */
  def addAll(xs: IterableOnce[A]^): this.type = {
    if (xs.asInstanceOf[AnyRef] eq this) addAll(Buffer.from(xs)) // avoid mutating under our own iterator
    else {
      val it = xs.iterator
      while (it.hasNext) {
        addOne(it.next())
      }
    }
    this
  }

  /** Alias for `addAll` */
  @`inline` final def ++= (xs: IterableOnce[A]^): this.type = addAll(xs)

  /** @return The number of elements in the collection under construction, if it can be cheaply computed,
    *  -1 otherwise. The default implementation always returns -1.
    */
  def knownSize: Int = -1
}

object Growable {

  /**
    * Fills a `Growable` instance with the elements of a given iterable
    * @param empty Instance to fill
    * @param it Elements to add
    * @tparam A Element type
    * @return The filled instance
    */
  def from[A](empty: Growable[A], it: collection.IterableOnce[A]^): empty.type = empty ++= it

}

/** This trait forms part of collections that can be cleared
  *  with a clear() call.
  *
  *  @define coll collection
  */
trait Clearable {
  /** Clears the $coll's contents. After this operation, the
    *  $coll is empty.
    */
  def clear(): Unit
}

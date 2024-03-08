package scala.annotation
package internal

/** An annotation that is used for marking type definitions that should get
 *  context bound companions. The scheme is as follows:
 *
 *  1. When desugaring a context-bounded type A, add a @WitnessNames(n_1, ... , n_k)
 *  annotation to the type declaration node, where n_1, ..., n_k are the names of
 *  all the witnesses generated for the context bounds of A. This annotation will
 *  be pickled as usual.
 *
 *  2. During Namer or Unpickling, when encountering a type declaration A with
 *  a WitnessNames(n_1, ... , n_k) annotation, create a CB companion `val A` with
 *  rtype `<context-bound-companion>`[ref_1 | ... | ref_k] where ref_i is a TermRef
 *  with the same prefix as A and name n_i. Except, don't do this if the type in
 *  question is a type parameter and there is already a term parameter with name A
 *  defined for the same method.
 *
 *  ContextBoundCompanion is defined as an internal abstract type like this:
 *
 *     type `<context-bound-companion>`[-Refs]
 *
 *  The context bound companion's variance is negative, so that unons in the
 *  arguments are joined when encountering multiple definfitions and forming a glb.
 *
 *  3. Add a special case for typing a selection A.m on a value A of type
 *  ContextBoundCompanion[ref_1, ..., ref_k]. Namely, try to typecheck all
 *  selections ref_1.m, ..., ref_k.m with the expected type. There must be
 *  a unique selection ref_i.m that typechecks and such that for all other
 *  selections ref_j.m that also typecheck one of the following three criteria
 *  applies:
 *
 *    1. ref_i.m and ref_j.m are the same. This means: If they are types then
 *       ref_i.m is an alias of ref_j.m. If they are terms then they are both
 *       singleton types and ref_i.m =:= ref_j.m.
 *    2. The underlying type (under widen) of ref_i is a true supertype of the
 *       underlying type of ref_j.
 *    3. ref_i.m is a term, the underlying type of ref_j is not a strict subtype
 *       of the underlying type of ref_j, and the underlying type ref_i.m is a
 *       strict subtype of the underlying type of ref_j.m.
 *
 *  If there is such a selection, map A.m to ref_i.m, otherwise report an error.
 *
 *  (2) might surprise. It is the analogue of given disambiguation, where we also
 *  pick the most general candidate that matches the expected type. E.g. we have
 *  context bounds for Functor, Monad, and Applicable. In this case we want to
 *  select the `map` method of `Functor`.
 *
 *  4. At PostTyper, issue an error when encountering any reference to a CB companion.
 */
class WitnessNames(names: String*) extends StaticAnnotation



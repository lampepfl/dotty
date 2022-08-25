package scala.runtime.stdLibPatches

import scala.annotation.compileTimeOnly

/** Scala 3 additions and replacements to the `scala.language` object.
 */
object language:

  /** The experimental object contains features that have been recently added but have not
   *  been thoroughly tested in production yet.
   *
   *  Experimental features '''may undergo API changes''' in future releases, so production
   *  code should not rely on them.
   *
   *  Programmers are encouraged to try out experimental features and
   *  [[http://issues.scala-lang.org report any bugs or API inconsistencies]]
   *  they encounter so they can be improved in future releases.
   *
   *  @group experimental
   */
  object experimental:

    /* Experimental support for richer dependent types (disabled for now)
     * One can still run the compiler with support for parsing singleton applications
     * using command line option `-language:experimental.dependent`.
     * But one cannot use a feature import for this as long as this entry is commented out.
     */
    //object dependent

    /** Experimental support for named type arguments.
      *
      * @see [[https://dotty.epfl.ch/docs/reference/other-new-features/named-typeargs]]
      */
    @compileTimeOnly("`namedTypeArguments` can only be used at compile time in import statements")
    object namedTypeArguments

    /** Experimental support for generic number literals.
      *
      * @see [[https://dotty.epfl.ch/docs/reference/changed-features/numeric-literals]]
      */
    @compileTimeOnly("`genericNumberLiterals` can only be used at compile time in import statements")
    object genericNumberLiterals

    /** Experimental support for `erased` modifier
     *
     *  @see [[https://dotty.epfl.ch/docs/reference/experimental/erased-defs]]
     */
    @compileTimeOnly("`erasedDefinitions` can only be used at compile time in import statements")
    object erasedDefinitions

    /** Experimental support for using indentation for arguments
     */
    @compileTimeOnly("`fewerBraces` can only be used at compile time in import statements")
    object fewerBraces

    /** Experimental support for typechecked exception capabilities
     *
     *  @see [[https://dotty.epfl.ch/docs/reference/experimental/canthrow]]
     */
    @compileTimeOnly("`saferExceptions` can only be used at compile time in import statements")
    object saferExceptions

    /** Experimental support for unsafe Java return in explicit nulls
     */
    @compileTimeOnly("`unsafeJavaReturn` can only be used at compile time in import statements")
    object unsafeJavaReturn

  end experimental

  /** The deprecated object contains features that are no longer officially suypported in Scala.
   *  Features in this object are slated for removal. New code should not use them and
   *  old code should migrate away from them.
   */
  @compileTimeOnly("`deprecated` can only be used at compile time in import statements")
  object deprecated:

    /** Symbol literals have been deprecated since 2.13. Since Scala 3.0 they
     *  are no longer an official part of Scala. For compatibility with legacy software,
     *  symbol literals are still supported with a language import, but new software
     *  should not use them.
     */
    @compileTimeOnly("`symbolLiterals` can only be used at compile time in import statements")
    object symbolLiterals
  end deprecated

  /** Where imported, auto-tupling is disabled.
    *
    * '''Why control the feature?''' Auto-tupling can lead to confusing and
    * brittle code in presence of overloads. In particular, surprising overloads
    * can be selected, and adding new overloads can change which overload is selected
    * in suprising ways.
    *
    * '''Why allow it?''' Not allowing auto-tupling is difficult to reconcile with
    * operators accepting tuples.
    */
  @compileTimeOnly("`noAutoTupling` can only be used at compile time in import statements")
  object noAutoTupling

  /** Where imported, loose equality using eqAny is disabled.
    *
    * '''Why allow and control the feature?''' For compatibility and migration reasons,
    * strict equality is opt-in. See linked documentation for more information.
    *
    * @see [[https://dotty.epfl.ch/docs/reference/contextual/multiversal-equality]]
    */
  @compileTimeOnly("`strictEquality` can only be used at compile time in import statements")
  object strictEquality

  /** Where imported, ad hoc extensions of non-open classes in other
   *  compilation units are allowed.
   *
   *  '''Why control the feature?''' Ad-hoc extensions should usually be avoided
   *  since they typically cannot rely on an "internal" contract between a class
   *  and its extensions. Only open classes need to specify such a contract.
   *  Ad-hoc extensions might break for future versions of the extended class,
   *  since the extended class is free to change its implementation without
   *  being constrained by an internal contract.
   *
   *  '''Why allow it?''' An ad-hoc extension can sometimes be necessary,
   *  for instance when mocking a class in a testing framework, or to work
   *  around a bug or missing feature in the original class. Nevertheless,
   *  such extensions should be limited in scope and clearly documented.
   *  That's why the language import is required for them.
   */
  @compileTimeOnly("`adhocExtensions` can only be used at compile time in import statements")
  object adhocExtensions

  /** Unsafe Nulls fot Explicit Nulls
    * Inside the "unsafe" scope, `Null` is considered as a subtype of all reference types.
    *
    * @see [[http://dotty.epfl.ch/docs/reference/other-new-features/explicit-nulls.html]]
    */
  @compileTimeOnly("`unsafeNulls` can only be used at compile time in import statements")
  object unsafeNulls

  @compileTimeOnly("`future` can only be used at compile time in import statements")
  object future

  @compileTimeOnly("`future-migration` can only be used at compile time in import statements")
  object `future-migration`

  /** Set source version to 3.0-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.0-migration` can only be used at compile time in import statements")
  object `3.0-migration`

  /** Set source version to 3.0.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.0` can only be used at compile time in import statements")
  object `3.0`

  /** Set source version to 3.1-migration.
    *
    * This is a no-op, and should not be used. A syntax error will be reported upon import.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.1-migration` can only be used at compile time in import statements")
  @deprecated("`3.1-migration` is not valid, use `3.1` instead", since = "3.2")
  object `3.1-migration`

  /** Set source version to 3.1
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.1` can only be used at compile time in import statements")
  object `3.1`

  /** Set source version to 3.2-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.2-migration` can only be used at compile time in import statements")
  object `3.2-migration`

  /** Set source version to 3.2
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.2` can only be used at compile time in import statements")
  object `3.2`

/* This can be added when we go to 3.3
  /** Set source version to 3.3-migration.
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.3-migration` can only be used at compile time in import statements")
  object `3.3-migration`

  /** Set source version to 3.3
    *
    * @see [[https://docs.scala-lang.org/scala3/guides/migration/compatibility-intro.html]]
    */
  @compileTimeOnly("`3.3` can only be used at compile time in import statements")
  object `3.3`
*/
end language

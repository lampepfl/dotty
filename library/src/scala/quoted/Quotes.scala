package scala.quoted

import scala.annotation.experimental
import scala.annotation.implicitNotFound
import scala.reflect.TypeTest

/** Current Quotes in scope
 *
 *  Usage:
 *  ```scala
 *  import scala.quoted.*
 *  def myExpr[T](using Quotes): Expr[T] = {
 *     import quotes.reflect.*
 *     ???
 *  }
 *  ```
 */
transparent inline def quotes(using q: Quotes): q.type = q

/** Quotation context provided by a macro expansion or in the scope of `scala.quoted.staging.run`.
 *  Used to perform all operations on quoted `Expr` or `Type`.
 *
 *  It contains the low-level Typed AST API metaprogramming API.
 *  This API does not have the static type guarantees that `Expr` and `Type` provide.
 *  `Quotes` are generated from an enclosing `${ ... }` or `scala.staging.run`. For example:
 *  ```scala sc:nocompile
 *  import scala.quoted.*
 *  inline def myMacro: Expr[T] =
 *    ${ /* (quotes: Quotes) ?=> */ myExpr }
 *  def myExpr(using Quotes): Expr[T] =
 *    '{ f(${ /* (quotes: Quotes) ?=> */ myOtherExpr }) }
 *  }
 *  def myOtherExpr(using Quotes): Expr[U] = '{ ... }
 *  ```
 */

@implicitNotFound("""explain=Maybe this method is missing a `(using Quotes)` parameter.

Maybe that splice `$ { ... }` is missing?
Given instances of `Quotes` are generated from an enclosing splice `$ { ... }` (or `scala.staging.run` call).
A splice can be thought as a method with the following signature.
  def $[T](body: Quotes ?=> Expr[T]): T
""")
trait Quotes { self: runtime.QuoteUnpickler & runtime.QuoteMatching =>

  // Extension methods for `Expr[T]`
  extension [T](self: Expr[T])
    /** Show a source code like representation of this expression */
    def show: String

    /** Pattern matches `this` against `that`. Effectively performing a deep equality check.
    *  It does the equivalent of
    *  ```scala sc:nocompile
    *  this match
    *    case '{...} => true // where the contents of the pattern are the contents of `that`
    *    case _ => false
    *  ```
    */
    def matches(that: Expr[Any]): Boolean

    /** Return the value of this expression.
     *
     *  Returns `None` if the expression does not represent a value or possibly contains side effects.
     *  Otherwise returns the `Some` of the value.
     */
    def value(using FromExpr[T]): Option[T] =
      given Quotes = Quotes.this
      summon[FromExpr[T]].unapply(self)

    /** Return the value of this expression.
     *
     *  Emits an error and throws if the expression does not represent a value or possibly contains side effects.
     *  Otherwise returns the value.
     */
    @deprecated("Use valueOrAbort", "3.1.0")
    def valueOrError(using FromExpr[T]): T =
      val fromExpr = summon[FromExpr[T]]
      def reportError =
        val msg = s"Expected a known value. \n\nThe value of: ${self.show}\ncould not be extracted using $fromExpr"
        reflect.report.throwError(msg, self)
      given Quotes = Quotes.this
      fromExpr.unapply(self).getOrElse(reportError)

    /** Return the value of this expression.
     *
     *  Emits an error and aborts if the expression does not represent a value or possibly contains side effects.
     *  Otherwise returns the value.
     */
    def valueOrAbort(using FromExpr[T]): T

  end extension

  // Extension methods for `Expr[Any]` that take another explicit type parameter
  extension (self: Expr[Any])
    /** Checks is the `quoted.Expr[?]` is valid expression of type `X` */
    def isExprOf[X](using Type[X]): Boolean

    /** Convert this to an `quoted.Expr[X]` if this expression is a valid expression of type `X` or throws */
    def asExprOf[X](using Type[X]): Expr[X]
  end extension

  /** Low-level Typed AST metaprogramming API.
   *
   *  Provides all functionality related to AST-based metaprogramming.
   *
   *  Usage:
   *  ```scala
   *  import scala.quoted.*
   *  def f(expr: Expr[Int])(using Quotes) =
   *    import quotes.reflect.*
   *    val ast: Term = expr.asTerm
   *    ???
   *  ```
   *
   *  See `reflectModule` for full API.
   *
   */
  val reflect: reflectModule

  /** Low-level Typed AST metaprogramming API.
   *
   *  Provides all functionality related to AST-based metaprogramming.
   *
   *  Each type `XYZ` in the API is defined as an abstract type `type XYZ`.
   *  Methods on `XYZ` are provided by a `given XYZMethods` which implements extension methods on `XYZ` in the trait `XYZMethods`.
   *  The `XYZ` module is defined by a `val XYZ: XYZModule` which contains the methods defined in `XYZModule`.
   *  Type tests (`TypeTest`) are also given to perform subtype checks on these types.
   *
   *  Type hierarchy
   *  ```none
   *
   *  +- Tree -+- PackageClause
   *           |
   *           +- Statement -+- Import
   *           |             +- Export
   *           |             +- Definition --+- ClassDef
   *           |             |               +- TypeDef
   *           |             |               +- ValOrDefDef -+- DefDef
   *           |             |                               +- ValDef
   *           |             |
   *           |             +- Term --------+- Ref -+- Ident -+- Wildcard
   *           |                             |       +- Select
   *           |                             |
   *           |                             +- Literal
   *           |                             +- This
   *           |                             +- New
   *           |                             +- NamedArg
   *           |                             +- Apply
   *           |                             +- TypeApply
   *           |                             +- Super
   *           |                             +- Assign
   *           |                             +- Block
   *           |                             +- Closure
   *           |                             +- If
   *           |                             +- Match
   *           |                             +- SummonFrom
   *           |                             +- Try
   *           |                             +- Return
   *           |                             +- Repeated
   *           |                             +- Inlined
   *           |                             +- SelectOuter
   *           |                             +- While
   *           |                             +---+- Typed
   *           |                                /
   *           +- TypedOrTest +----------------·
   *           +- Bind
   *           +- Unapply
   *           +- Alternatives
   *           |
   *           +- CaseDef
   *           +- TypeCaseDef
   *           |
   *           +- TypeTree ----+- Inferred
   *           |               +- TypeIdent
   *           |               +- TypeSelect
   *           |               +- TypeProjection
   *           |               +- Singleton
   *           |               +- Refined
   *           |               +- Applied
   *           |               +- Annotated
   *           |               +- MatchTypeTree
   *           |               +- ByName
   *           |               +- LambdaTypeTree
   *           |               +- TypeBind
   *           |               +- TypeBlock
   *           |
   *           +- TypeBoundsTree
   *           +- WildcardTypeTree
   *
   *  +- ParamClause -+- TypeParamClause
   *                  +- TermParamClause
   *
   *  +- TypeRepr -+- NamedType -+- TermRef
   *               |             +- TypeRef
   *               +- ConstantType
   *               +- SuperType
   *               +- Refinement
   *               +- AppliedType
   *               +- AnnotatedType
   *               +- AndOrType -+- AndType
   *               |             +- OrType
   *               +- MatchType
   *               +- ByNameType
   *               +- ParamRef
   *               +- ThisType
   *               +- RecursiveThis
   *               +- RecursiveType
   *               +- LambdaType -+- MethodOrPoly -+- MethodType
   *               |              |                +- PolyType
   *               |              +- TypeLambda
   *               +- MatchCase
   *               +- TypeBounds
   *               +- NoPrefix
   *
   *  +- Selector -+- SimpleSelector
   *               +- RenameSelector
   *               +- OmitSelector
   *               +- GivenSelector
   *
   *  +- Signature
   *
   *  +- Position
   *
   *  +- SourceFile
   *
   *  +- Constant -+- BooleanConstant
   *               +- ByteConstant
   *               +- ShortConstant
   *               +- IntConstant
   *               +- LongConstant
   *               +- FloatConstant
   *               +- DoubleConstant
   *               +- CharConstant
   *               +- StringConstant
   *               +- UnitConstant
   *               +- NullConstant
   *               +- ClassOfConstant
   *  +- Symbol
   *
   *  +- Flags
   *
   *  ```
   *
   */
  trait reflectModule { self: reflect.type =>

    /** Module object of `type CompilationInfo`  */
    val CompilationInfo: CompilationInfoModule

    /** Methods of the module object `val CompilationInfo` */
    trait CompilationInfoModule { this: CompilationInfo.type =>
      /** Are we expanding a `inline` macro while typing the program?
       *
       *  This will be true when the macro is used in a transparent inline.
       */
      def isWhileTyping: Boolean

      /** Expose macro-specific settings as a list of strings.
       *  Settings can be set from command line with help of -Xmacro-settings options.
       *
       *  These will be used to expand any transparent macros or any non-transparent macro that is forced to expand while expanding the transparent macro.
       *  Non-transparent macros are not guaranteed to be expanded with the same set of settings.
       */
      @experimental
      def XmacroSettings: List[String]
    }


    /** Returns the `Term` representation this expression */
    extension (expr: Expr[Any])
      def asTerm: Term

    ///////////////
    //   TREES   //
    ///////////////

    /** Tree representing code written in the source */
    type Tree <: AnyRef

    /** Module object of `type Tree`  */
    val Tree: TreeModule

    /** Methods of the module object `val Tree` */
    trait TreeModule { this: Tree.type => }

    /** Makes extension methods on `Tree` available without any imports */
    given TreeMethods: TreeMethods

    /** Extension methods of `Tree` */
    trait TreeMethods {

      extension (self: Tree)
        /** Position in the source code */
        def pos: Position

        /** Symbol of defined or referred by this tree */
        def symbol: Symbol

        /** Shows the tree as String */
        def show(using Printer[Tree]): String

        /** Does this tree represent a valid expression? */
        def isExpr: Boolean

        /** Convert this tree to an `quoted.Expr[Any]` if the tree is a valid expression or throws */
        def asExpr: Expr[Any]
      end extension

      /** Convert this tree to an `quoted.Expr[T]` if the tree is a valid expression or throws */
      extension (self: Tree)
        def asExprOf[T](using Type[T]): Expr[T]

      extension [ThisTree <: Tree](self: ThisTree)
        /** Changes the owner of the symbols in the tree */
        def changeOwner(newOwner: Symbol): ThisTree
      end extension

    }

    /** Tree representing a package clause in the source code
     *
     *  ```scala sc:nocompile
     *  package foo {
     *    // package stats
     *  }
     *  ```
     *
     *  or
     *
     *  ```scala sc:nocompile
     *  package foo.bar
     *  // package stats
     *  ```
     */
    type PackageClause <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `PackageClause` */
    given PackageClauseTypeTest: TypeTest[Tree, PackageClause]

    /** Module object of `type PackageClause`  */
    val PackageClause: PackageClauseModule

    /** Methods of the module object `val PackageClause` */
    trait PackageClauseModule { this: PackageClause.type =>
      /** Create a package clause `package pid { stats }` */
      def apply(pid: Ref, stats: List[Tree]): PackageClause
      /** Copy a package clause `package pid { stats }` */
      def copy(original: Tree)(pid: Ref, stats: List[Tree]): PackageClause
      /** Matches a package clause `package pid { stats }` and extracts the `pid` and `stats` */
      def unapply(tree: PackageClause): (Ref, List[Tree])
    }

    /** Makes extension methods on `PackageClause` available without any imports */
    given PackageClauseMethods: PackageClauseMethods

    /** Extension methods of `PackageClause` */
    trait PackageClauseMethods:
      extension (self: PackageClause)
        /** Tree containing the package name */
        def pid: Ref
        /** Definitions, imports or exports within the package */
        def stats: List[Tree]
      end extension
    end PackageClauseMethods

    /** Tree representing an import in the source code.
     *
     *  See also documentation on `Selector`.
     */
    type Import <: Statement

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Import` */
    given ImportTypeTest: TypeTest[Tree, Import]

    /** Module object of `type Import`  */
    val Import: ImportModule

    /** Methods of the module object `val Import` */
    trait ImportModule { this: Import.type =>
      /** Create an `Import` with the given qualifier and selectors */
      def apply(expr: Term, selectors: List[Selector]): Import
      /** Copy an `Import` with the given qualifier and selectors */
      def copy(original: Tree)(expr: Term, selectors: List[Selector]): Import
      /** Matches an `Import` and extracts the qualifier and selectors */
      def unapply(tree: Import): (Term, List[Selector])
    }

    /** Makes extension methods on `Import` available without any imports */
    given ImportMethods: ImportMethods

    /** Extension methods of `Import` */
    trait ImportMethods:
      extension (self: Import)
        /** Qualifier of the import */
        def expr: Term
        /** List selectors of the import
         *
         *  See documentation on `Selector`
         */
        def selectors: List[Selector]
      end extension
    end ImportMethods

    /** Tree representing an export clause in the source code.
     *  Export forwarders generated from this clause appear in the same scope.
     */
    type Export <: Statement

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Export` */
    given ExportTypeTest: TypeTest[Tree, Export]

    /** Module object of `type Export`  */
    val Export: ExportModule

    /** Methods of the module object `val Export` */
    trait ExportModule { this: Export.type =>
      /** Matches an `Export` and extracts the qualifier and selectors */
      def unapply(tree: Export): (Term, List[Selector])
    }

    /** Makes extension methods on `Export` available without any imports */
    given ExportMethods: ExportMethods

    /** Extension methods of `Export` */
    trait ExportMethods:
      extension (self: Export)
        /** Qualifier of the export */
        def expr: Term
        /** List selectors of the export
         *
         *  See documentation on `Selector`
         */
        def selectors: List[Selector]
      end extension
    end ExportMethods

    /** Tree representing a statement in the source code */
    type Statement <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Statement` */
    given StatementTypeTest: TypeTest[Tree, Statement]

    // ----- Definitions ----------------------------------------------

    /** Tree representing a definition in the source code. It can be `ClassDef`, `TypeDef`, `DefDef` or `ValDef` */
    type Definition <: Statement

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Definition` */
    given DefinitionTypeTest: TypeTest[Tree, Definition]

    /** Module object of `type Definition`  */
    val Definition: DefinitionModule

    /** Methods of the module object `val Definition` */
    trait DefinitionModule { this: Definition.type => }

    /** Makes extension methods on `Definition` available without any imports */
    given DefinitionMethods: DefinitionMethods

    /** Extension methods of `Definition` */
    trait DefinitionMethods:
      extension (self: Definition)
        /** Name of the definition */
        def name: String
      end extension
    end DefinitionMethods

    // ClassDef

    /** Tree representing a class definition. This includes anonymous class definitions and the class of a module object */
    type ClassDef <: Definition

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `ClassDef` */
    given ClassDefTypeTest: TypeTest[Tree, ClassDef]

    /** Module object of `type ClassDef`  */
    val ClassDef: ClassDefModule

    /** Methods of the module object `val ClassDef` */
    trait ClassDefModule { this: ClassDef.type =>
      /** Create a class definition tree
       *
       *  @param cls The class symbol. A new class symbol can be created using `Symbol.newClass`.
       *  @param parents The parents trees class. The trees must align with the parent types of `cls`.
       *                 Parents can be `TypeTree`s if they don't have term parameter,
       *                 otherwise the can be `Term` containing the `New` applied to the parameters of the extended class.
       *  @param body List of members of the class. The members must align with the members of `cls`.
       */
      // TODO add selfOpt: Option[ValDef]?
      @experimental def apply(cls: Symbol, parents: List[Tree /* Term | TypeTree */], body: List[Statement]): ClassDef
      def copy(original: Tree)(name: String, constr: DefDef, parents: List[Tree /* Term | TypeTree */], selfOpt: Option[ValDef], body: List[Statement]): ClassDef
      def unapply(cdef: ClassDef): (String, DefDef, List[Tree /* Term | TypeTree */], Option[ValDef], List[Statement])


      /** Create the ValDef and ClassDef of a module (equivalent to an `object` declaration in source code).
       *
       *  Equivalent to
       *  ```
       *  def module(module: Symbol, parents: List[Tree], body: List[Statement]): (ValDef, ClassDef) =
       *    val modCls = module.moduleClass
       *    val modClassDef = ClassDef(modCls, parents, body)
       *    val modValDef = ValDef(module, Some(Apply(Select(New(TypeIdent(modCls)), cls.primaryConstructor), Nil)))
       *    List(modValDef, modClassDef)
       *  ```
       *
       *  @param module the module symbol (created using `Symbol.newModule`)
       *  @param parents parents of the module class
       *  @param body body of the module class
       *  @return The module lazy val definition and module class definition.
       *          These should be added one after the other (in that order) in the body of a class or statements of a block.
       *
       *  @syntax markdown
       */
      // TODO add selfOpt: Option[ValDef]?
      @experimental def module(module: Symbol, parents: List[Tree /* Term | TypeTree */], body: List[Statement]): (ValDef, ClassDef)
    }

    /** Makes extension methods on `ClassDef` available without any imports */
    given ClassDefMethods: ClassDefMethods

    /** Extension methods of `ClassDef` */
    trait ClassDefMethods:
      extension (self: ClassDef)
        /** The primary constructor of this class */
        def constructor: DefDef
        /** List of extended parent classes or traits.
         *  The first parent is always a class.
         */
        def parents: List[Tree /* Term | TypeTree */]
        /** Self-type of the class
         *
         *  ```scala
         *  //{
         *  type T
         *  //}
         *  class C { self: T =>
         *    ???
         *  }
         *  ```
         */
        def self: Option[ValDef]
        /** Statements within the class
         *
         *  ```scala
         *  class C {
         *    ??? // statements
         *  }
         *  ```
         */
        def body: List[Statement]
      end extension
    end ClassDefMethods

    // ValOrDefDef

    /** Tree representing a value or method definition in the source code.
     *  This includes `def`, `val`, `lazy val`, `var`, `object` and parameter definitions.
     */
    type ValOrDefDef <: Definition

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `ValOrDefDef` */
    given ValOrDefDefTypeTest: TypeTest[Tree, ValOrDefDef]

    /** Makes extension methods on `ValOrDefDef` available without any imports */
    given ValOrDefDefMethods: ValOrDefDefMethods

    /** Extension methods of `ValOrDefDef` */
    trait ValOrDefDefMethods:
      extension (self: ValOrDefDef)
        /** The type tree of this `val` or `def` definition */
        def tpt: TypeTree
        /** The right-hand side of this `val` or `def` definition */
        def rhs: Option[Term]
      end extension
    end ValOrDefDefMethods

    // DefDef

    /** Tree representing a method definition in the source code */
    type DefDef <: ValOrDefDef

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `DefDef` */
    given DefDefTypeTest: TypeTest[Tree, DefDef]

    /** Module object of `type DefDef`  */
    val DefDef: DefDefModule

    /** Methods of the module object `val DefDef` */
    trait DefDefModule { this: DefDef.type =>
      /** Create a method definition `def f[..](...)` with the signature defined in the symbol.
       *
       *  The `rhsFn` is a function that receives references to its parameters and should return
       *  `Some` containing the implementation of the method. Returns `None` the method has no implementation.
       *  Any definition directly inside the implementation should have `symbol` as owner.
       *
       *  Use `Symbol.asQuotes` to create the rhs using quoted code.
       *
       *  See also: `Tree.changeOwner`
       */
      def apply(symbol: Symbol, rhsFn: List[List[Tree]] => Option[Term]): DefDef
      def copy(original: Tree)(name: String, paramss: List[ParamClause], tpt: TypeTree, rhs: Option[Term]): DefDef
      def unapply(ddef: DefDef): (String, List[ParamClause], TypeTree, Option[Term])
    }

    /** Makes extension methods on `DefDef` available without any imports */
    given DefDefMethods: DefDefMethods

    /** Extension methods of `DefDef` */
    trait DefDefMethods:
      extension (self: DefDef)
        /** List of type and term parameter clauses */
        def paramss: List[ParamClause]

        /** List of leading type parameters or Nil if the method does not have leading type parameters.
         *
         *  Note: Non leading type parameters can be found in extension methods such as
         *  ```scala
         *  //{
         *  type A
         *  type T
         *  //}
         *  extension (a: A) def f[T]() = ???
         *  ```
         */
        def leadingTypeParams: List[TypeDef]

        /** List of parameter clauses following the leading type parameters or all clauses.
         *  Return all parameter clauses if there are no leading type parameters.
         *
         *  Non leading type parameters can be found in extension methods such as
         *  ```scala
         *  //{
         *  type T
         *  type A
         *  //}
         *  extension (a: A) def f[T]() = ???
         *  ```
         */
        def trailingParamss: List[ParamClause]

        /** List of term parameter clauses */
        def termParamss: List[TermParamClause]

        /** The tree of the return type of this `def` definition */
        def returnTpt: TypeTree

        /** The tree of the implementation of the method.
         *  Returns `None` if the method does not have an implementation.
         */
        def rhs: Option[Term]
      end extension
    end DefDefMethods

    // ValDef

    /** Tree representing a value definition in the source code. This includes `val`, `lazy val`, `var`, `object` and parameter definitions. */
    type ValDef <: ValOrDefDef

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `ValDef` */
    given ValDefTypeTest: TypeTest[Tree, ValDef]

    /** Module object of `type ValDef`  */
    val ValDef: ValDefModule

    /** Methods of the module object `val ValDef` */
    trait ValDefModule { this: ValDef.type =>
      /** Create a value definition `val x`, `var x` or `lazy val x` with the signature defined in the symbol.
       *
       *  The `rhs` should return be `Some` containing the implementation of the method.
       *  Returns `None` the method has no implementation.
       *  Any definition directly inside the implementation should have `symbol` as owner.
       *
       *  Use `Symbol.asQuotes` to create the rhs using quoted code.
       *
       *  See also: `Tree.changeOwner`
       */
      def apply(symbol: Symbol, rhs: Option[Term]): ValDef
      def copy(original: Tree)(name: String, tpt: TypeTree, rhs: Option[Term]): ValDef
      def unapply(vdef: ValDef): (String, TypeTree, Option[Term])

      /** Creates a block `{ val <name> = <rhs: Term>; <body(x): Term> }`
       *
       *  Usage:
       *  ```
       *  ValDef.let(owner, "x", rhs1) { x =>
       *    ValDef.let(x.symbol.owner, "y", rhs2) { y =>
       *      // use `x` and `y`
       *    }
       *  }
       *  ```
       */
      def let(owner: Symbol, name: String, rhs: Term)(body: Ref => Term): Term

      /** Creates a block `{ val x = <rhs: Term>; <body(x): Term> }`
       *
       *  Usage:
       *  ```
       *  ValDef.let(owner, rhs1) { x =>
       *    ValDef.let(owner, rhs2) { y =>
       *      // use `x` and `y`
       *    }
       *  }
       *  ```
       */
      def let(owner: Symbol, rhs: Term)(body: Ref => Term): Term =
        let(owner, "x", rhs)(body)

      /** Creates a block `{ val x1 = <terms(0): Term>; ...; val xn = <terms(n-1): Term>; <body(List(x1, ..., xn)): Term> }`
       *
       *  Usage:
       *  ```
       *  ValDef.let(owner, rhsList) { xs =>
       *     ...
       *  }
       *  ```
       */
      def let(owner: Symbol, terms: List[Term])(body: List[Ref] => Term): Term
    }

    /** Makes extension methods on `ValDef` available without any imports */
    given ValDefMethods: ValDefMethods

    /** Extension methods of `ValDef` */
    trait ValDefMethods:
      extension (self: ValDef)
        /** The type tree of this `val` definition */
        def tpt: TypeTree
        /** The right-hand side of this `val` definition */
        def rhs: Option[Term]
      end extension
    end ValDefMethods

    // TypeDef

    /** Tree representing a type (parameter or member) definition in the source code */
    type TypeDef <: Definition

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeDef` */
    given TypeDefTypeTest: TypeTest[Tree, TypeDef]

    /** Module object of `type TypeDef`  */
    val TypeDef: TypeDefModule

    /** Methods of the module object `val TypeDef` */
    trait TypeDefModule { this: TypeDef.type =>
      def apply(symbol: Symbol): TypeDef
      def copy(original: Tree)(name: String, rhs: Tree): TypeDef
      def unapply(tdef: TypeDef): (String, Tree)
    }

    /** Makes extension methods on `TypeDef` available without any imports */
    given TypeDefMethods: TypeDefMethods

    /** Extension methods of `TypeDef` */
    trait TypeDefMethods:
      extension (self: TypeDef)
        /** The type bounds on the right-hand side of this `type` definition */
        def rhs: Tree
      end extension
    end TypeDefMethods


    // ----- Terms ----------------------------------------------------

    /** Tree representing an expression in the source code */
    type Term <: Statement

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Term` */
    given TermTypeTest: TypeTest[Tree, Term]

    /** Module object of `type Term`  */
    val Term: TermModule

    /** Methods of the module object `val Term` */
    trait TermModule { this: Term.type =>

      /** Returns a term that is functionally equivalent to `t`,
      *  however if `t` is of the form `((y1, ..., yn) => e2)(e1, ..., en)`
      *  then it optimizes this the top most call by returning the `Some`
      *  with the result of beta-reducing the application.
      *  Otherwise returns `None`.
      *
      *   To retain semantics the argument `ei` is bound as `val yi = ei` and by-name arguments to `def yi = ei`.
      *   Some bindings may be elided as an early optimization.
      */
      def betaReduce(term: Term): Option[Term]

    }

    /** Makes extension methods on `Term` available without any imports */
    given TermMethods: TermMethods

    /** Extension methods of `Term` */
    trait TermMethods {
      extension (self: Term)

        /** TypeRepr of this term */
        def tpe: TypeRepr

        /** Replace Inlined nodes and InlineProxy references to underlying arguments.
         *  The resulting tree is useful for inspection of the value or content of a non-inline argument.
         *
         *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
         *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
         */
        def underlyingArgument: Term

        /** Replace Ident nodes references to the underlying tree that defined them.
         *  The resulting tree is useful for inspection of the definition of some bindings.
         *
         *  Warning: This tree may contain references that are out of scope and should not be used in the generated code.
         *           This method should only used to port Scala 2 that used to access their outer scope unsoundly.
         */
        def underlying: Term

        /** Converts a partially applied term into a lambda expression */
        def etaExpand(owner: Symbol): Term

        /** A unary apply node with given argument: `tree(arg)` */
        def appliedTo(arg: Term): Term

        /** An apply node with given arguments: `tree(arg, args0, ..., argsN)` */
        def appliedTo(arg: Term, args: Term*): Term

        /** An apply node with given argument list `tree(args(0), ..., args(args.length - 1))` */
        def appliedToArgs(args: List[Term]): Apply

        /** The current tree applied to given argument lists:
        *  `tree (argss(0)) ... (argss(argss.length -1))`
        */
        def appliedToArgss(argss: List[List[Term]]): Term

        /** The current tree applied to (): `tree()` */
        def appliedToNone: Apply

        /** The current tree applied to given type argument: `tree[targ]` */
        def appliedToType(targ: TypeRepr): Term

        /** The current tree applied to given type arguments: `tree[targ0, ..., targN]` */
        def appliedToTypes(targs: List[TypeRepr]): Term

        /** The current tree applied to given type argument list: `tree[targs(0), ..., targs(targs.length - 1)]` */
        def appliedToTypeTrees(targs: List[TypeTree]): Term

        /** A select node that selects the given symbol. */
        def select(sym: Symbol): Select

      end extension

    }

    /** Tree representing a reference to definition */
    type Ref <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Ref` */
    given RefTypeTest: TypeTest[Tree, Ref]

    /** Module object of `type Ref`  */
    val Ref: RefModule

    /** Methods of the module object `val Ref` */
    trait RefModule { this: Ref.type =>

      /** A tree representing the same reference as the given type */
      def term(tp: TermRef): Ref

      /** Create a reference tree from a symbol
      *
      *  If `sym` refers to a class member `foo` in class `C`,
      *  returns a tree representing `C.this.foo`.
      *
      *  If `sym` refers to a local definition `foo`, returns
      *  a tree representing `foo`.
      *
      *  @note In both cases, the constructed tree should only
      *  be spliced into the places where such accesses make sense.
      *  For example, it is incorrect to have `C.this.foo` outside
      *  the class body of `C`, or have `foo` outside the lexical
      *  scope for the definition of `foo`.
      */
      def apply(sym: Symbol): Ref
    }

    /** Tree representing a reference to definition with a given name */
    type Ident <: Ref

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Ident` */
    given IdentTypeTest: TypeTest[Tree, Ident]

    /** Module object of `type Ident`  */
    val Ident: IdentModule

    /** Methods of the module object `val Ident` */
    trait IdentModule { this: Ident.type =>
      def apply(tmref: TermRef): Term

      def copy(original: Tree)(name: String): Ident

      /** Matches a term identifier and returns its name */
      def unapply(tree: Ident): Some[String]
    }

    /** Makes extension methods on `Ident` available without any imports */
    given IdentMethods: IdentMethods

    /** Extension methods of `Ident` */
    trait IdentMethods:
      extension (self: Ident)
        /** Name of this `Ident` */
        def name: String
      end extension
    end IdentMethods

    /** Pattern representing a `_` wildcard. */
    type Wildcard <: Ident

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Wildcard` */
    given WildcardTypeTest: TypeTest[Tree, Wildcard]

    /** Module object of `type Wildcard`  */
    val Wildcard: WildcardModule

    /** Methods of the module object `val Wildcard` */
    trait WildcardModule { this: Wildcard.type =>
      /** Create a tree representing a `_` wildcard. */
      def apply(): Wildcard
      /** Match a tree representing a `_` wildcard. */
      def unapply(wildcard: Wildcard): true
    }

    /** Tree representing a selection of definition with a given name on a given prefix */
    type Select <: Ref

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Select` */
    given SelectTypeTest: TypeTest[Tree, Select]

    /** Module object of `type Select`  */
    val Select: SelectModule

    /** Methods of the module object `val Select` */
    trait SelectModule { this: Select.type =>
      /** Select a term member by symbol */
      def apply(qualifier: Term, symbol: Symbol): Select

      /** Select a field or a non-overloaded method by name
      *
      *  @note The method will produce an assertion error if the selected
      *        method is overloaded. The method `overloaded` should be used
      *        in that case.
      */
      def unique(qualifier: Term, name: String): Select

      /** Call an overloaded method with the given type and term parameters */
      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term]): Term

      /** Call an overloaded method with the given type and term parameters */
      def overloaded(qualifier: Term, name: String, targs: List[TypeRepr], args: List[Term], returnType: TypeRepr): Term

      def copy(original: Tree)(qualifier: Term, name: String): Select

      /** Matches `<qualifier: Term>.<name: String>` */
      def unapply(x: Select): (Term, String)
    }

    /** Makes extension methods on `Select` available without any imports */
    given SelectMethods: SelectMethods

    /** Extension methods of `Select` */
    trait SelectMethods:
      extension (self: Select)
        /** Qualifier of the `qualifier.name` */
        def qualifier: Term
        /** Name of this `Select` */
        def name: String
        /** Signature of this method */
        def signature: Option[Signature]
      end extension
    end SelectMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Literal` */
    given LiteralTypeTest: TypeTest[Tree, Literal]

    /** Tree representing a literal value in the source code */
    type Literal <: Term

    /** Module object of `type Literal`  */
    val Literal: LiteralModule

    /** Methods of the module object `val Literal` */
    trait LiteralModule { this: Literal.type =>

      /** Create a literal constant */
      def apply(constant: Constant): Literal

      def copy(original: Tree)(constant: Constant): Literal

      /** Matches a literal constant */
      def unapply(x: Literal): Some[Constant]
    }

    /** Makes extension methods on `Literal` available without any imports */
    given LiteralMethods: LiteralMethods

    /** Extension methods of `Literal` */
    trait LiteralMethods:
      extension (self: Literal)
        /** Value of this literal */
        def constant: Constant
      end extension
    end LiteralMethods

    /** Tree representing `this` or `C.this` in the source code */
    type This <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `This` */
    given ThisTypeTest: TypeTest[Tree, This]

    /** Module object of `type This`  */
    val This: ThisModule

    /** Methods of the module object `val This` */
    trait ThisModule { this: This.type =>

      /** Create a `C.this` for `C` pointing to `cls` */
      def apply(cls: Symbol): This

      def copy(original: Tree)(qual: Option[String]): This

      /** Matches `this` or `qual.this` and returns the name of `qual` */
      def unapply(x: This): Some[Option[String]]
    }

    /** Makes extension methods on `This` available without any imports */
    given ThisMethods: ThisMethods

    /** Extension methods of `This` */
    trait ThisMethods:
      extension (self: This)
        /** Returns `C` if the underlying tree is of the form `C.this`
         *
         *  Otherwise, return `None`.
         */
        def id: Option[String]
      end extension
    end ThisMethods

    /** Tree representing `new` in the source code */
    type New <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `New` */
    given NewTypeTest: TypeTest[Tree, New]

    /** Module object of `type New`  */
    val New: NewModule

    /** Methods of the module object `val New` */
    trait NewModule { this: New.type =>

      /** Create a `new <tpt: TypeTree>` */
      def apply(tpt: TypeTree): New

      def copy(original: Tree)(tpt: TypeTree): New

      /** Matches `new <tpt: TypeTree>` */
      def unapply(x: New): Some[TypeTree]
    }

    /** Makes extension methods on `New` available without any imports */
    given NewMethods: NewMethods

    /** Extension methods of `New` */
    trait NewMethods:
      extension (self: New)
        /** Returns the type tree of this `new` */
        def tpt: TypeTree
      end extension
    end NewMethods

    /** Tree representing an argument passed with an explicit name. Such as `arg1 = x` in `foo(arg1 = x)` */
    type NamedArg <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `NamedArg` */
    given NamedArgTypeTest: TypeTest[Tree, NamedArg]

    /** Module object of `type NamedArg`  */
    val NamedArg: NamedArgModule

    /** Methods of the module object `val NamedArg` */
    trait NamedArgModule { this: NamedArg.type =>

      /** Create a named argument `<name: String> = <value: Term>` */
      def apply(name: String, arg: Term): NamedArg

      def copy(original: Tree)(name: String, arg: Term): NamedArg

      /** Matches a named argument `<name: String> = <value: Term>` */
      def unapply(x: NamedArg): (String, Term)
    }

    /** Makes extension methods on `NamedArg` available without any imports */
    given NamedArgMethods: NamedArgMethods

    /** Extension methods of `NamedArg` */
    trait NamedArgMethods:
      extension (self: NamedArg)
        /** The name part of `name = arg` */
        def name: String
        /** The argument part of `name = arg` */
        def value: Term
      end extension
    end NamedArgMethods

    /** Tree representing an application of arguments.
     *  It represents a single list of arguments, multiple argument lists will have nested `Apply`s
     */
    type Apply <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Apply` */
    given ApplyTypeTest: TypeTest[Tree, Apply]

    /** Module object of `type Apply`  */
    val Apply: ApplyModule

    /** Methods of the module object `val Apply` */
    trait ApplyModule { this: Apply.type =>

      /** Create a function application `<fun: Term>(<args: List[Term]>)` */
      def apply(fun: Term, args: List[Term]): Apply

      def copy(original: Tree)(fun: Term, args: List[Term]): Apply

      /** Matches a function application `<fun: Term>(<args: List[Term]>)` */
      def unapply(x: Apply): (Term, List[Term])
    }

    /** Makes extension methods on `Apply` available without any imports */
    given ApplyMethods: ApplyMethods

    /** Extension methods of `Apply` */
    trait ApplyMethods:
      extension (self: Apply)
        /** The `fun` part of an (implicit) application like `fun(args)`
         *
         *  It may be a partially applied method:
         *  ```scala
         *  def f(x1: Int)(x2: Int) = ???
         *  f(1)(2)
         *  ```
         *  - `fun` is `f(1)` in the `Apply` of `f(1)(2)`
         *  - `fun` is `f` in the `Apply` of `f(1)`
         */
        def fun: Term
        /** The arguments (implicitly) passed to the method
         *
         *  The `Apply` may be a partially applied method:
         *  ```scala
         *  def f(x1: Int)(x2: Int) = ???
         *  f(1)(2)
         *  ```
         *  - `args` is `(2)` in the `Apply` of `f(1)(2)`
         *  - `args` is `(1)` in the `Apply` of `f(1)`
         */
        def args: List[Term]
      end extension
    end ApplyMethods

    /** Tree representing an application of type arguments */
    type TypeApply <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeApply` */
    given TypeApplyTypeTest: TypeTest[Tree, TypeApply]

    /** Module object of `type TypeApply`  */
    val TypeApply: TypeApplyModule

    /** Methods of the module object `val TypeApply` */
    trait TypeApplyModule { this: TypeApply.type =>

      /** Create a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def apply(fun: Term, args: List[TypeTree]): TypeApply

      def copy(original: Tree)(fun: Term, args: List[TypeTree]): TypeApply

      /** Matches a function type application `<fun: Term>[<args: List[TypeTree]>]` */
      def unapply(x: TypeApply): (Term, List[TypeTree])
    }

    /** Makes extension methods on `TypeApply` available without any imports */
    given TypeApplyMethods: TypeApplyMethods

    /** Extension methods of `TypeApply` */
    trait TypeApplyMethods:
      extension (self: TypeApply)
        /** The `fun` part of an (inferred) type application like `fun[Args]`
         *
         *  It may be a partially applied method:
         *  ```scala
         *  //{
         *  type T
         *  //}
         *  extension (x: Int) def f[T](y: T) = ???
         *  // represented as
         *  // def f(x: Int)[T](y: T) = ???
         *
         *  1.f[Int](2)
         *  // represented as
         *  // f(1)[Int](2)
         *  ```
         *  - `fun` is `f(1)` in the `TypeApply` of `f(1)[Int]`
         */
        def fun: Term
        /** The (inferred) type arguments passed to the method
         *
         *  The `TypeApply` may be a partially applied method:
         *  ```scala
         *  //{
         *  type T
         *  //}
         *  extension (x: Int) def f[T](y: T) = ???
         *  // represented as
         *  // def f(x: Int)[T](y: T) = ???
         *
         *  1.f[Int](2)
         *  // represented as
         *  // f(1)[Int](2)
         *  ```
         *  - `fun` is `[Int]` in the `TypeApply` of `f(1)[Int]`
         */
        def args: List[TypeTree]
      end extension
    end TypeApplyMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Super` */
    given SuperTypeTest: TypeTest[Tree, Super]

    /** Tree representing `super` in the source code */
    type Super <: Term

    /** Module object of `type Super`  */
    val Super: SuperModule

    /** Methods of the module object `val Super` */
    trait SuperModule { this: Super.type =>

      /** Creates a `<qualifier: Term>.super[<id: Option[Id]>` */
      def apply(qual: Term, mix: Option[String]): Super

      def copy(original: Tree)(qual: Term, mix: Option[String]): Super

      /** Matches a `<qualifier: Term>.super[<id: Option[Id]>` */
      def unapply(x: Super): (Term, Option[String])
    }

    /** Makes extension methods on `Super` available without any imports */
    given SuperMethods: SuperMethods

    /** Extension methods of `Super` */
    trait SuperMethods:
      extension (self: Super)
        def qualifier: Term
        def id: Option[String]
        def idPos: Position
      end extension
    end SuperMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Typed` */
    given TypedTypeTest: TypeTest[Tree, Typed]

    /** Tree representing a type ascription `x: T` in the source code.
     *
     *  Also represents a pattern that contains a term `x`.
     *  Other `: T` patterns use the more general `TypedOrTest`.
     */
    type Typed <: Term & TypedOrTest

    /** Module object of `type Typed`  */
    val Typed: TypedModule

    /** Methods of the module object `val Typed` */
    trait TypedModule { this: Typed.type =>

      /** Create a type ascription `<x: Term>: <tpt: TypeTree>` */
      def apply(expr: Term, tpt: TypeTree): Typed

      def copy(original: Tree)(expr: Term, tpt: TypeTree): Typed

      /** Matches `<expr: Term>: <tpt: TypeTree>` */
      def unapply(x: Typed): (Term, TypeTree)
    }

    /** Makes extension methods on `Typed` available without any imports */
    given TypedMethods: TypedMethods

    /** Extension methods of `Typed` */
    trait TypedMethods:
      extension (self: Typed)
        def expr: Term
        def tpt: TypeTree
      end extension
    end TypedMethods

    /** Tree representing an assignment `x = y` in the source code */
    type Assign <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Assign` */
    given AssignTypeTest: TypeTest[Tree, Assign]

    /** Module object of `type Assign`  */
    val Assign: AssignModule

    /** Methods of the module object `val Assign` */
    trait AssignModule { this: Assign.type =>

      /** Create an assignment `<lhs: Term> = <rhs: Term>` */
      def apply(lhs: Term, rhs: Term): Assign

      def copy(original: Tree)(lhs: Term, rhs: Term): Assign

      /** Matches an assignment `<lhs: Term> = <rhs: Term>` */
      def unapply(x: Assign): (Term, Term)
    }

    /** Makes extension methods on `Assign` available without any imports */
    given AssignMethods: AssignMethods

    /** Extension methods of `Assign` */
    trait AssignMethods:
      extension (self: Assign)
        def lhs: Term
        def rhs: Term
      end extension
    end AssignMethods

    /** Tree representing a block `{ ... }` in the source code */
    type Block <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Block` */
    given BlockTypeTest: TypeTest[Tree, Block]

    /** Module object of `type Block`  */
    val Block: BlockModule

    /** Methods of the module object `val Block` */
    trait BlockModule { this: Block.type =>

      /** Creates a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def apply(stats: List[Statement], expr: Term): Block

      def copy(original: Tree)(stats: List[Statement], expr: Term): Block

      /** Matches a block `{ <statements: List[Statement]>; <expr: Term> }` */
      def unapply(x: Block): (List[Statement], Term)
    }

    /** Makes extension methods on `Block` available without any imports */
    given BlockMethods: BlockMethods

    /** Extension methods of `Block` */
    trait BlockMethods:
      extension (self: Block)
        def statements: List[Statement]
        def expr: Term
      end extension
    end BlockMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Closure` */
    given ClosureTypeTest: TypeTest[Tree, Closure]

    /** A lambda `(...) => ...` in the source code is represented as
     *  a local method and a closure:
     *
     *  {
     *    def m(...) = ...
     *    closure(m)
     *  }
     *
     */
    type Closure <: Term

    /** Module object of `type Closure`  */
    val Closure: ClosureModule

    /** Methods of the module object `val Closure` */
    trait ClosureModule { this: Closure.type =>

      def apply(meth: Term, tpe: Option[TypeRepr]): Closure

      def copy(original: Tree)(meth: Tree, tpe: Option[TypeRepr]): Closure

      def unapply(x: Closure): (Term, Option[TypeRepr])
    }

    /** Makes extension methods on `Closure` available without any imports */
    given ClosureMethods: ClosureMethods

    /** Extension methods of `Closure` */
    trait ClosureMethods:
      extension (self: Closure)
        def meth: Term
        def tpeOpt: Option[TypeRepr]
      end extension
    end ClosureMethods

    /** A lambda `(...) => ...` in the source code is represented as
     *  a local method and a closure:
     *
     *  ```scala sc:nocompile
     *  {
     *    def m(...) = ...
     *    closure(m)
     *  }
     *  ```
     *  @note Due to the encoding, in pattern matches the case for `Lambda`
     *        should come before the case for `Block` to avoid mishandling
     *        of `Lambda`.
     */
    val Lambda: LambdaModule

    /** Methods of the module object `val Lambda` */
    trait LambdaModule { this: Lambda.type =>
      /** Matches a lambda definition of the form
       *  ```scala sc:nocompile
       *  Block((DefDef(_, _, params :: Nil, _, Some(body))) :: Nil, Closure(meth, _))
       *  ```
       *  Extracts the parameter definitions and body.
       */
      def unapply(tree: Block): Option[(List[ValDef], Term)]

      /** Generates a lambda with the given method type.
       *  ```scala sc:nocompile
       *  Block((DefDef(_, _, params :: Nil, _, Some(rhsFn(meth, paramRefs)))) :: Nil, Closure(meth, _))
       *  ```
       *
       * Usage:
       *  ```
       *  val mtpe = MethodType(List("arg1"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Int])
       *  Lambda(owner, mtpe, {
       *    case (methSym, List(arg1: Term)) =>
       *      ValDef.let(methSym, f(arg1)) { ... }
       *    }
       *  )
       *  ```
       *
       *  Usage with quotes:
       *  ```
       *  val mtpe = MethodType(List("arg1"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Int])
       *  Lambda(owner, mtpe, {
       *    case (methSym, List(arg1: Term)) =>
       *      given Quotes = methSym.asQuotes
       *      '{ ... }
       *    }
       *  )
       *  ```
       *
       *  @param owner owner of the generated `meth` symbol
       *  @param tpe Type of the definition
       *  @param rhsFn Function that receives the `meth` symbol and the a list of references to the `params`
       */
      def apply(owner: Symbol, tpe: MethodType, rhsFn: (Symbol, List[Tree]) => Tree): Block
    }

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `If` */
    given IfTypeTest: TypeTest[Tree, If]

    /** Tree representing an if/then/else `if (...) ... else ...` in the source code */
    type If <: Term

    /** Module object of `type If` */
    val If: IfModule

    /** Methods of the module object `val If` */
    trait IfModule { this: If.type =>

      /** Create an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def apply(cond: Term, thenp: Term, elsep: Term): If

      def copy(original: Tree)(cond: Term, thenp: Term, elsep: Term): If

      /** Matches an if/then/else `if (<cond: Term>) <thenp: Term> else <elsep: Term>` */
      def unapply(tree: If): (Term, Term, Term)
    }

    /** Makes extension methods on `If` available without any imports */
    given IfMethods: IfMethods

    /** Extension methods of `If` */
    trait IfMethods:
      extension (self: If)
        def cond: Term
        def thenp: Term
        def elsep: Term
        def isInline: Boolean
      end extension
    end IfMethods

    /** Tree representing a pattern match `x match  { ... }` in the source code */
    type Match <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Match` */
    given MatchTypeTest: TypeTest[Tree, Match]

    /** Module object of `type Match`  */
    val Match: MatchModule

    /** Methods of the module object `val Match` */
    trait MatchModule { this: Match.type =>

      /** Creates a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def apply(selector: Term, cases: List[CaseDef]): Match

      def copy(original: Tree)(selector: Term, cases: List[CaseDef]): Match

      /** Matches a pattern match `<scrutinee: Term> match { <cases: List[CaseDef]> }` */
      def unapply(x: Match): (Term, List[CaseDef])
    }

    /** Makes extension methods on `Match` available without any imports */
    given MatchMethods: MatchMethods

    /** Extension methods of `Match` */
    trait MatchMethods:
      extension (self: Match)
        def scrutinee: Term
        def cases: List[CaseDef]
        def isInline: Boolean
      end extension
    end MatchMethods

    /** Tree representing a summoning match `summonFrom { ... }` in the source code */
    type SummonFrom <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `SummonFrom` */
    given SummonFromTypeTest: TypeTest[Tree, SummonFrom]

    /** Module object of `type SummonFrom`  */
    val SummonFrom: SummonFromModule

    /** Methods of the module object `val SummonFrom` */
    trait SummonFromModule { this: SummonFrom.type =>

      /** Creates a pattern match `given match { <cases: List[CaseDef]> }` */
      def apply(cases: List[CaseDef]): SummonFrom

      def copy(original: Tree)(cases: List[CaseDef]): SummonFrom

      /** Matches a pattern match `given match { <cases: List[CaseDef]> }` */
      def unapply(x: SummonFrom): Some[List[CaseDef]]
    }

    /** Makes extension methods on `SummonFrom` available without any imports */
    given SummonFromMethods: SummonFromMethods

    /** Extension methods of `SummonFrom` */
    trait SummonFromMethods:
      extension (self: SummonFrom)
        def cases: List[CaseDef]
      end extension
    end SummonFromMethods

    /** Tree representing a try catch `try x catch { ... } finally { ... }` in the source code */
    type Try <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Try` */
    given TryTypeTest: TypeTest[Tree, Try]

    /** Module object of `type Try`  */
    val Try: TryModule

    /** Methods of the module object `val Try` */
    trait TryModule { this: Try.type =>

      /** Create a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def apply(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      def copy(original: Tree)(expr: Term, cases: List[CaseDef], finalizer: Option[Term]): Try

      /** Matches a try/catch `try <body: Term> catch { <cases: List[CaseDef]> } finally <finalizer: Option[Term]>` */
      def unapply(x: Try): (Term, List[CaseDef], Option[Term])
    }

    /** Makes extension methods on `Try` available without any imports */
    given TryMethods: TryMethods

    /** Extension methods of `Try` */
    trait TryMethods:
      extension (self: Try)
        def body: Term
        def cases: List[CaseDef]
        def finalizer: Option[Term]
      end extension
    end TryMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Return` */
    given ReturnTypeTest: TypeTest[Tree, Return]

    /** Tree representing a `return` in the source code */
    type Return <: Term

    /** Module object of `type Return`  */
    val Return: ReturnModule

    /** Methods of the module object `val Return` */
    trait ReturnModule { this: Return.type =>

      /** Creates `return <expr: Term>` */
      def apply(expr: Term, from: Symbol): Return

      def copy(original: Tree)(expr: Term, from: Symbol): Return

      /** Matches `return <expr: Term>` and extracts the expression and symbol of the method */
      def unapply(x: Return): (Term, Symbol)
    }

    /** Makes extension methods on `Return` available without any imports */
    given ReturnMethods: ReturnMethods

    /** Extension methods of `Return` */
    trait ReturnMethods:
      extension (self: Return)
        def expr: Term
        def from: Symbol
      end extension
    end ReturnMethods

    /** Tree representing a variable argument list in the source code.
     *
     *  This tree is used to encode varargs terms. The Repeated encapsulates
     *  the sequence of the elements but needs to be wrapped in a
     *  `scala.<repeated>[T]` (see `defn.RepeatedParamClass`). For example the
     *   arguments `1, 2` of `List.apply(1, 2)` can be represented as follows:
     *
     *
     *  ```scala
     *  //{
     *  import scala.quoted._
     *  def inQuotes(using Quotes) = {
     *    val q: Quotes = summon[Quotes]
     *    import q.reflect._
     *  //}
     *    val intArgs = List(Literal(IntConstant(1)), Literal(IntConstant(2)))
     *    Typed(
     *      Repeated(intArgs, TypeTree.of[Int]),
     *      Inferred(defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Int]))
     *    )
     *  //{
     *  }
     *  //}
     *  ```
     */
    type Repeated <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Repeated` */
    given RepeatedTypeTest: TypeTest[Tree, Repeated]

    /** Module object of `type Repeated`  */
    val Repeated: RepeatedModule

    /** Methods of the module object `val Repeated` */
    trait RepeatedModule { this: Repeated.type =>
      /** Create a literal sequence of elements */
      def apply(elems: List[Term], tpt: TypeTree): Repeated
      /** Copy a literal sequence of elements */
      def copy(original: Tree)(elems: List[Term], tpt: TypeTree): Repeated
      /** Matches a literal sequence of elements */
      def unapply(x: Repeated): (List[Term], TypeTree)
    }

    /** Makes extension methods on `Repeated` available without any imports */
    given RepeatedMethods: RepeatedMethods

    /** Extension methods of `Repeated` */
    trait RepeatedMethods:
      extension (self: Repeated)
        def elems: List[Term]
        def elemtpt: TypeTree
      end extension
    end RepeatedMethods

    /** Tree representing the scope of an inlined tree */
    type Inlined <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Inlined` */
    given InlinedTypeTest: TypeTest[Tree, Inlined]

    /** Module object of `type Inlined`  */
    val Inlined: InlinedModule

    /** Methods of the module object `val Inlined` */
    trait InlinedModule { this: Inlined.type =>
      def apply(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def copy(original: Tree)(call: Option[Tree /* Term | TypeTree */], bindings: List[Definition], expansion: Term): Inlined
      def unapply(x: Inlined): (Option[Tree /* Term | TypeTree */], List[Definition], Term)
    }

    /** Makes extension methods on `Inlined` available without any imports */
    given InlinedMethods: InlinedMethods

    /** Extension methods of `Inlined` */
    trait InlinedMethods:
      extension (self: Inlined)
        def call: Option[Tree /* Term | TypeTree */]
        def bindings: List[Definition]
        def body: Term
      end extension
    end InlinedMethods

    /** Tree representing a selection of definition with a given name on a given prefix and number of nested scopes of inlined trees */
    type SelectOuter <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `SelectOuter` */
    given SelectOuterTypeTest: TypeTest[Tree, SelectOuter]

    /** Module object of `type SelectOuter`  */
    val SelectOuter: SelectOuterModule

    /** Methods of the module object `val SelectOuter` */
    trait SelectOuterModule { this: SelectOuter.type =>
      def apply(qualifier: Term, name: String, levels: Int): SelectOuter
      def copy(original: Tree)(qualifier: Term, name: String, levels: Int): SelectOuter
      def unapply(x: SelectOuter): (Term, String, Int)
    }

    /** Makes extension methods on `SelectOuter` available without any imports */
    given SelectOuterMethods: SelectOuterMethods

    /** Extension methods of `SelectOuter` */
    trait SelectOuterMethods:
      extension (self: SelectOuter)
        def qualifier: Term
        def name: String
        def level: Int
      end extension
    end SelectOuterMethods

    /** Tree representing a while loop */
    type While <: Term

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `While` */
    given WhileTypeTest: TypeTest[Tree, While]

    /** Module object of `type While`  */
    val While: WhileModule

    /** Methods of the module object `val While` */
    trait WhileModule { this: While.type =>

      /** Creates a while loop `while (<cond>) <body>` and returns (<cond>, <body>) */
      def apply(cond: Term, body: Term): While

      def copy(original: Tree)(cond: Term, body: Term): While

      /** Extractor for while loops. Matches `while (<cond>) <body>` and returns (<cond>, <body>) */
      def unapply(x: While): (Term, Term)
    }

    /** Makes extension methods on `While` available without any imports */
    given WhileMethods: WhileMethods

    /** Extension methods of `While` */
    trait WhileMethods:
      extension (self: While)
        def cond: Term
        def body: Term
      end extension
    end WhileMethods

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypedOrTest` */
    given TypedOrTestTypeTest: TypeTest[Tree, TypedOrTest]

    /** Tree representing a type ascription or type test pattern `x: T` in the source code. */
    type TypedOrTest <: Tree

    /** Module object of `type TypedOrTest`  */
    val TypedOrTest: TypedOrTestModule

    /** Methods of the module object `val TypedOrTest` */
    trait TypedOrTestModule { this: TypedOrTest.type =>

      /** Create a type ascription `<x: Tree>: <tpt: TypeTree>` */
      def apply(expr: Tree, tpt: TypeTree): TypedOrTest

      def copy(original: Tree)(expr: Tree, tpt: TypeTree): TypedOrTest

      /** Matches `<expr: Tree>: <tpt: TypeTree>` */
      def unapply(x: TypedOrTest): (Tree, TypeTree)
    }

    /** Makes extension methods on `TypedOrTest` available without any imports */
    given TypedOrTestMethods: TypedOrTestMethods

    /** Extension methods of `TypedOrTest` */
    trait TypedOrTestMethods:
      extension (self: TypedOrTest)
        def tree: Tree
        def tpt: TypeTree
      end extension
    end TypedOrTestMethods

    // ----- TypeTrees ------------------------------------------------

    /** Type tree representing a type written in the source */
    type TypeTree <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeTree` */
    given TypeTreeTypeTest: TypeTest[Tree, TypeTree]

    /** Module object of `type TypeTree`  */
    val TypeTree: TypeTreeModule

    /** Methods of the module object `val TypeTree` */
    trait TypeTreeModule { this: TypeTree.type =>
      /** Returns the tree of type or kind (TypeTree) of T */
      def of[T <: AnyKind](using Type[T]): TypeTree

      /** Returns a type tree reference to the symbol
       *
       *  @param typeSymbol The type symbol for which we are creating a type tree reference.
       */
      def ref(typeSymbol: Symbol): TypeTree
    }

    /** Makes extension methods on `TypeTree` available without any imports */
    given TypeTreeMethods: TypeTreeMethods

    /** Extension methods of `TypeTree` */
    trait TypeTreeMethods:
      extension (self: TypeTree)
        /** TypeRepr of this type tree */
        def tpe: TypeRepr
      end extension
    end TypeTreeMethods

    /** Type tree representing an inferred type */
    type Inferred <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Inferred` */
    given InferredTypeTest: TypeTest[Tree, Inferred]

    /** Module object of `type Inferred`  */
    val Inferred: InferredModule

    /** Methods of the module object `val Inferred` */
    trait InferredModule { this: Inferred.type =>
      def apply(tpe: TypeRepr): Inferred
      /** Matches a TypeTree containing an inferred type */
      def unapply(x: Inferred): true
    }

    /** Type tree representing a reference to definition with a given name */
    type TypeIdent <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeIdent` */
    given TypeIdentTypeTest: TypeTest[Tree, TypeIdent]

    /** Module object of `type TypeIdent`  */
    val TypeIdent: TypeIdentModule

    /** Methods of the module object `val TypeIdent` */
    trait TypeIdentModule { this: TypeIdent.type =>
      def apply(sym: Symbol): TypeTree
      def copy(original: Tree)(name: String): TypeIdent
      def unapply(x: TypeIdent): Some[String]
    }

    /** Makes extension methods on `TypeIdent` available without any imports */
    given TypeIdentMethods: TypeIdentMethods

    /** Extension methods of `TypeIdent` */
    trait TypeIdentMethods:
      extension (self: TypeIdent)
        def name: String
      end extension
    end TypeIdentMethods

    /** Type tree representing a selection of definition with a given name on a given term prefix */
    type TypeSelect <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeSelect` */
    given TypeSelectTypeTest: TypeTest[Tree, TypeSelect]

    /** Module object of `type TypeSelect`  */
    val TypeSelect: TypeSelectModule

    /** Methods of the module object `val TypeSelect` */
    trait TypeSelectModule { this: TypeSelect.type =>
      def apply(qualifier: Term, name: String): TypeSelect
      def copy(original: Tree)(qualifier: Term, name: String): TypeSelect
      def unapply(x: TypeSelect): (Term, String)
    }

    /** Makes extension methods on `TypeSelect` available without any imports */
    given TypeSelectMethods: TypeSelectMethods

    /** Extension methods of `TypeSelect` */
    trait TypeSelectMethods:
      extension (self: TypeSelect)
        def qualifier: Term
        def name: String
      end extension
    end TypeSelectMethods

    /** Type tree representing a selection of definition with a given name on a given type prefix */
    type TypeProjection <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeProjection` */
    given TypeProjectionTypeTest: TypeTest[Tree, TypeProjection]

    /** Module object of `type TypeProjection`  */
    val TypeProjection: TypeProjectionModule

    /** Methods of the module object `val TypeProjection` */
    trait TypeProjectionModule { this: TypeProjection.type =>
      def copy(original: Tree)(qualifier: TypeTree, name: String): TypeProjection
      def unapply(x: TypeProjection): (TypeTree, String)
    }

    /** Makes extension methods on `TypeProjection` available without any imports */
    given TypeProjectionMethods: TypeProjectionMethods

    /** Extension methods of `TypeProjection` */
    trait TypeProjectionMethods:
      extension (self: TypeProjection)
        def qualifier: TypeTree
        def name: String
      end extension
    end TypeProjectionMethods

    /** Type tree representing a singleton type */
    type Singleton <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Singleton` */
    given SingletonTypeTest: TypeTest[Tree, Singleton]

    /** Module object of `type Singleton`  */
    val Singleton: SingletonModule

    /** Methods of the module object `val Singleton` */
    trait SingletonModule { this: Singleton.type =>
      def apply(ref: Term): Singleton
      def copy(original: Tree)(ref: Term): Singleton
      def unapply(x: Singleton): Some[Term]
    }

    /** Makes extension methods on `Singleton` available without any imports */
    given SingletonMethods: SingletonMethods

    /** Extension methods of `Singleton` */
    trait SingletonMethods:
      extension (self: Singleton)
        def ref: Term
      end extension
    end SingletonMethods

    /** Type tree representing a type refinement */
    type Refined <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Refined` */
    given RefinedTypeTest: TypeTest[Tree, Refined]

    /** Module object of `type Refined`  */
    val Refined: RefinedModule

    /** Methods of the module object `val Refined` */
    trait RefinedModule { this: Refined.type =>
      def copy(original: Tree)(tpt: TypeTree, refinements: List[Definition]): Refined
      def unapply(x: Refined): (TypeTree, List[Definition])
    }

    /** Makes extension methods on `Refined` available without any imports */
    given RefinedMethods: RefinedMethods

    /** Extension methods of `Refined` */
    trait RefinedMethods:
      extension (self: Refined)
        def tpt: TypeTree
        def refinements: List[Definition]
      end extension
    end RefinedMethods

    /** Type tree representing a type application */
    type Applied <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Applied` */
    given AppliedTypeTest: TypeTest[Tree, Applied]

    /** Module object of `type Applied`  */
    val Applied: AppliedModule

    /** Methods of the module object `val Applied` */
    trait AppliedModule { this: Applied.type =>
      def apply(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def copy(original: Tree)(tpt: TypeTree, args: List[Tree /*TypeTree | TypeBoundsTree*/]): Applied
      def unapply(x: Applied): (TypeTree, List[Tree /*TypeTree | TypeBoundsTree*/])
    }

    /** Makes extension methods on `Applied` available without any imports */
    given AppliedMethods: AppliedMethods

    /** Extension methods of `Applied` */
    trait AppliedMethods:
      extension (self: Applied)
        def tpt: TypeTree
        def args: List[Tree /*TypeTree | TypeBoundsTree*/]
      end extension
    end AppliedMethods

    /** Type tree representing an annotated type */
    type Annotated <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Annotated` */
    given AnnotatedTypeTest: TypeTest[Tree, Annotated]

    /** Module object of `type Annotated`  */
    val Annotated: AnnotatedModule

    /** Methods of the module object `val Annotated` */
    trait AnnotatedModule { this: Annotated.type =>
      def apply(arg: TypeTree, annotation: Term): Annotated
      def copy(original: Tree)(arg: TypeTree, annotation: Term): Annotated
      def unapply(x: Annotated): (TypeTree, Term)
    }

    /** Makes extension methods on `Annotated` available without any imports */
    given AnnotatedMethods: AnnotatedMethods

    /** Extension methods of `Annotated` */
    trait AnnotatedMethods:
      extension (self: Annotated)
        def arg: TypeTree
        def annotation: Term
      end extension
    end AnnotatedMethods

    /** Type tree representing a type match */
    type MatchTypeTree <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `MatchTypeTree` */
    given MatchTypeTreeTypeTest: TypeTest[Tree, MatchTypeTree]

    /** Module object of `type MatchTypeTree`  */
    val MatchTypeTree: MatchTypeTreeModule

    /** Methods of the module object `val MatchTypeTree` */
    trait MatchTypeTreeModule { this: MatchTypeTree.type =>
      def apply(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def copy(original: Tree)(bound: Option[TypeTree], selector: TypeTree, cases: List[TypeCaseDef]): MatchTypeTree
      def unapply(x: MatchTypeTree): (Option[TypeTree], TypeTree, List[TypeCaseDef])
    }

    /** Makes extension methods on `MatchTypeTree` available without any imports */
    given MatchTypeTreeMethods: MatchTypeTreeMethods

    /** Extension methods of `MatchTypeTree` */
    trait MatchTypeTreeMethods:
      extension (self: MatchTypeTree)
        def bound: Option[TypeTree]
        def selector: TypeTree
        def cases: List[TypeCaseDef]
      end extension
    end MatchTypeTreeMethods

    /** Type tree representing a by name parameter */
    type ByName <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `ByName` */
    given ByNameTypeTest: TypeTest[Tree, ByName]

    /** Module object of `type ByName`  */
    val ByName: ByNameModule

    /** Methods of the module object `val ByName` */
    trait ByNameModule { this: ByName.type =>
      def apply(result: TypeTree): ByName
      def copy(original: Tree)(result: TypeTree): ByName
      def unapply(x: ByName): Some[TypeTree]
    }

    /** Makes extension methods on `ByName` available without any imports */
    given ByNameMethods: ByNameMethods

    /** Extension methods of `ByName` */
    trait ByNameMethods:
      extension (self: ByName)
        def result: TypeTree
      end extension
    end ByNameMethods

    /** Type tree representing a lambda abstraction type */
    type LambdaTypeTree <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `LambdaTypeTree` */
    given LambdaTypeTreeTypeTest: TypeTest[Tree, LambdaTypeTree]

    /** Module object of `type LambdaTypeTree`  */
    val LambdaTypeTree: LambdaTypeTreeModule

    /** Methods of the module object `val LambdaTypeTree` */
    trait LambdaTypeTreeModule { this: LambdaTypeTree.type =>
      def apply(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def copy(original: Tree)(tparams: List[TypeDef], body: Tree /*TypeTree | TypeBoundsTree*/): LambdaTypeTree
      def unapply(tree: LambdaTypeTree): (List[TypeDef], Tree /*TypeTree | TypeBoundsTree*/)
    }

    /** Makes extension methods on `LambdaTypeTree` available without any imports */
    given LambdaTypeTreeMethods: LambdaTypeTreeMethods

    /** Extension methods of `LambdaTypeTree` */
    trait LambdaTypeTreeMethods:
      extension (self: LambdaTypeTree)
        def tparams: List[TypeDef]
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end LambdaTypeTreeMethods

    /** Type tree representing a type binding */
    type TypeBind <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeBind` */
    given TypeBindTypeTest: TypeTest[Tree, TypeBind]

    /** Module object of `type TypeBind`  */
    val TypeBind: TypeBindModule

    /** Methods of the module object `val TypeBind` */
    trait TypeBindModule { this: TypeBind.type =>
      def copy(original: Tree)(name: String, tpt: Tree /*TypeTree | TypeBoundsTree*/): TypeBind
      def unapply(x: TypeBind): (String, Tree /*TypeTree | TypeBoundsTree*/)
    }

    /** Makes extension methods on `TypeBind` available without any imports */
    given TypeBindMethods: TypeBindMethods

    /** Extension methods of `TypeBind` */
    trait TypeBindMethods:
      extension (self: TypeBind)
        def name: String
        def body: Tree /*TypeTree | TypeBoundsTree*/
      end extension
    end TypeBindMethods

    /** Type tree within a block with aliases `{ type U1 = ... ; T[U1, U2] }` */
    type TypeBlock <: TypeTree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeBlock` */
    given TypeBlockTypeTest: TypeTest[Tree, TypeBlock]

    /** Module object of `type TypeBlock`  */
    val TypeBlock: TypeBlockModule

    /** Methods of the module object `val TypeBlock` */
    trait TypeBlockModule { this: TypeBlock.type =>
      def apply(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def copy(original: Tree)(aliases: List[TypeDef], tpt: TypeTree): TypeBlock
      def unapply(x: TypeBlock): (List[TypeDef], TypeTree)
    }

    /** Makes extension methods on `TypeBlock` available without any imports */
    given TypeBlockMethods: TypeBlockMethods

    /** Extension methods of `TypeBlock` */
    trait TypeBlockMethods:
      extension (self: TypeBlock)
        def aliases: List[TypeDef]
        def tpt: TypeTree
      end extension
    end TypeBlockMethods

    // ----- TypeBoundsTrees ------------------------------------------------

    /** Type tree representing a type bound written in the source */
    type TypeBoundsTree <: Tree /*TypeTree | TypeBoundsTree*/

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeBoundsTree` */
    given TypeBoundsTreeTypeTest: TypeTest[Tree, TypeBoundsTree]

    /** Module object of `type TypeBoundsTree`  */
    val TypeBoundsTree: TypeBoundsTreeModule

    /** Methods of the module object `val TypeBoundsTree` */
    trait TypeBoundsTreeModule { this: TypeBoundsTree.type =>
      def apply(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def copy(original: Tree)(low: TypeTree, hi: TypeTree): TypeBoundsTree
      def unapply(x: TypeBoundsTree): (TypeTree, TypeTree)
    }

    /** Makes extension methods on `TypeBoundsTree` available without any imports */
    given TypeBoundsTreeMethods: TypeBoundsTreeMethods

    /** Extension methods of `TypeBoundsTree` */
    trait TypeBoundsTreeMethods:
      extension (self: TypeBoundsTree)
        def tpe: TypeBounds
        def low: TypeTree
        def hi: TypeTree
      end extension
    end TypeBoundsTreeMethods

    /** Type tree representing wildcard type bounds written in the source.
    *  The wildcard type `_` (for example in in `List[_]`) will be a type tree that
    *  represents a type but has `TypeBounds` inside.
    */
    type WildcardTypeTree <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `WildcardTypeTree` */
    given WildcardTypeTreeTypeTest: TypeTest[Tree, WildcardTypeTree]

    /** Module object of `type WildcardTypeTree`  */
    val WildcardTypeTree: WildcardTypeTreeModule

    /** Methods of the module object `val WildcardTypeTree` */
    trait WildcardTypeTreeModule { this: WildcardTypeTree.type =>
      def apply(tpe: TypeRepr): WildcardTypeTree
      /** Matches a TypeBoundsTree containing wildcard type bounds */
      def unapply(x: WildcardTypeTree): true
    }

    /** Makes extension methods on `WildcardTypeTree` available without any imports */
    given WildcardTypeTreeMethods: WildcardTypeTreeMethods

    /** Extension methods of `WildcardTypeTree` */
    trait WildcardTypeTreeMethods:
      extension (self: WildcardTypeTree)
        def tpe: TypeRepr
      end extension
    end WildcardTypeTreeMethods

    // ----- CaseDefs ------------------------------------------------

    /** Branch of a pattern match or catch clause */
    type CaseDef <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `CaseDef` */
    given CaseDefTypeTest: TypeTest[Tree, CaseDef]

    /** Module object of `type CaseDef`  */
    val CaseDef: CaseDefModule

    /** Methods of the module object `val CaseDef` */
    trait CaseDefModule { this: CaseDef.type =>
      def apply(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def copy(original: Tree)(pattern: Tree, guard: Option[Term], rhs: Term): CaseDef
      def unapply(x: CaseDef): (Tree, Option[Term], Term)
    }

    /** Makes extension methods on `CaseDef` available without any imports */
    given CaseDefMethods: CaseDefMethods

    /** Extension methods of `CaseDef` */
    trait CaseDefMethods:
      extension (self: CaseDef)
        def pattern: Tree
        def guard: Option[Term]
        def rhs: Term
      end extension
    end CaseDefMethods

    /** Branch of a type pattern match */
    type TypeCaseDef <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `TypeCaseDef` */
    given TypeCaseDefTypeTest: TypeTest[Tree, TypeCaseDef]

    /** Module object of `type TypeCaseDef`  */
    val TypeCaseDef: TypeCaseDefModule

    /** Methods of the module object `val TypeCaseDef` */
    trait TypeCaseDefModule { this: TypeCaseDef.type =>
      def apply(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def copy(original: Tree)(pattern: TypeTree, rhs: TypeTree): TypeCaseDef
      def unapply(tree: TypeCaseDef): (TypeTree, TypeTree)
    }

    /** Makes extension methods on `TypeCaseDef` available without any imports */
    given TypeCaseDefMethods: TypeCaseDefMethods

    /** Extension methods of `TypeCaseDef` */
    trait TypeCaseDefMethods:
      extension (self: TypeCaseDef)
        def pattern: TypeTree
        def rhs: TypeTree
      end extension
    end TypeCaseDefMethods

    // ----- Patterns ------------------------------------------------

    /** Pattern representing a `_ @ _` binding. */
    type Bind <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is a `Bind` */
    given BindTypeTest: TypeTest[Tree, Bind]

    /** Module object of `type Bind`  */
    val Bind: BindModule

    /** Methods of the module object `val Bind` */
    trait BindModule { this: Bind.type =>
      def apply(sym: Symbol, pattern: Tree): Bind
      def copy(original: Tree)(name: String, pattern: Tree): Bind
      def unapply(pattern: Bind): (String, Tree)
    }

    /** Makes extension methods on `Bind` available without any imports */
    given BindMethods: BindMethods

    /** Extension methods of `Bind` */
    trait BindMethods:
      extension (self: Bind)
        def name: String
        def pattern: Tree
      end extension
    end BindMethods

    /** Pattern representing a `Xyz(...)` unapply. */
    type Unapply <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Unapply` */
    given UnapplyTypeTest: TypeTest[Tree, Unapply]

    /** Module object of `type Unapply`  */
    val Unapply: UnapplyModule

    /** Methods of the module object `val Unapply` */
    trait UnapplyModule { this: Unapply.type =>
      /** Create an `Unapply` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)` */
      def apply(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
      /** Copy an `Unapply` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)` */
      def copy(original: Tree)(fun: Term, implicits: List[Term], patterns: List[Tree]): Unapply
      /** Matches an `Unapply(fun, implicits, patterns)` tree representing a pattern `<fun>(<patterns*>)(using <implicits*>)` */
      def unapply(x: Unapply): (Term, List[Term], List[Tree])
    }

    /** Makes extension methods on `Unapply` available without any imports */
    given UnapplyMethods: UnapplyMethods

    /** Extension methods of `Unapply` */
    trait UnapplyMethods:
      extension (self: Unapply)
        /** The extractor function of the pattern.
         *
         *  It may be a reference to the `unapply` method of the pattern or may be a
         *  partially applied tree containing type parameters and leading given parameters.
         */
        def fun: Term
        /** Training implicit parameters of the `unapply` method */
        def implicits: List[Term]
        /** List of nested patterns */
        def patterns: List[Tree]
      end extension
    end UnapplyMethods

    /** Pattern representing `X | Y | ...` alternatives. */
    type Alternatives <: Tree

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Tree` is an `Alternatives` */
    given AlternativesTypeTest: TypeTest[Tree, Alternatives]

    /** Module object of `type Alternatives`  */
    val Alternatives: AlternativesModule

    /** Methods of the module object `val Alternatives` */
    trait AlternativesModule { this: Alternatives.type =>
      def apply(patterns: List[Tree]): Alternatives
      def copy(original: Tree)(patterns: List[Tree]): Alternatives
      def unapply(x: Alternatives): Some[List[Tree]]
    }

    /** Makes extension methods on `Alternatives` available without any imports */
    given AlternativesMethods: AlternativesMethods

    /** Extension methods of `Alternatives` */
    trait AlternativesMethods:
      extension (self: Alternatives)
        def patterns: List[Tree]
      end extension
    end AlternativesMethods

    /** A parameter clause `[X1, ..., Xn]` or `(x1: X1, ..., xn: Xx)`
     *
     *  `[X1, ..., Xn]` are represented with `TypeParamClause` and `(x1: X1, ..., xn: Xx)` are represented with `TermParamClause`
     *
     *  `ParamClause` encodes the following enumeration
     *  ```scala
     *  //{
     *  import scala.quoted.*
     *  def inQuotes(using Quotes) = {
     *    val q: Quotes = summon[Quotes]
     *    import q.reflect.*
     *  //}
     *    enum ParamClause:
     *      case TypeParamClause(params: List[TypeDef])
     *      case TermParamClause(params: List[ValDef])
     *  //{
     *  }
     *  //}
     *  ```
     */
    type ParamClause <: AnyRef

    /** Module object of `type ParamClause`  */
    val ParamClause: ParamClauseModule

    /** Methods of the module object `val ParamClause` */
    trait ParamClauseModule { this: ParamClause.type =>
    }

    /** Makes extension methods on `ParamClause` available without any imports */
    given ParamClauseMethods: ParamClauseMethods

    /** Extension methods of `ParamClause` */
    trait ParamClauseMethods:
      extension (self: ParamClause)
        /** List of parameters of the clause */
        def params: List[ValDef] | List[TypeDef]
    end ParamClauseMethods

    /** A term parameter clause `(x1: X1, ..., xn: Xx)`
     *  Can also be `(implicit X1, ..., Xn)`, `(given X1, ..., Xn)` or `(given x1: X1, ..., xn: Xn)`
     */
    type TermParamClause <: ParamClause

    /** `TypeTest` that allows testing at runtime in a pattern match if a `ParamClause` is a `TermParamClause` */
    given TermParamClauseTypeTest: TypeTest[ParamClause, TermParamClause]

    /** Module object of `type TermParamClause`  */
    val TermParamClause: TermParamClauseModule

    /** Methods of the module object `val TermParamClause` */
    trait TermParamClauseModule { this: TermParamClause.type =>
      def apply(params: List[ValDef]): TermParamClause
      def unapply(x: TermParamClause): Some[List[ValDef]]
    }

    /** Makes extension methods on `TermParamClause` available without any imports */
    given TermParamClauseMethods: TermParamClauseMethods

    /** Extension methods of `TermParamClause` */
    trait TermParamClauseMethods:
      extension (self: TermParamClause)
        /** List of parameters of the clause */
        def params: List[ValDef]
        /** Is this an implicit parameter clause `(implicit x1: X1, ..., xn: Xn)` */
        def isImplicit: Boolean
        /** Is this a given parameter clause `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)` */
        def isGiven: Boolean
        /** Is this a erased parameter clause `(erased x1: X1, ..., xn: Xn)` */
        @deprecated("Use `hasErasedArgs` and `erasedArgs`", "3.4")
        def isErased: Boolean
        /** List of `erased` flags for each parameter of the clause */
        @experimental
        def erasedArgs: List[Boolean]
        /** Whether the clause has any erased parameters */
        @experimental
        def hasErasedArgs: Boolean
    end TermParamClauseMethods

    /** A type parameter clause `[X1, ..., Xn]` */
    type TypeParamClause <: ParamClause

    /** `TypeTest` that allows testing at runtime in a pattern match if a `ParamClause` is a `TypeParamClause` */
    given TypeParamClauseTypeTest: TypeTest[ParamClause, TypeParamClause]

    /** Module object of `type TypeParamClause`  */
    val TypeParamClause: TypeParamClauseModule

    /** Methods of the module object `val TypeParamClause` */
    trait TypeParamClauseModule { this: TypeParamClause.type =>
      def apply(params: List[TypeDef]): TypeParamClause
      def unapply(x: TypeParamClause): Some[List[TypeDef]]
    }

    /** Makes extension methods on `TypeParamClause` available without any imports */
    given TypeParamClauseMethods: TypeParamClauseMethods

    /** Extension methods of `TypeParamClause` */
    trait TypeParamClauseMethods:
      extension (self: TypeParamClause)
        /** List of parameters of the clause */
        def params: List[TypeDef]
    end TypeParamClauseMethods

    //////////////////////
    //    SELECTORS     //
    /////////////////////

    /** Import/Export selectors:
     *  - SimpleSelector: `.bar` in `import foo.bar`
     *  - RenameSelector: `.{bar => baz}` in `export foo.{bar => baz}`
     *  - OmitSelector: `.{bar => _}` in `import foo.{bar => _}`
     *  - GivenSelector: `.given`/`.{given T}` in `export foo.given`/`import foo.{given T}`
     */
    type Selector <: AnyRef

    /** Module object of `type Selector`  */
    val Selector: SelectorModule

    /** Methods of the module object `val Selector` */
    trait SelectorModule { this: Selector.type => }

    /** Simple import/export selector: `.bar` in `import foo.bar` */
    type SimpleSelector <: Selector

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Selector` is a `SimpleSelector` */
    given SimpleSelectorTypeTest: TypeTest[Selector, SimpleSelector]

    /** Module object of `type SimpleSelector`  */
    val SimpleSelector: SimpleSelectorModule

    /** Methods of the module object `val SimpleSelector` */
    trait SimpleSelectorModule { this: SimpleSelector.type =>
      def unapply(x: SimpleSelector): Some[String]
    }

    /** Makes extension methods on `SimpleSelector` available without any imports */
    given SimpleSelectorMethods: SimpleSelectorMethods

    /** Extension methods of `SimpleSelector` */
    trait SimpleSelectorMethods:
      extension (self: SimpleSelector)
        def name: String
        def namePos: Position
      end extension
    end SimpleSelectorMethods

    /** Rename import/export selector: `.{bar => baz}` in `import foo.{bar => baz}` */
    type RenameSelector <: Selector

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Selector` is a `RenameSelector` */
    given RenameSelectorTypeTest: TypeTest[Selector, RenameSelector]

    /** Module object of `type RenameSelector`  */
    val RenameSelector: RenameSelectorModule

    /** Methods of the module object `val RenameSelector` */
    trait RenameSelectorModule { this: RenameSelector.type =>
      def unapply(x: RenameSelector): (String, String)
    }

    /** Makes extension methods on `RenameSelector` available without any imports */
    given RenameSelectorMethods: RenameSelectorMethods

    /** Extension methods of `RenameSelector` */
    trait RenameSelectorMethods:
      extension (self: RenameSelector)
        def fromName: String
        def fromPos: Position
        def toName: String
        def toPos: Position
      end extension
    end RenameSelectorMethods

    /** Omit import/export selector: `.{bar => _}` in `import foo.{bar => _}` */
    type OmitSelector <: Selector

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Selector` is an `OmitSelector` */
    given OmitSelectorTypeTest: TypeTest[Selector, OmitSelector]

    /** Module object of `type OmitSelector`  */
    val OmitSelector: OmitSelectorModule

    /** Methods of the module object `val OmitSelector` */
    trait OmitSelectorModule { this: OmitSelector.type =>
      def unapply(x: OmitSelector): Some[String]
    }

    /** Makes extension methods on `OmitSelector` available without any imports */
    given OmitSelectorMethods: OmitSelectorMethods

    /** Extension methods of `OmitSelector` */
    trait OmitSelectorMethods:
      extension (self: OmitSelector)
        def name: String
        def namePos: Position
    end OmitSelectorMethods

    /** given import/export selector: `.given`/`.{given T}` in `import foo.given`/`export foo.{given T}` */
    type GivenSelector <: Selector

    /** `TypeTest` that allows testing at runtime in a pattern match if an `Selector` is a `GivenSelector` */
    given GivenSelectorTypeTest: TypeTest[Selector, GivenSelector]

    /** Module object of `type GivenSelector`  */
    val GivenSelector: GivenSelectorModule

    /** Methods of the module object `val GivenSelector` */
    trait GivenSelectorModule { this: GivenSelector.type =>
      def unapply(x: GivenSelector): Some[Option[TypeTree]]
    }

    /** Makes extension methods on `GivenSelector` available without any imports */
    given GivenSelectorMethods: GivenSelectorMethods

    /** Extension methods of `GivenSelector` */
    trait GivenSelectorMethods:
      extension (self: GivenSelector)
        def bound: Option[TypeTree]
    end GivenSelectorMethods

    ///////////////
    //   TYPES   //
    ///////////////

    // ----- Types ----------------------------------------------------

    /** A type, type constructors, type bounds or NoPrefix */
    type TypeRepr

    /** Module object of `type TypeRepr`  */
    val TypeRepr: TypeReprModule

    /** Methods of the module object `val TypeRepr` */
    trait TypeReprModule { this: TypeRepr.type =>
      /** Returns the type or kind (TypeRepr) of T */
      def of[T <: AnyKind](using Type[T]): TypeRepr

      /** Returns the type constructor of the runtime (erased) class */
      def typeConstructorOf(clazz: Class[?]): TypeRepr
    }

    /** Makes extension methods on `TypeRepr` available without any imports */
    given TypeReprMethods: TypeReprMethods

    /** Extension methods of `TypeRepr` */
    trait TypeReprMethods {
      extension (self: TypeRepr)

        /** Shows the type as a String */
        def show(using Printer[TypeRepr]): String

        /** Convert this `TypeRepr` to an `Type[?]`
        *
        *  Usage:
        *  ```scala
        *  //{
        *  import scala.quoted.*
        *  def f(using Quotes) = {
        *    val q: Quotes = summon[Quotes]
        *    import q.reflect.*
        *    val typeRepr: TypeRepr = ???
        *  //}
        *    typeRepr.asType match
        *      case '[t] =>
        *        '{ val x: t = ??? }
        *  //{
        *  }
        *  //}
        *  ```
        */
        def asType: Type[?]

        /** Is `self` type the same as `that` type?
        *  This is the case iff `self <:< that` and `that <:< self`.
        */
        def =:=(that: TypeRepr): Boolean

        /** Is this type a subtype of that type? */
        def <:<(that: TypeRepr): Boolean

        /** Widen from singleton type to its underlying non-singleton
         *  base type by applying one or more `underlying` dereferences,
         *  Also go from => T to T.
         *  Identity for all other types. Example:
         *
         *  class Outer { class C ; val x: C }
         *  def o: Outer
         *  <o.x.type>.widen = o.C
         */
        def widen: TypeRepr

        /** Widen from TermRef to its underlying non-termref
         *  base type, while also skipping ByName types.
         */
        def widenTermRefByName: TypeRepr

        /** Widen from ByName type to its result type. */
        def widenByName: TypeRepr

        /** Follow aliases, annotated types until type is no longer alias type, annotated type. */
        def dealias: TypeRepr

        /** Follow non-opaque aliases, annotated types until type is no longer alias type, annotated type. */
        def dealiasKeepOpaques: TypeRepr

        /** A simplified version of this type which is equivalent wrt =:= to this type.
        *  Reduces typerefs, applied match types, and and or types.
        */
        def simplified: TypeRepr

        def classSymbol: Option[Symbol]
        def typeSymbol: Symbol
        def termSymbol: Symbol
        def isSingleton: Boolean

       /** The type of `member` as seen from prefix `self`.
        *
        *  Also see `typeRef` and `termRef`
        */
        def memberType(member: Symbol): TypeRepr

        /** The base classes of this type with the class itself as first element. */
        def baseClasses: List[Symbol]

        /** The least type instance of given class which is a super-type
        *  of this type.  Example:
        *  {{{
        *    class D[T]
        *    class C extends p.D[Int]
        *    ThisType(C).baseType(D) = p.D[Int]
        * }}}
        */
        def baseType(cls: Symbol): TypeRepr

        /** Is this type an instance of a non-bottom subclass of the given class `cls`? */
        def derivesFrom(cls: Symbol): Boolean

        /** Is this type a function type?
        *
        *  @return true if the dealiased type of `self` without refinement is `FunctionN[T1, T2, ..., Tn]`
        *
        *  @note The function
        *
        *     - returns true for `given Int => Int` and `erased Int => Int`
        *     - returns false for `List[Int]`, despite that `List[Int] <:< Int => Int`.
        */
        def isFunctionType: Boolean

        /** Is this type an context function type?
        *
        *  @see `isFunctionType`
        */
        def isContextFunctionType: Boolean

        /** Is this type a function type with erased parameters?
        *
        *  @see `isFunctionType`
        */
        def isErasedFunctionType: Boolean

        /** Is this type a dependent function type?
        *
        *  @see `isFunctionType`
        */
        def isDependentFunctionType: Boolean

        /** Is this type a `TupleN` type?
         *
         * @return true if the dealiased type of `self` is `TupleN[T1, T2, ..., Tn]`
         */
        def isTupleN: Boolean

        /** The type <this . sym>, reduced if possible */
        def select(sym: Symbol): TypeRepr

        /** The current type applied to given type arguments: `this[targ]` */
        def appliedTo(targ: TypeRepr): TypeRepr

        /** The current type applied to given type arguments: `this[targ0, ..., targN]` */
        def appliedTo(targs: List[TypeRepr]): TypeRepr

        /** Substitute all types that refer in their symbol attribute to
         *  one of the symbols in `from` by the corresponding types in `to`.
         */
        def substituteTypes(from: List[Symbol], to: List[TypeRepr]): TypeRepr

        /** The applied type arguments (empty if there is no such arguments) */
        def typeArgs: List[TypeRepr]
      end extension
    }

    /** A singleton type representing a known constant value */
    type ConstantType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ConstantType` */
    given ConstantTypeTypeTest: TypeTest[TypeRepr, ConstantType]

    /** Module object of `type ConstantType`  */
    val ConstantType: ConstantTypeModule

    /** Methods of the module object `val Type` */
    trait ConstantTypeModule { this: ConstantType.type =>
      def apply(x : Constant): ConstantType
      def unapply(x: ConstantType): Some[Constant]
    }

    /** Makes extension methods on `ConstantType` available without any imports */
    given ConstantTypeMethods: ConstantTypeMethods

    /** Extension methods of `ConstantType` */
    trait ConstantTypeMethods:
      extension (self: ConstantType)
        def constant: Constant
      end extension
    end ConstantTypeMethods

    /** Type of a reference to a type or term symbol */
    type NamedType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `NamedType` */
    given NamedTypeTypeTest: TypeTest[TypeRepr, NamedType]

    /** Makes extension methods on `NamedType` available without any imports */
    given NamedTypeMethods: NamedTypeMethods

    /** Extension methods of `NamedType` */
    trait NamedTypeMethods:
      extension (self: NamedType)
        def qualifier: TypeRepr
        def name: String
      end extension
    end NamedTypeMethods

    /** Type of a reference to a term symbol */
    type TermRef <: NamedType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TermRef` */
    given TermRefTypeTest: TypeTest[TypeRepr, TermRef]

    /** Module object of `type TermRef`  */
    val TermRef: TermRefModule

    /** Methods of the module object `val TermRef` */
    trait TermRefModule { this: TermRef.type =>
      def apply(qual: TypeRepr, name: String): TermRef
      def unapply(x: TermRef): (TypeRepr, String)
    }

    /** Type of a reference to a type symbol */
    type TypeRef <: NamedType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TypeRef` */
    given TypeRefTypeTest: TypeTest[TypeRepr, TypeRef]

    /** Module object of `type TypeRef`  */
    val TypeRef: TypeRefModule

    /** Methods of the module object `val TypeRef` */
    trait TypeRefModule { this: TypeRef.type =>
      def unapply(x: TypeRef): (TypeRepr, String)
    }

    /** Makes extension methods on `TypeRef` available without any imports */
    given TypeRefMethods: TypeRefMethods

    /** Extension methods of `TypeRef` */
    trait TypeRefMethods:
      extension (self: TypeRef)
        def isOpaqueAlias: Boolean
        def translucentSuperType: TypeRepr
      end extension
    end TypeRefMethods

    /** Type of a `super` reference */
    type SuperType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `SuperType` */
    given SuperTypeTypeTest: TypeTest[TypeRepr, SuperType]

    /** Module object of `type SuperType`  */
    val SuperType: SuperTypeModule

    /** Methods of the module object `val SuperType` */
    trait SuperTypeModule { this: SuperType.type =>
      def apply(thistpe: TypeRepr, supertpe: TypeRepr): SuperType
      def unapply(x: SuperType): (TypeRepr, TypeRepr)
    }

    /** Makes extension methods on `SuperType` available without any imports */
    given SuperTypeMethods: SuperTypeMethods

    /** Extension methods of `SuperType` */
    trait SuperTypeMethods { this: SuperTypeMethods =>
      extension (self: SuperType)
        def thistpe: TypeRepr
        def supertpe: TypeRepr
      end extension
    }

    /** A type with a type refinement `T { type U }` */
    type Refinement <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `Refinement` */
    given RefinementTypeTest: TypeTest[TypeRepr, Refinement]

    /** Module object of `type Refinement`  */
    val Refinement: RefinementModule

    /** Methods of the module object `val Refinement` */
    trait RefinementModule { this: Refinement.type =>
      def apply(parent: TypeRepr, name: String, info: TypeRepr): Refinement
      def unapply(x: Refinement): (TypeRepr, String, TypeRepr)
    }

    /** Makes extension methods on `Refinement` available without any imports */
    given RefinementMethods: RefinementMethods

    /** Extension methods of `Refinement` */
    trait RefinementMethods:
      extension (self: Refinement)
        def parent: TypeRepr
        def name: String
        def info: TypeRepr
      end extension
    end RefinementMethods

    /** A higher kinded type applied to some types `T[U]` */
    type AppliedType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `AppliedType` */
    given AppliedTypeTypeTest: TypeTest[TypeRepr, AppliedType]

    /** Module object of `type AppliedType`  */
    val AppliedType: AppliedTypeModule

    /** Methods of the module object `val AppliedType` */
    trait AppliedTypeModule { this: AppliedType.type =>
      /** Applied the type constructor `T` to a list of type arguments `T_1,..,T_n` to create `T[T_1,..,T_n]` */
      def apply(tycon: TypeRepr, args: List[TypeRepr]): AppliedType
      def unapply(x: AppliedType): (TypeRepr, List[TypeRepr])
    }

    /** Makes extension methods on `AppliedType` available without any imports */
    given AppliedTypeMethods: AppliedTypeMethods

    /** Extension methods of `AppliedType` */
    trait AppliedTypeMethods:
      extension (self: AppliedType)
        def tycon: TypeRepr
        def args: List[TypeRepr]
      end extension
    end AppliedTypeMethods

    /** A type with an annotation `T @foo` */
    type AnnotatedType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `AnnotatedType` */
    given AnnotatedTypeTypeTest: TypeTest[TypeRepr, AnnotatedType]

    /** Module object of `type AnnotatedType`  */
    val AnnotatedType: AnnotatedTypeModule

    /** Methods of the module object `val AnnotatedType` */
    trait AnnotatedTypeModule { this: AnnotatedType.type =>
      def apply(underlying: TypeRepr, annot: Term): AnnotatedType
      def unapply(x: AnnotatedType): (TypeRepr, Term)
    }

    /** Makes extension methods on `AnnotatedType` available without any imports */
    given AnnotatedTypeMethods: AnnotatedTypeMethods

    /** Extension methods of `AnnotatedType` */
    trait AnnotatedTypeMethods:
      extension (self: AnnotatedType)
        def underlying: TypeRepr
        def annotation: Term
      end extension
    end AnnotatedTypeMethods


    /** Intersection type `T & U` or an union type `T | U` */
    type AndOrType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `AndOrType` */
    given AndOrTypeTypeTest: TypeTest[TypeRepr, AndOrType]

    /** Makes extension methods on `AndOrType` available without any imports */
    given AndOrTypeMethods: AndOrTypeMethods

    /** Extension methods of `AndOrType` */
    trait AndOrTypeMethods:
      extension (self: AndOrType)
        def left: TypeRepr
        def right: TypeRepr
      end extension
    end AndOrTypeMethods

    /** Intersection type `T & U` */
    type AndType <: AndOrType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `AndType` */
    given AndTypeTypeTest: TypeTest[TypeRepr, AndType]

    /** Module object of `type AndType`  */
    val AndType: AndTypeModule

    /** Methods of the module object `val AndType` */
    trait AndTypeModule { this: AndType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): AndType
      def unapply(x: AndType): (TypeRepr, TypeRepr)
    }

    /** Union type `T | U` */
    type OrType <: AndOrType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is an `OrType` */
    given OrTypeTypeTest: TypeTest[TypeRepr, OrType]

    /** Module object of `type OrType`  */
    val OrType: OrTypeModule

    /** Methods of the module object `val OrType` */
    trait OrTypeModule { this: OrType.type =>
      def apply(lhs: TypeRepr, rhs: TypeRepr): OrType
      def unapply(x: OrType): (TypeRepr, TypeRepr)
    }

    /** Type match `T match { case U => ... }` */
    type MatchType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `MatchType` */
    given MatchTypeTypeTest: TypeTest[TypeRepr, MatchType]

    /** Module object of `type MatchType`  */
    val MatchType: MatchTypeModule

    /** Methods of the module object `val MatchType` */
    trait MatchTypeModule { this: MatchType.type =>
      def apply(bound: TypeRepr, scrutinee: TypeRepr, cases: List[TypeRepr]): MatchType
      def unapply(x: MatchType): (TypeRepr, TypeRepr, List[TypeRepr])
    }

    /** Makes extension methods on `MatchType` available without any imports */
    given MatchTypeMethods: MatchTypeMethods

    /** Extension methods of `MatchType` */
    trait MatchTypeMethods:
      extension (self: MatchType)
        def bound: TypeRepr
        def scrutinee: TypeRepr
        def cases: List[TypeRepr]
      end extension
    end MatchTypeMethods

    /** Type of a by-name definition of type `=>T`.
     *
     *  May represent by-name parameter such as `thunk` in
     *  ```scala
     *  //{
     *  type T
     *  //}
     *  def log[T](thunk: => T): T = ???
     *  ```
     *
     *  May also represent a the return type of a parameterless method definition such as
     *  ```scala
     *    def foo: Int = ???
     *  ```
     */
    type ByNameType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ByNameType` */
    given ByNameTypeTypeTest: TypeTest[TypeRepr, ByNameType]

    /** Module object of `type ByNameType`  */
    val ByNameType: ByNameTypeModule

    /** Methods of the module object `val ByNameType` */
    trait ByNameTypeModule { this: ByNameType.type =>
      def apply(underlying: TypeRepr): TypeRepr
      def unapply(x: ByNameType): Some[TypeRepr]
    }

    /** Makes extension methods on `ByNameType` available without any imports */
    given ByNameTypeMethods: ByNameTypeMethods

    /** Extension methods of `ByNameType` */
    trait ByNameTypeMethods:
      extension (self: ByNameType)
        def underlying: TypeRepr
      end extension
    end ByNameTypeMethods

    /** Type of a parameter reference */
    type ParamRef <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ParamRef` */
    given ParamRefTypeTest: TypeTest[TypeRepr, ParamRef]

    /** Module object of `type ParamRef`  */
    val ParamRef: ParamRefModule

    /** Methods of the module object `val ParamRef` */
    trait ParamRefModule { this: ParamRef.type =>
      def unapply(x: ParamRef): (TypeRepr, Int)
    }

    /** Makes extension methods on `ParamRef` available without any imports */
    given ParamRefMethods: ParamRefMethods

    /** Extension methods of `ParamRef` */
    trait ParamRefMethods:
      extension (self: ParamRef)
        def binder: TypeRepr
        def paramNum: Int
      end extension
    end ParamRefMethods

    /** Type of `this` */
    type ThisType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `ThisType` */
    given ThisTypeTypeTest: TypeTest[TypeRepr, ThisType]

    /** Module object of `type ThisType`  */
    val ThisType: ThisTypeModule

    /** Methods of the module object `val ThisType` */
    trait ThisTypeModule { this: ThisType.type =>
      def unapply(x: ThisType): Some[TypeRepr]
    }

    /** Makes extension methods on `ThisType` available without any imports */
    given ThisTypeMethods: ThisTypeMethods

    /** Extension methods of `ThisType` */
    trait ThisTypeMethods:
      extension (self: ThisType)
        def tref: TypeRepr
      end extension
    end ThisTypeMethods

    /** A type that is recursively defined `this` */
    type RecursiveThis <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `RecursiveThis` */
    given RecursiveThisTypeTest: TypeTest[TypeRepr, RecursiveThis]

    /** Module object of `type RecursiveThis`  */
    val RecursiveThis: RecursiveThisModule

    /** Methods of the module object `val RecursiveThis` */
    trait RecursiveThisModule { this: RecursiveThis.type =>
      def unapply(x: RecursiveThis): Some[RecursiveType]
    }

    /** Makes extension methods on `RecursiveThis` available without any imports */
    given RecursiveThisMethods: RecursiveThisMethods

    /** Extension methods of `RecursiveThis` */
    trait RecursiveThisMethods:
      extension (self: RecursiveThis)
        def binder: RecursiveType
      end extension
    end RecursiveThisMethods

    /** A type that is recursively defined */
    type RecursiveType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `RecursiveType` */
    given RecursiveTypeTypeTest: TypeTest[TypeRepr, RecursiveType]

    /** Module object of `type RecursiveType`  */
    val RecursiveType: RecursiveTypeModule

    /** Methods of the module object `val RecursiveType` */
    trait RecursiveTypeModule { this: RecursiveType.type =>

      /** Create a RecType, normalizing its contents. This means:
      *
      *   1. Nested Rec types on the type's spine are merged with the outer one.
      *   2. Any refinement of the form `type T = z.T` on the spine of the type
      *      where `z` refers to the created rec-type is replaced by
      *      `type T`. This avoids infinite recursions later when we
      *      try to follow these references.
      */
      def apply(parentExp: RecursiveType => TypeRepr): RecursiveType

      def unapply(x: RecursiveType): Some[TypeRepr]
    }

    /** Makes extension methods on `RecursiveType` available without any imports */
    given RecursiveTypeMethods: RecursiveTypeMethods

    /** Extension methods of `RecursiveType` */
    trait RecursiveTypeMethods:
      extension (self: RecursiveType)
        def underlying: TypeRepr
        def recThis: RecursiveThis
      end extension
    end RecursiveTypeMethods

    /** Type of the definition of a method taking a single list of type or term parameters */
    type LambdaType <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `LambdaType` */
    given LambdaTypeTypeTest: TypeTest[TypeRepr, LambdaType]

    /** Makes extension methods on `LambdaType` available without any imports */
    given LambdaTypeMethods: LambdaTypeMethods

    /** Extension methods of `LambdaType` */
    trait LambdaTypeMethods:
      extension (self: LambdaType)
        def paramNames: List[String]
        def paramTypes: List[TypeRepr]
        def resType: TypeRepr
      end extension
    end LambdaTypeMethods

    /** Type of the definition of a method taking a single list of type or term parameters */
    type MethodOrPoly <: LambdaType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `MethodOrPoly` */
    given MethodOrPolyTypeTest: TypeTest[TypeRepr, MethodOrPoly]

    /** Type of the definition of a method taking a single list of parameters. It's return type may be a MethodType. */
    type MethodType <: MethodOrPoly

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `MethodType` */
    given MethodTypeTypeTest: TypeTest[TypeRepr, MethodType]

    /** Module object of `type MethodType`  */
    val MethodType: MethodTypeModule

    /** Methods of the module object `val MethodType` */
    trait MethodTypeModule { this: MethodType.type =>
      def apply(paramNames: List[String])(paramInfosExp: MethodType => List[TypeRepr], resultTypeExp: MethodType => TypeRepr): MethodType
      def unapply(x: MethodType): (List[String], List[TypeRepr], TypeRepr)
    }

    /** Makes extension methods on `MethodType` available without any imports */
    given MethodTypeMethods: MethodTypeMethods

    /** Extension methods of `MethodType` */
    trait MethodTypeMethods:
      extension (self: MethodType)
        /** Is this the type of using parameter clause `(implicit X1, ..., Xn)`, `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)` */
        def isImplicit: Boolean
        /** Is this the type of erased parameter clause `(erased x1: X1, ..., xn: Xn)` */
        @deprecated("Use `hasErasedParams` and `erasedParams`", "3.4")
        def isErased: Boolean
        /** List of `erased` flags for each parameters of the clause */
        @experimental
        def erasedParams: List[Boolean]
        /** Whether the clause has any erased parameters */
        @experimental
        def hasErasedParams: Boolean
        def param(idx: Int): TypeRepr
      end extension
    end MethodTypeMethods

    /** Type of the definition of a method taking a list of type parameters. It's return type may be a MethodType. */
    type PolyType <: MethodOrPoly

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `PolyType` */
    given PolyTypeTypeTest: TypeTest[TypeRepr, PolyType]

    /** Module object of `type PolyType`  */
    val PolyType: PolyTypeModule

    /** Methods of the module object `val PolyType` */
    trait PolyTypeModule { this: PolyType.type =>
      def apply(paramNames: List[String])(paramBoundsExp: PolyType => List[TypeBounds], resultTypeExp: PolyType => TypeRepr): PolyType
      def unapply(x: PolyType): (List[String], List[TypeBounds], TypeRepr)
    }

    /** Makes extension methods on `PolyType` available without any imports */
    given PolyTypeMethods: PolyTypeMethods

    /** Extension methods of `PolyType` */
    trait PolyTypeMethods:
      extension (self: PolyType)
        def param(idx: Int): TypeRepr
        def paramBounds: List[TypeBounds]
      end extension
    end PolyTypeMethods

    /** Type of the definition of a type lambda taking a list of type parameters. It's return type may be a TypeLambda. */
    type TypeLambda <: LambdaType

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TypeLambda` */
    given TypeLambdaTypeTest: TypeTest[TypeRepr, TypeLambda]

    /** Module object of `type TypeLambda`  */
    val TypeLambda: TypeLambdaModule

    /** Methods of the module object `val TypeLambda` */
    trait TypeLambdaModule { this: TypeLambda.type =>
      def apply(paramNames: List[String], boundsFn: TypeLambda => List[TypeBounds], bodyFn: TypeLambda => TypeRepr): TypeLambda
      def unapply(x: TypeLambda): (List[String], List[TypeBounds], TypeRepr)
    }

    /** Makes extension methods on `TypeLambda` available without any imports */
    given TypeLambdaMethods: TypeLambdaMethods

    /** Extension methods of `TypeLambda` */
    trait TypeLambdaMethods:
      extension (self: TypeLambda)
        /** Reference to the i-th parameter */
        def param(idx: Int) : TypeRepr
        /** Type bounds of the i-th parameter */
        def paramBounds: List[TypeBounds]
        /** Variance flags for the i-th parameter
         *
         *  Variance flags can be one of `Flags.{Covariant, Contravariant, EmptyFlags}`.
         */
        def paramVariances: List[Flags]
      end extension
    end TypeLambdaMethods

    /** Case of a `MatchType` containing pattern `case P => R`.
     *
     *  Note: cases with type bindings are represented nested in a `TypeLambda`.
     */
    type MatchCase <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `MatchCase` */
    given MatchCaseTypeTest: TypeTest[TypeRepr, MatchCase]

    /** Module object of `type MatchCase`  */
    val MatchCase: MatchCaseModule

    /** Methods of the module object `val MatchCase` */
    trait MatchCaseModule { this: MatchCase.type =>
      /* Create match type case `case <pattern> => <rhs>` */
      def apply(pattern: TypeRepr, rhs: TypeRepr): MatchCase
      /* Matches a match type case `case <pattern> => <rhs>` */
      def unapply(x: MatchCase): (TypeRepr, TypeRepr)
    }

    /** Makes extension methods on `MatchCase` available without any imports */
    given MatchCaseMethods: MatchCaseMethods

    /** Extension methods of `MatchCase` */
    trait MatchCaseMethods:
      extension (self: MatchCase)
        /** Pattern `P` of `case P => R` in a `MatchType` */
        def pattern: TypeRepr
        /** RHS `R` of `case P => R` in a `MatchType` */
        def rhs: TypeRepr
      end extension
    end MatchCaseMethods


    // ----- TypeBounds -----------------------------------------------

    /** Type bounds */
    type TypeBounds <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `TypeBounds` */
    given TypeBoundsTypeTest: TypeTest[TypeRepr, TypeBounds]

    /** Module object of `type TypeBounds`  */
    val TypeBounds: TypeBoundsModule

    /** Methods of the module object `val TypeBounds` */
    trait TypeBoundsModule { this: TypeBounds.type =>
      def apply(low: TypeRepr, hi: TypeRepr): TypeBounds
      def unapply(x: TypeBounds): (TypeRepr, TypeRepr)
      def empty: TypeBounds
      def upper(hi: TypeRepr): TypeBounds
      def lower(lo: TypeRepr): TypeBounds
    }

    /** Makes extension methods on `TypeBounds` available without any imports */
    given TypeBoundsMethods: TypeBoundsMethods

    /** Extension methods of `TypeBounds` */
    trait TypeBoundsMethods:
      extension (self: TypeBounds)
        def low: TypeRepr
        def hi: TypeRepr
      end extension
    end TypeBoundsMethods

    // ----- NoPrefix -------------------------------------------------

    /** NoPrefix for a type selection */
    type NoPrefix <: TypeRepr

    /** `TypeTest` that allows testing at runtime in a pattern match if a `TypeRepr` is a `NoPrefix` */
    given NoPrefixTypeTest: TypeTest[TypeRepr, NoPrefix]

    /** Module object of `type NoPrefix`  */
    val NoPrefix: NoPrefixModule

    /** Methods of the module object `val NoPrefix` */
    trait NoPrefixModule { this: NoPrefix.type =>
      def unapply(x: NoPrefix): true
    }

    ///////////////
    // CONSTANTS //
    ///////////////

    /** Constant value represented as the constant itself */
    type Constant <: AnyRef

    /** Constant value represented as the constant itself */
    val Constant: ConstantModule

    /** Constant value represented as the constant itself */
    trait ConstantModule { this: Constant.type => }

    /** Makes extension methods on `Constant` available without any imports */
    given ConstantMethods: ConstantMethods

    /** Extension methods of `Constant` */
    trait ConstantMethods {
      extension (self: Constant)
        /** Returns the value of the constant */
        def value: Any

        /** Shows the constant as a String */
        def show(using Printer[Constant]): String

      end extension
    }

    /** Constant Boolean value */
    type BooleanConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `BooleanConstant` */
    given BooleanConstantTypeTest: TypeTest[Constant, BooleanConstant]

    /** Module object of `type BooleanConstant` */
    val BooleanConstant: BooleanConstantModule

    /** Methods of the module object `val BooleanConstant` */
    trait BooleanConstantModule { this: BooleanConstant.type =>
      /** Create a constant Boolean value */
      def apply(x: Boolean): BooleanConstant
      /** Match Boolean value constant and extract its value */
      def unapply(constant: BooleanConstant): Some[Boolean]
    }

    /** Constant Byte value */
    type ByteConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `ByteConstant` */
    given ByteConstantTypeTest: TypeTest[Constant, ByteConstant]

    /** Module object of `type ByteConstant` */
    val ByteConstant: ByteConstantModule

    /** Methods of the module object `val ByteConstant` */
    trait ByteConstantModule { this: ByteConstant.type =>
      /** Create a constant Byte value */
      def apply(x: Byte): ByteConstant
      /** Match Byte value constant and extract its value */
      def unapply(constant: ByteConstant): Some[Byte]
    }

    /** Constant Short value */
    type ShortConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `ShortConstant` */
    given ShortConstantTypeTest: TypeTest[Constant, ShortConstant]

    /** Module object of `type ShortConstant` */
    val ShortConstant: ShortConstantModule

    /** Methods of the module object `val Short` */
    trait ShortConstantModule { this: ShortConstant.type =>
      /** Create a constant Short value */
      def apply(x: Short): ShortConstant
      /** Match Short value constant and extract its value */
      def unapply(constant: ShortConstant): Some[Short]
    }

    /** Constant Int value */
    type IntConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `IntConstant` */
    given IntConstantTypeTest: TypeTest[Constant, IntConstant]

    /** Module object of `type IntConstant` */
    val IntConstant: IntConstantModule

    /** Methods of the module object `val IntConstant` */
    trait IntConstantModule { this: IntConstant.type =>
      /** Create a constant Int value */
      def apply(x: Int): IntConstant
      /** Match Int value constant and extract its value */
      def unapply(constant: IntConstant): Some[Int]
    }

    /** Constant Long value */
    type LongConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `LongConstant` */
    given LongConstantTypeTest: TypeTest[Constant, LongConstant]

    /** Module object of `type LongConstant` */
    val LongConstant: LongConstantModule

    /** Methods of the module object `val LongConstant` */
    trait LongConstantModule { this: LongConstant.type =>
      /** Create a constant Long value */
      def apply(x: Long): LongConstant
      /** Match Long value constant and extract its value */
      def unapply(constant: LongConstant): Some[Long]
    }

    /** Constant Float value */
    type FloatConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `FloatConstant` */
    given FloatConstantTypeTest: TypeTest[Constant, FloatConstant]

    /** Module object of `type FloatConstant` */
    val FloatConstant: FloatConstantModule

    /** Methods of the module object `val FloatConstant` */
    trait FloatConstantModule { this: FloatConstant.type =>
      /** Create a constant Float value */
      def apply(x: Float): FloatConstant
      /** Match Float value constant and extract its value */
      def unapply(constant: FloatConstant): Some[Float]
    }

    /** Constant Double value */
    type DoubleConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `DoubleConstant` */
    given DoubleConstantTypeTest: TypeTest[Constant, DoubleConstant]

    /** Module object of `type DoubleConstant` */
    val DoubleConstant: DoubleConstantModule

    /** Methods of the module object `val DoubleConstant` */
    trait DoubleConstantModule { this: DoubleConstant.type =>
      /** Create a constant Double value */
      def apply(x: Double): DoubleConstant
      /** Match Double value constant and extract its value */
      def unapply(constant: DoubleConstant): Some[Double]
    }

    /** Constant Char value */
    type CharConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `CharConstant` */
    given CharConstantTypeTest: TypeTest[Constant, CharConstant]

    /** Module object of `type CharConstant` */
    val CharConstant: CharConstantModule

    /** Methods of the module object `val CharConstant` */
    trait CharConstantModule { this: CharConstant.type =>
      /** Create a constant Char value */
      def apply(x: Char): CharConstant
      /** Match Char value constant and extract its value */
      def unapply(constant: CharConstant): Some[Char]
    }

    /** Constant String value */
    type StringConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `StringConstant` */
    given StringConstantTypeTest: TypeTest[Constant, StringConstant]

    /** Module object of `type StringConstant` */
    val StringConstant: StringConstantModule

    /** Methods of the module object `val StringConstant` */
    trait StringConstantModule { this: StringConstant.type =>
      /** Create a constant String value */
      def apply(x: String): StringConstant
      /** Match String value constant and extract its value */
      def unapply(constant: StringConstant): Some[String]
    }

    /** Constant Unit value */
    type UnitConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `UnitConstant` */
    given UnitConstantTypeTest: TypeTest[Constant, UnitConstant]

    /** Module object of `type UnitConstant` */
    val UnitConstant: UnitConstantModule

    /** Methods of the module object `val UnitConstant` */
    trait UnitConstantModule { this: UnitConstant.type =>
      /** Create a constant Unit value */
      def apply(): UnitConstant
      /** Match Unit value constant */
      def unapply(constant: UnitConstant): true
    }

    /** Constant null value */
    type NullConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `NullConstant` */
    given NullConstantTypeTest: TypeTest[Constant, NullConstant]

    /** Module object of `type NullConstant` */
    val NullConstant: NullConstantModule

    /** Methods of the module object `val NullConstant` */
    trait NullConstantModule { this: NullConstant.type =>
      /** Create a constant null value */
      def apply(): NullConstant
      /** Match null value constant */
      def unapply(constant: NullConstant): Boolean
    }

    /** Constant class value representing a `classOf[T]` */
    type ClassOfConstant <: Constant

    /** `TypeTest` that allows testing at runtime in a pattern match if a `Constant` is a `ClassOfConstant` */
    given ClassOfConstantTypeTest: TypeTest[Constant, ClassOfConstant]

    /** Module object of `type ClassOfConstant` */
    val ClassOfConstant: ClassOfConstantModule

    /** Methods of the module object `val ClassOf` */
    trait ClassOfConstantModule { this: ClassOfConstant.type =>
      /** Create a constant class value representing `classOf[<tpe>]` */
      def apply(tpe: TypeRepr): ClassOfConstant
      /** Match a class value constant representing `classOf[<tpe>]` and extract its type */
      def unapply(constant: ClassOfConstant): Option[TypeRepr]
    }

    /////////////////////
    // IMPLICIT SEARCH //
    /////////////////////

    /** Module object of `type Implicits`  */
    val Implicits: ImplicitsModule

    /** Methods of the module object `val Implicits` */
    trait ImplicitsModule { self: Implicits.type =>
      /** Find a given instance of type `T` in the current scope provided by the current enclosing splice.
      *  Return an `ImplicitSearchResult`.
      *
      *  @param tpe type of the implicit parameter
      */
      def search(tpe: TypeRepr): ImplicitSearchResult
    }

    /** Result of a given instance search */
    type ImplicitSearchResult <: AnyRef

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is an `ImplicitSearchSuccess` */
    given ImplicitSearchSuccessTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchSuccess]

    type ImplicitSearchSuccess <: ImplicitSearchResult

    /** Makes extension methods on `ImplicitSearchSuccess` available without any imports */
    given ImplicitSearchSuccessMethods: ImplicitSearchSuccessMethods

    /** Extension methods of `ImplicitSearchSuccess` */
    trait ImplicitSearchSuccessMethods:
      extension (self: ImplicitSearchSuccess)
        def tree: Term
      end extension
    end ImplicitSearchSuccessMethods

    type ImplicitSearchFailure <: ImplicitSearchResult

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is an `ImplicitSearchFailure` */
    given ImplicitSearchFailureTypeTest: TypeTest[ImplicitSearchResult, ImplicitSearchFailure]

    /** Makes extension methods on `ImplicitSearchFailure` available without any imports */
    given ImplicitSearchFailureMethods: ImplicitSearchFailureMethods

    /** Extension methods of `ImplicitSearchFailure` */
    trait ImplicitSearchFailureMethods:
      extension (self: ImplicitSearchFailure)
        def explanation: String
      end extension
    end ImplicitSearchFailureMethods

    type DivergingImplicit <: ImplicitSearchFailure

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is a `DivergingImplicit` */
    given DivergingImplicitTypeTest: TypeTest[ImplicitSearchResult, DivergingImplicit]

    type NoMatchingImplicits <: ImplicitSearchFailure

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is a `NoMatchingImplicits` */
    given NoMatchingImplicitsTypeTest: TypeTest[ImplicitSearchResult, NoMatchingImplicits]

    type AmbiguousImplicits <: ImplicitSearchFailure

    /** `TypeTest` that allows testing at runtime in a pattern match if an `ImplicitSearchResult` is an `AmbiguousImplicits` */
    given AmbiguousImplicitsTypeTest: TypeTest[ImplicitSearchResult, AmbiguousImplicits]

    /////////////
    // SYMBOLS //
    /////////////

    /** Symbol of a definition.
    *   Symbols can be compared with `==` to know if two definitions are the same.
    */
    type Symbol <: AnyRef

    /** Module object of `type Symbol`  */
    val Symbol: SymbolModule

    /** Methods of the module object `val Symbol` */
    trait SymbolModule { this: Symbol.type =>

      /** Symbol of the definition that encloses the current splicing context.
       *
       *  For example, the following call to `spliceOwner` would return the symbol `x`.
       *  ```scala sc:nocompile
       *  val x = ${ ... Symbol.spliceOwner ... }
       *  ```
       *
       *  For a macro splice, it is the symbol of the definition where the macro expansion happens.
       */
      def spliceOwner: Symbol

      /** Get package symbol if package is either defined in current compilation run or present on classpath. */
      def requiredPackage(path: String): Symbol

      /** Get class symbol if class is either defined in current compilation run or present on classpath. */
      def requiredClass(path: String): Symbol

      /** Get module symbol if module is either defined in current compilation run or present on classpath. */
      def requiredModule(path: String): Symbol

      /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
      def requiredMethod(path: String): Symbol

      /** The class Symbol of a global class definition */
      def classSymbol(fullName: String): Symbol

      /** Generates a new class symbol for a class with a parameterless constructor.
       *
       *  Example usage:
       *  ```
       *  val name: String = "myClass"
       *  val parents = List(TypeTree.of[Object], TypeTree.of[Foo])
       *  def decls(cls: Symbol): List[Symbol] =
       *    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
       *
       *  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
       *  val fooSym = cls.declaredMethod("foo").head
       *
       *  val fooDef = DefDef(fooSym, argss => Some('{println(s"Calling foo")}.asTerm))
       *  val clsDef = ClassDef(cls, parents, body = List(fooDef))
       *  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Foo])
       *
       *  Block(List(clsDef), newCls).asExprOf[Foo]
       *  ```
       *  constructs the equivalent to
       *   ```
       *  '{
       *    class myClass() extends Object with Foo {
       *      def foo(): Unit = println("Calling foo")
       *    }
       *    new myClass(): Foo
       *  }
       *  ```
       *
       *  @param parent The owner of the class
       *  @param name The name of the class
       *  @param parents The parent classes of the class. The first parent must not be a trait.
       *  @param decls The member declarations of the class provided the symbol of this class
       *  @param selfType The self type of the class if it has one
       *
       *  This symbol starts without an accompanying definition.
       *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
       *  this symbol to the ClassDef constructor.
       *
       *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
       *        direct or indirect children of the reflection context's owner.
       */
      // TODO: add flags and privateWithin
      @experimental def newClass(parent: Symbol, name: String, parents: List[TypeRepr], decls: Symbol => List[Symbol], selfType: Option[TypeRepr]): Symbol

      /** Generates a new module symbol with an associated module class symbol,
       *  this is equivalent to an `object` declaration in source code.
       *  This method returns the module symbol. The module class can be accessed calling `moduleClass` on this symbol.
       *
       *  Example usage:
       *  ```scala
       *  //{
       *  given Quotes = ???
       *  import quotes.reflect.*
       *  //}
       *  val moduleName: String = Symbol.freshName("MyModule")
       *  val parents = List(TypeTree.of[Object])
       *  def decls(cls: Symbol): List[Symbol] =
       *    List(Symbol.newMethod(cls, "run", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.EmptyFlags, Symbol.noSymbol))
       *
       *  val mod = Symbol.newModule(Symbol.spliceOwner, moduleName, Flags.EmptyFlags, Flags.EmptyFlags, parents.map(_.tpe), decls, Symbol.noSymbol)
       *  val cls = mod.moduleClass
       *  val runSym = cls.declaredMethod("run").head
       *
       *  val runDef = DefDef(runSym, _ => Some('{ println("run") }.asTerm))
       *  val modDef = ClassDef.module(mod, parents, body = List(runDef))
       *
       *  val callRun = Apply(Select(Ref(mod), runSym), Nil)
       *
       *  Block(modDef.toList, callRun)
       *  ```
       *  constructs the equivalent to
       *  ```scala
       *  //{
       *  given Quotes = ???
       *  import quotes.reflect.*
       *  //}
       *  '{
       *    object MyModule$macro$1 extends Object:
       *      def run(): Unit = println("run")
       *    MyModule$macro$1.run()
       *  }
       *  ```
       *
       *  @param parent The owner of the class
       *  @param name The name of the class
       *  @param modFlags extra flags with which the module symbol should be constructed
       *  @param clsFlags extra flags with which the module class symbol should be constructed
       *  @param parents The parent classes of the class. The first parent must not be a trait.
       *  @param decls A function that takes the symbol of the module class as input and return the symbols of its declared members
       *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
       *
       *  This symbol starts without an accompanying definition.
       *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
       *  this symbol to `ClassDef.module`.
       *
       *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
       *        direct or indirect children of the reflection context's owner.
       *
       *  @syntax markdown
       */
      @experimental def newModule(owner: Symbol, name: String, modFlags: Flags, clsFlags: Flags, parents: List[TypeRepr], decls: Symbol => List[Symbol], privateWithin: Symbol): Symbol

      /** Generates a new method symbol with the given parent, name and type.
       *
       *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
       *
       *  @param parent The owner of the method
       *  @param name The name of the method
       *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
       *
       *  This symbol starts without an accompanying definition.
       *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
       *  this symbol to the DefDef constructor.
       *
       *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
       *        direct or indirect children of the reflection context's owner.
       */
      def newMethod(parent: Symbol, name: String, tpe: TypeRepr): Symbol

      /** Works as the other newMethod, but with additional parameters.
      *
      *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
      *
      *  @param parent The owner of the method
      *  @param name The name of the method
      *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
      *  @param flags extra flags to with which the symbol should be constructed. `Method` flag will be added. Can be `Private | Protected | Override | Deferred | Final | Method | Implicit | Given | Local | JavaStatic | Synthetic | Artifact`
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      */
      // Keep: `flags` doc aligned with QuotesImpl's `validMethodFlags`
      def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

      /** Generates a new val/var/lazy val symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the ValDef constructor.
      *
      *  Note: Also see ValDef.let
      *
      *  @param parent The owner of the val/var/lazy val
      *  @param name The name of the val/var/lazy val
      *  @param tpe The type of the val/var/lazy val
      *  @param flags extra flags to with which the symbol should be constructed. Can be `Private | Protected | Override | Deferred | Final | Param | Implicit | Lazy | Mutable | Local | ParamAccessor | Module | Package | Case | CaseAccessor | Given | Enum | JavaStatic | Synthetic | Artifact`
      *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
      // Keep: `flags` doc aligned with QuotesImpl's `validValFlags`
      def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol

      /** Generates a pattern bind symbol with the given parent, name and type.
      *
      *  This symbol starts without an accompanying definition.
      *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
      *  this symbol to the BindDef constructor.
      *
      *  @param parent The owner of the binding
      *  @param name The name of the binding
      *  @param flags extra flags to with which the symbol should be constructed. `Case` flag will be added. Can be `Case`
      *  @param tpe The type of the binding
      *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
      *        direct or indirect children of the reflection context's owner.
      */
      // Keep: `flags` doc aligned with QuotesImpl's `validBindFlags`
      def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr): Symbol

      /** Definition not available */
      def noSymbol: Symbol

      /** A fresh name for class or member symbol names.
       *
       *  Fresh names are constructed using the following format `prefix + "$macro$" + freshIndex`.
       *  The `freshIndex` are unique within the current source file.
       *
       *  Examples: See `scala.annotation.MacroAnnotation`
       *
       *  @param prefix Prefix of the fresh name
       */
      @experimental
      def freshName(prefix: String): String
    }

    /** Makes extension methods on `Symbol` available without any imports */
    given SymbolMethods: SymbolMethods

    /** Extension methods of `Symbol` */
    trait SymbolMethods {
      extension (self: Symbol)

        /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Throws if this symbol does not have an owner. */
        def owner: Symbol

        /** Owner of this symbol. The owner is the symbol in which this symbol is defined. Returns `NoSymbol` if this symbol does not have an owner. */
        def maybeOwner: Symbol

        /** Flags of this symbol */
        def flags: Flags

        /** This symbol is private within the resulting type */
        def privateWithin: Option[TypeRepr]

        /** This symbol is protected within the resulting type */
        def protectedWithin: Option[TypeRepr]

        /** The name of this symbol */
        def name: String

        /** The full name of this symbol up to the root package */
        def fullName: String

        /** Type of the definition */
        @experimental
        def info: TypeRepr

        /** The position of this symbol */
        def pos: Option[Position]

        /** The documentation for this symbol, if any */
        def docstring: Option[String]

        /** Tree of this definition
         *
         *  If this symbol `isClassDef` it will return `a `ClassDef`,
         *  if this symbol `isTypeDef` it will return `a `TypeDef`,
         *  if this symbol `isValDef` it will return `a `ValDef`,
         *  if this symbol `isDefDef` it will return `a `DefDef`
         *  if this symbol `isBind` it will return `a `Bind`,
         *  else will throw
         *
         *  **Warning**: avoid using this method in macros.
         *
         *  **Caveat**: The tree is not guaranteed to exist unless the compiler
         *  option `-Yretain-trees` is enabled.
         *
         *  **Anti-pattern**: The following code is an anti-pattern:
         *
         *      symbol.tree.tpe
         *
         *  It should be replaced by one of the following:
         *
         *      tp.memberType(symbol)
         *      symbol.typeRef
         *      symbol.termRef
         *
         */
        def tree: Tree

        /** Is the annotation defined with `annotSym` attached to this symbol? */
        def hasAnnotation(annotSym: Symbol): Boolean

        /** Get the annotation defined with `annotSym` attached to this symbol */
        def getAnnotation(annotSym: Symbol): Option[Term]

        /** Annotations attached to this symbol */
        def annotations: List[Term]

        /** Does this symbol come from a currently compiled source file? */
        def isDefinedInCurrentRun: Boolean

        /** Dummy val symbol that owns all statements within the initialization of the class.
        *  This may also contain local definitions such as classes defined in a `locally` block in the class.
        */
        def isLocalDummy: Boolean

        /** Is this symbol a class representing a refinement? */
        def isRefinementClass: Boolean

        /** Is this symbol an alias type? */
        def isAliasType: Boolean

        /** Is this symbol an anonymous class? */
        def isAnonymousClass: Boolean

        /** Is this symbol an anonymous function? */
        def isAnonymousFunction: Boolean

        /** Is this symbol an abstract type? */
        def isAbstractType: Boolean

        /** Is this the constructor of a class? */
        def isClassConstructor: Boolean

        /** Is this the super accessor? */
        @experimental // TODO when stable, remove `dotty.tools.scaladoc.tasty.ClassLikeSupport.isSuperBridgeMethod` and use this method
        def isSuperAccessor: Boolean

        /** Is this the definition of a type? */
        def isType: Boolean

        /** Is this the definition of a term? */
        def isTerm: Boolean

        /** Is this the definition of a PackageDef tree? */
        def isPackageDef: Boolean

        /** Is this the definition of a ClassDef tree? */
        def isClassDef: Boolean

        /** Is this the definition of a TypeDef tree */
        def isTypeDef: Boolean

        /** Is this the definition of a ValDef tree? */
        def isValDef: Boolean

        /** Is this the definition of a DefDef tree? */
        def isDefDef: Boolean

        /** Is this the definition of a Bind pattern? */
        def isBind: Boolean

        /** Does this symbol represent a no definition? */
        def isNoSymbol: Boolean

        /** Does this symbol represent a definition? */
        def exists: Boolean

        /** Field with the given name directly declared in the class */
        def declaredField(name: String): Symbol

        /** Fields directly declared in the class */
        def declaredFields: List[Symbol]

        /** Get named non-private fields declared or inherited */
        @deprecated("Use fieldMember", "3.1.0")
        def memberField(name: String): Symbol

        /** Get named non-private fields declared or inherited */
        def fieldMember(name: String): Symbol

        /** Get all non-private fields declared or inherited */
        @deprecated("Use fieldMembers", "3.1.0")
        def memberFields: List[Symbol]

        /** Get all non-private fields declared or inherited */
        def fieldMembers: List[Symbol]

        /** Get non-private named methods defined directly inside the class */
        def declaredMethod(name: String): List[Symbol]

        /** Get all non-private methods defined directly inside the class, excluding constructors */
        def declaredMethods: List[Symbol]

        /** Get named non-private methods declared or inherited */
        @deprecated("Use methodMember", "3.1.0")
        def memberMethod(name: String): List[Symbol]

        /** Get named non-private methods declared or inherited */
        def methodMember(name: String): List[Symbol]

        /** Get all non-private methods declared or inherited */
        @deprecated("Use methodMembers", "3.1.0")
        def memberMethods: List[Symbol]

        /** Get all non-private methods declared or inherited */
        def methodMembers: List[Symbol]

        /** Get non-private named type defined directly inside the class */
        def declaredType(name: String): List[Symbol]

        /** Get all non-private types defined directly inside the class */
        def declaredTypes: List[Symbol]

        /** Type member with the given name directly declared in the class */
        @deprecated("Use declaredType or typeMember", "3.1.0")
        def memberType(name: String): Symbol

        /** Type member with the given name declared or inherited in the class */
        def typeMember(name: String): Symbol

        /** Type member directly declared in the class */
        @deprecated("Use declaredTypes or typeMembers", "3.1.0")
        def memberTypes: List[Symbol]

        /** Type member directly declared or inherited in the class */
        def typeMembers: List[Symbol]

        /** All members directly declared in the class */
        def declarations: List[Symbol]

        /** The symbols of each type parameter list and value parameter list of this
          *  method, or Nil if this isn't a method.
          */
        def paramSymss: List[List[Symbol]]

        /** Returns all symbols overridden by this symbol. */
        def allOverriddenSymbols: Iterator[Symbol]

        /** The symbol overriding this symbol in given subclass `ofclazz`.
         *
         *  @param ofclazz is a subclass of this symbol's owner
         */
        def overridingSymbol(ofclazz: Symbol): Symbol

        /** The primary constructor of a class or trait, `noSymbol` if not applicable. */
        def primaryConstructor: Symbol

        /** Fields of a case class type -- only the ones declared in primary constructor */
        def caseFields: List[Symbol]

        /** Is this the symbol of a type parameter */
        def isTypeParam: Boolean

        /** Variance flags for of this type parameter.
         *
         *  Variance flags can be one of `Flags.{Covariant, Contravariant, EmptyFlags}`.
         *  If this is not the symbol of a type parameter the result is `Flags.EmptyFlags`.
         */
        def paramVariance: Flags

        /** Signature of this definition */
        def signature: Signature

        /** The class symbol of the companion module class */
        def moduleClass: Symbol

        /** The symbol of the companion class */
        def companionClass: Symbol

        /** The symbol of the companion module */
        def companionModule: Symbol

        /** Case class or case object children of a sealed trait or cases of an `enum`. */
        def children: List[Symbol]

        /** Returns a nested quote with this symbol as splice owner (`Symbol.spliceOwner`).
         *
         *  Changes the owner under which the definition in a quote are created.
         *
         *  Usages:
         *  ```scala
         *  def rhsExpr(using q: Quotes): Expr[Unit] =
         *    import q.reflect.*
         *    '{ val y = ???; (y, y) }
         *  def aValDef(using q: Quotes)(owner: q.reflect.Symbol) =
         *    import q.reflect.*
         *    val sym = Symbol.newVal(owner, "x", TypeRepr.of[Unit], Flags.EmptyFlags, Symbol.noSymbol)
         *    val rhs = rhsExpr(using sym.asQuotes).asTerm
         *    ValDef(sym, Some(rhs))
         *  ```
         *
         *  ```scala
         *  //{
         *  def inQuotes(using q: Quotes) = {
         *    import q.reflect.*
         *  //}
         *    new TreeMap:
         *      override def transformTerm(tree: Term)(owner: Symbol): Term =
         *        tree match
         *          case tree: Ident =>
         *            given Quotes = owner.asQuotes
         *            // Definitions contained in the quote will be owned by `owner`.
         *            // No need to use `changeOwner` in this case.
         *            '{ val x = ???; x }.asTerm
         *  //{
         *  }
         *  //}
         *  ```
         */
        def asQuotes: Nested

        /** Type reference to the symbol usable in the scope of its owner.
         *
         *  To get a reference to a symbol from a specific prefix `tp`, use `tp.select(symbol)` instead.
         *  @see TypeReprMethods.select
         *
         *  @pre symbol.isType returns true
         */
        def typeRef: TypeRef

        /** Term reference to the symbol usable in the scope of its owner.
         *
         *  @pre symbol.isType returns false
         */
        def termRef: TermRef
      end extension
    }

    ////////////////
    // SIGNATURES //
    ////////////////

    /** The signature of a method */
    type Signature <: AnyRef

    /** Module object of `type Signature`  */
    val Signature: SignatureModule

    /** Methods of the module object `val Signature` */
    trait SignatureModule { this: Signature.type =>
      /** Matches the method signature and returns its parameters and result type. */
      def unapply(sig: Signature): (List[String | Int], String)
    }

    /** Makes extension methods on `Signature` available without any imports */
    given SignatureMethods: SignatureMethods

    /** Extension methods of `Signature` */
    trait SignatureMethods {
      extension (self: Signature)

        /** The signatures of the method parameters.
          *
          *  Each *type parameter section* is represented by a single Int corresponding
          *  to the number of type parameters in the section.
          *  Each *term parameter* is represented by a String corresponding to the fully qualified
          *  name of the parameter type.
          */
        def paramSigs: List[String | Int]

        /** The signature of the result type */
        def resultSig: String

      end extension
    }

    //////////////////////////
    // STANDARD DEFINITIONS //
    //////////////////////////

    /** A value containing all standard definitions */
    val defn: defnModule

    /** Methods of the module object `val defn` */
    trait defnModule { self: defn.type =>

      /** The module symbol of root package `_root_`. */
      def RootPackage: Symbol

      /** The class symbol of root package `_root_`. */
      def RootClass: Symbol

      /** The class symbol of empty package `_root_._empty_`. */
      def EmptyPackageClass: Symbol

      /** The module symbol of package `scala`. */
      def ScalaPackage: Symbol

      /** The class symbol of package `scala`. */
      def ScalaPackageClass: Symbol

      /** The class symbol of core class `scala.Any`. */
      def AnyClass: Symbol

      /** The class symbol of core trait `scala.Matchable` */
      def MatchableClass: Symbol

      /** The class symbol of core class `scala.AnyVal`. */
      def AnyValClass: Symbol

      /** The class symbol of core class `java.lang.Object`. */
      def ObjectClass: Symbol

      /** The type symbol of core class `scala.AnyRef`. */
      def AnyRefClass: Symbol

      /** The class symbol of core class `scala.Null`. */
      def NullClass: Symbol

      /** The class symbol of core class `scala.Nothing`. */
      def NothingClass: Symbol

      /** The class symbol of primitive class `scala.Unit`. */
      def UnitClass: Symbol

      /** The class symbol of primitive class `scala.Byte`. */
      def ByteClass: Symbol

      /** The class symbol of primitive class `scala.Short`. */
      def ShortClass: Symbol

      /** The class symbol of primitive class `scala.Char`. */
      def CharClass: Symbol

      /** The class symbol of primitive class `scala.Int`. */
      def IntClass: Symbol

      /** The class symbol of primitive class `scala.Long`. */
      def LongClass: Symbol

      /** The class symbol of primitive class `scala.Float`. */
      def FloatClass: Symbol

      /** The class symbol of primitive class `scala.Double`. */
      def DoubleClass: Symbol

      /** The class symbol of primitive class `scala.Boolean`. */
      def BooleanClass: Symbol

      /** The class symbol of class `scala.String`. */
      def StringClass: Symbol

      /** The class symbol of class `java.lang.Class`. */
      def ClassClass: Symbol

      /** The class symbol of class `scala.Array`. */
      def ArrayClass: Symbol

      /** The module symbol of module `scala.Predef`. */
      def PredefModule: Symbol

      /** The method symbol of method `scala.Predef.classOf`. */
      def Predef_classOf: Symbol

      /** The module symbol of package `java.lang`. */
      def JavaLangPackage: Symbol

      /** The module symbol of module `scala.Array`. */
      def ArrayModule: Symbol

      /** The method symbol of method `apply` in class `scala.Array`. */
      def Array_apply: Symbol

      /** The method symbol of method `clone` in class `scala.Array`. */
      def Array_clone: Symbol

      /** The method symbol of method `length` in class `scala.Array`. */
      def Array_length: Symbol

      /** The method symbol of method `update` in class `scala.Array`. */
      def Array_update: Symbol

      /** A dummy class symbol that is used to indicate repeated parameters
      *  compiled by the Scala compiler.
      *
      *  @see Repeated
      */
      def RepeatedParamClass: Symbol

      /** The class symbol of class `scala.annotation.reflection.Repeated` */
      def RepeatedAnnot: Symbol

      /** The class symbol of class `scala.Option`. */
      def OptionClass: Symbol

      /** The module symbol of module `scala.None`. */
      def NoneModule: Symbol

      /** The module symbol of module `scala.Some`. */
      def SomeModule: Symbol

      /** Function-like object that maps arity to symbols for classes `scala.Product` */
      def ProductClass: Symbol

      /** Function-like object that maps arity to symbols for classes `scala.FunctionX`.
      *   -  0th element is `Function0`
      *   -  1st element is `Function1`
      *   -  ...
      *   -  Nth element is `FunctionN`
      */
      @deprecated("Use overload of `FunctionClass` with 1 or 2 arguments", "3.4")
      def FunctionClass(arity: Int, isImplicit: Boolean = false, isErased: Boolean = false): Symbol

      /** Class symbol of a function class `scala.FunctionN`.
       *
       *  @param arity the arity of the function where `0 <= arity`
       *  @return class symbol of `scala.FunctionN` where `N == arity`
       */
      def FunctionClass(arity: Int): Symbol

      /** Class symbol of a context function class `scala.FunctionN` or `scala.ContextFunctionN`.
       *
       *  @param arity the arity of the function where `0 <= arity`
       *  @param isContextual if it is a `scala.ContextFunctionN`
       *  @return class symbol of `scala.FunctionN` or `scala.ContextFunctionN` where `N == arity`
       */
      def FunctionClass(arity: Int, isContextual: Boolean): Symbol

      /** The `scala.PolyFunction` built-in trait. */
      def PolyFunctionClass: Symbol

      /** Function-like object that maps arity to symbols for classes `scala.TupleX`.
      *   -  0th element is `NoSymbol`
      *   -  1st element is `NoSymbol`
      *   -  2st element is `Tuple2`
      *   -  ...
      *   - 22nd element is `Tuple22`
      *   - 23nd element is `NoSymbol`  // TODO update when we will have more tuples
      *   - ...
      */
      def TupleClass(arity: Int): Symbol

      /** Returns `true` if `sym` is a `Tuple1`, `Tuple2`, ... `Tuple22` */
      def isTupleClass(sym: Symbol): Boolean

      /** Contains Scala primitive value classes:
      *   - Byte
      *   - Short
      *   - Int
      *   - Long
      *   - Float
      *   - Double
      *   - Char
      *   - Boolean
      *   - Unit
      */
      def ScalaPrimitiveValueClasses: List[Symbol]

      /** Contains Scala numeric value classes:
      *   - Byte
      *   - Short
      *   - Int
      *   - Long
      *   - Float
      *   - Double
      *   - Char
      */
      def ScalaNumericValueClasses: List[Symbol]

    }

    ///////////////
    //   FLAGS   //
    ///////////////

    /** Flags of a Symbol */
    type Flags

    /** Module object of `type Flags`  */
    val Flags: FlagsModule

    /** Methods of the module object `val Flags` */
    trait FlagsModule { this: Flags.type =>

      /** Is this symbol `abstract` */
      def Abstract: Flags

      /** Is this an abstract override method?
       *
       *  This corresponds to a definition declared as "abstract override def" in the source.
       *  See https://stackoverflow.com/questions/23645172/why-is-abstract-override-required-not-override-alone-in-subtrait for examples.
       */
      def AbsOverride: Flags

      /** Is this generated by Scala compiler.
       *  Corresponds to ACC_SYNTHETIC in the JVM.
       */
      def Artifact: Flags

      /** Is this symbol `case` */
      def Case: Flags

      /** Is this symbol a getter for case class parameter */
      def CaseAccessor: Flags

      /** Is this symbol a type parameter marked as contravariant `-` */
      def Contravariant: Flags

      /** Is this symbol a type parameter marked as covariant `+` */
      def Covariant: Flags

      /** Is a declared, but not defined member */
      def Deferred: Flags

      /** The empty set of flags */
      def EmptyFlags: Flags

      /** Is this symbol an enum */
      def Enum: Flags

      /** Is this symbol `erased` */
      def Erased: Flags

      /** Is this symbol exported from provided instance */
      def Exported: Flags

      /** Is this symbol a `def` defined in an `extension` */
      def ExtensionMethod: Flags

      /** Is this symbol a getter or a setter */
      def FieldAccessor: Flags

      /** Is this symbol `final` */
      def Final: Flags

      /** Is this symbol an inferable ("given") parameter */
      def Given: Flags

      /** Is this symbol a parameter with a default value? */
      def HasDefault: Flags

      /** Is this symbol `implicit` */
      def Implicit: Flags

      /** Is an infix method or type */
      def Infix: Flags

      /** Is this symbol `inline` */
      def Inline: Flags

      /** Is this symbol invisible when typechecking? */
      def Invisible: Flags

      /** Is this symbol defined in a Java class */
      def JavaDefined: Flags

      /** Is implemented as a Java static */
      def JavaStatic: Flags

      /** Is this an annotation defined in Java */
      def JavaAnnotation: Flags

      /** Is this symbol `lazy` */
      def Lazy: Flags

      /** Is this symbol local? Used in conjunction with private/private[T] to mean private[this] extends Modifier protected[this] */
      def Local: Flags

      /** Is this symbol marked as a macro. An inline method containing top level splices */
      def Macro: Flags

      /** Is this symbol `def` */
      def Method: Flags

      /** Is this symbol an object or its class (used for a ValDef or a ClassDef extends Modifier respectively) */
      def Module: Flags

      /** Is this symbol a `var` (when used on a ValDef) */
      def Mutable: Flags

      /** Trait does not have fields or initialization code. */
      def NoInits: Flags

      /** Is this symbol `opaque` */
      def Opaque: Flags

      /** Is this symbol `open` */
      def Open: Flags

      /** Is this symbol `override` */
      def Override: Flags

      /** Is this symbol a package */
      def Package: Flags

      /** Is this symbol a parameter */
      def Param: Flags

      /** Is this symbol a parameter accessor */
      def ParamAccessor: Flags

      /** Is this symbol `private` */
      def Private: Flags

      /** Is this symbol labeled private[this] */
      def PrivateLocal: Flags

      /** Is this symbol `protected` */
      def Protected: Flags

      /** Was this symbol imported from Scala2.x */
      def Scala2x: Flags

      /** Is this symbol `sealed` */
      def Sealed: Flags

      /** Is this symbol member that is assumed to be stable and realizable */
      def StableRealizable: Flags

      /** Is this symbol marked as static. Mapped to static Java member */
      @deprecated("Use JavaStatic instead", "3.3.0") def Static: Flags

      /** Is this symbol to be tagged Java Synthetic */
      def Synthetic: Flags

      /** Is this symbol a trait */
      def Trait: Flags

      /** Is a transparent inline method or trait */
      def Transparent: Flags

    }

    /** Makes extension methods on `Flags` available without any imports */
    given FlagsMethods: FlagsMethods

    /** Extension methods of `Flags` */
    trait FlagsMethods {
      extension (self: Flags)
        /** Is the given flag set a subset of this flag sets */
        def is(that: Flags): Boolean

        /** Union of the two flag sets */
        def |(that: Flags): Flags

        /** Intersection of the two flag sets */
        def &(that: Flags): Flags

        /** Shows the flags as a String */
        def show: String

      end extension
    }


    ///////////////
    // POSITIONS //
    ///////////////


    /** Position in a source file */
    type Position <: AnyRef

    /** Module object of `type Position`  */
    val Position: PositionModule

    /** Methods of the module object `val Position` */
    trait PositionModule { this: Position.type =>
      /** Position of the expansion site of the macro */
      def ofMacroExpansion: Position

      /** Create a new position in the source with the given range. The range must be contained in the file. */
      def apply(sourceFile: SourceFile, start: Int, end: Int): Position
    }

    /** Makes extension methods on `Position` available without any imports */
    given PositionMethods: PositionMethods

    /** Extension methods of `Position` */
    trait PositionMethods {
      extension (self: Position)

        /** The start offset in the source file */
        def start: Int

        /** The end offset in the source file */
        def end: Int

        /** Source file in which this position is located */
        def sourceFile: SourceFile

        /** The start line in the source file */
        def startLine: Int

        /** The end line in the source file */
        def endLine: Int

        /** The start column in the source file */
        def startColumn: Int

        /** The end column in the source file */
        def endColumn: Int

        /** Source code within the position */
        def sourceCode: Option[String]

      end extension
    }

    /** Scala source file */
    type SourceFile <: AnyRef

    /** Module object of `type SourceFile`  */
    val SourceFile: SourceFileModule

    /** Methods of the module object `val SourceFile` */
    trait SourceFileModule { this: SourceFile.type =>
      /** Returns the source file being compiled. The path is relative to the current working directory. */
      def current: SourceFile
    }

    /** Makes extension methods on `SourceFile` available without any imports */
    given SourceFileMethods: SourceFileMethods

    /** Extension methods of `SourceFile` */
    trait SourceFileMethods {
      extension (self: SourceFile)
        /** Path to this source file. May be `null` for virtual files such as in the REPL.  */
        @deprecated("Use getJPath, name, or path instead of jpath", "3.0.2")
        def jpath: java.nio.file.Path

        /** Path to this source file. May be `None` for virtual files such as in the REPL. */
        def getJPath: Option[java.nio.file.Path]

        /** Name of the source file */
        def name: String

        /** Path of the source file.
         *
         *  It does not necessarily point to a path in the filesystem, it could be the path of a virtual file.
         *  Use `getJPath` to get paths to the filesystem.
         */
        def path: String

        /** Content of this source file */
        def content: Option[String]
      end extension
    }

    ///////////////
    // REPORTING //
    ///////////////

    /** Module containing error and warning reporting. */
    val report: reportModule

    /** Methods of the module object `val report` */
    trait reportModule { self: report.type =>

      /** Report an error at the position of the macro expansion */
      def error(msg: String): Unit

      /** Report an error at the position of `expr` */
      def error(msg: String, expr: Expr[Any]): Unit

      /** Report an error message at the given position */
      def error(msg: String, pos: Position): Unit

      /** Report an error at the position of the macro expansion and throw a StopMacroExpansion */
      def errorAndAbort(msg: String): Nothing

      /** Report an error at the position of `expr` and throw a StopMacroExpansion */
      def errorAndAbort(msg: String, expr: Expr[Any]): Nothing

      /** Report an error message at the given position and throw a StopMacroExpansion */
      def errorAndAbort(msg: String, pos: Position): Nothing

      /** Report an error at the position of the macro expansion and throw a StopMacroExpansion */
      @deprecated("Use errorAndAbort", "3.1.0")
      def throwError(msg: String): Nothing

      /** Report an error at the position of `expr` and throw a StopMacroExpansion */
      @deprecated("Use errorAndAbort", "3.1.0")
      def throwError(msg: String, expr: Expr[Any]): Nothing

      /** Report an error message at the given position and throw a StopMacroExpansion */
      @deprecated("Use errorAndAbort", "3.1.0")
      def throwError(msg: String, pos: Position): Nothing

      /** Report a warning at the position of the macro expansion */
      def warning(msg: String): Unit

      /** Report a warning at the position of `expr` */
      def warning(msg: String, expr: Expr[Any]): Unit

      /** Report a warning message at the given position */
      def warning(msg: String, pos: Position): Unit

      /** Report an info at the position of the macro expansion */
      def info(msg: String): Unit

      /** Report an info message at the position of `expr` */
      def info(msg: String, expr: Expr[Any]): Unit

      /** Report an info message at the given position */
      def info(msg: String, pos: Position): Unit

    }



    ///////////////
    //   UTILS   //
    ///////////////

    /** Customizable Tree accumulator.
    *
    *  Usage:
    *  ```scala
    *  //{
    *  def inQuotes(using q: Quotes) = {
    *    import q.reflect.*
    *  //}
    *    class MyTreeAccumulator[X] extends TreeAccumulator[X] {
    *      def foldTree(x: X, tree: Tree)(owner: Symbol): X = ???
    *    }
    *  //{
    *  }
    *  //}
    *  ```
    */
    trait TreeAccumulator[X]:

      // Ties the knot of the traversal: call `foldOver(x, tree))` to dive in the `tree` node.
      def foldTree(x: X, tree: Tree)(owner: Symbol): X

      def foldTrees(x: X, trees: Iterable[Tree])(owner: Symbol): X = trees.foldLeft(x)((acc, y) => foldTree(acc, y)(owner))

      def foldOverTree(x: X, tree: Tree)(owner: Symbol): X = {
        tree match {
          case Ident(_) =>
            x
          case Select(qualifier, _) =>
            foldTree(x, qualifier)(owner)
          case This(qual) =>
            x
          case Super(qual, _) =>
            foldTree(x, qual)(owner)
          case Apply(fun, args) =>
            foldTrees(foldTree(x, fun)(owner), args)(owner)
          case TypeApply(fun, args) =>
            foldTrees(foldTree(x, fun)(owner), args)(owner)
          case Literal(const) =>
            x
          case New(tpt) =>
            foldTree(x, tpt)(owner)
          case Typed(expr, tpt) =>
            foldTree(foldTree(x, expr)(owner), tpt)(owner)
          case TypedOrTest(expr, tpt) =>
            foldTree(foldTree(x, expr)(owner), tpt)(owner)
          case NamedArg(_, arg) =>
            foldTree(x, arg)(owner)
          case Assign(lhs, rhs) =>
            foldTree(foldTree(x, lhs)(owner), rhs)(owner)
          case Block(stats, expr) =>
            foldTree(foldTrees(x, stats)(owner), expr)(owner)
          case If(cond, thenp, elsep) =>
            foldTree(foldTree(foldTree(x, cond)(owner), thenp)(owner), elsep)(owner)
          case While(cond, body) =>
            foldTree(foldTree(x, cond)(owner), body)(owner)
          case Closure(meth, tpt) =>
            foldTree(x, meth)(owner)
          case Match(selector, cases) =>
            foldTrees(foldTree(x, selector)(owner), cases)(owner)
          case Return(expr, _) =>
            foldTree(x, expr)(owner)
          case Try(block, handler, finalizer) =>
            foldTrees(foldTrees(foldTree(x, block)(owner), handler)(owner), finalizer)(owner)
          case Repeated(elems, elemtpt) =>
            foldTrees(foldTree(x, elemtpt)(owner), elems)(owner)
          case Inlined(call, bindings, expansion) =>
            foldTree(foldTrees(x, bindings)(owner), expansion)(owner)
          case vdef @ ValDef(_, tpt, rhs) =>
            val owner = vdef.symbol
            foldTrees(foldTree(x, tpt)(owner), rhs)(owner)
          case ddef @ DefDef(_, paramss, tpt, rhs) =>
            val owner = ddef.symbol
            foldTrees(foldTree(paramss.foldLeft(x)((acc, y) => foldTrees(acc, y.params)(owner)), tpt)(owner), rhs)(owner)
          case tdef @ TypeDef(_, rhs) =>
            val owner = tdef.symbol
            foldTree(x, rhs)(owner)
          case cdef @ ClassDef(_, constr, parents, self, body) =>
            val owner = cdef.symbol
            foldTrees(foldTrees(foldTrees(foldTree(x, constr)(owner), parents)(owner), self)(owner), body)(owner)
          case Import(expr, _) =>
            foldTree(x, expr)(owner)
          case Export(expr, _) =>
            foldTree(x, expr)(owner)
          case clause @ PackageClause(pid, stats) =>
            foldTrees(foldTree(x, pid)(owner), stats)(clause.symbol)
          case Inferred() => x
          case TypeIdent(_) => x
          case TypeSelect(qualifier, _) => foldTree(x, qualifier)(owner)
          case TypeProjection(qualifier, _) => foldTree(x, qualifier)(owner)
          case Singleton(ref) => foldTree(x, ref)(owner)
          case Refined(tpt, refinements) => foldTrees(foldTree(x, tpt)(owner), refinements)(owner)
          case Applied(tpt, args) => foldTrees(foldTree(x, tpt)(owner), args)(owner)
          case ByName(result) => foldTree(x, result)(owner)
          case Annotated(arg, annot) => foldTree(foldTree(x, arg)(owner), annot)(owner)
          case LambdaTypeTree(typedefs, arg) => foldTree(foldTrees(x, typedefs)(owner), arg)(owner)
          case TypeBind(_, tbt) => foldTree(x, tbt)(owner)
          case TypeBlock(typedefs, tpt) => foldTree(foldTrees(x, typedefs)(owner), tpt)(owner)
          case MatchTypeTree(boundopt, selector, cases) =>
            foldTrees(foldTree(boundopt.fold(x)(y => foldTree(x, y)(owner)), selector)(owner), cases)(owner)
          case WildcardTypeTree() => x
          case TypeBoundsTree(lo, hi) => foldTree(foldTree(x, lo)(owner), hi)(owner)
          case CaseDef(pat, guard, body) => foldTree(foldTrees(foldTree(x, pat)(owner), guard)(owner), body)(owner)
          case TypeCaseDef(pat, body) => foldTree(foldTree(x, pat)(owner), body)(owner)
          case Bind(_, body) => foldTree(x, body)(owner)
          case Unapply(fun, implicits, patterns) => foldTrees(foldTrees(foldTree(x, fun)(owner), implicits)(owner), patterns)(owner)
          case Alternatives(patterns) => foldTrees(x, patterns)(owner)
          case SummonFrom(cases) => foldTrees(x, cases)(owner)
          case _ => throw MatchError(tree.show(using Printer.TreeStructure))
        }
      }
    end TreeAccumulator


    /** Customizable tree traverser.
    *
    *  Usage:
    *  ```scala
    *  //{
    *  def inQuotes(using q: Quotes) = {
    *    import q.reflect.*
    *  //}
    *    class MyTraverser extends TreeTraverser {
    *      override def traverseTree(tree: Tree)(owner: Symbol): Unit = ???
    *    }
    *  //{
    *  }
    *  //}
    *  ```
    */
    trait TreeTraverser extends TreeAccumulator[Unit]:

      def traverseTree(tree: Tree)(owner: Symbol): Unit = traverseTreeChildren(tree)(owner)

      def foldTree(x: Unit, tree: Tree)(owner: Symbol): Unit = traverseTree(tree)(owner)

      protected def traverseTreeChildren(tree: Tree)(owner: Symbol): Unit = foldOverTree((), tree)(owner)

    end TreeTraverser

    /** Customizable tree mapper.
    *
    *  Usage:
    *  ```scala
    *  //{
    *  def inQuotes(using q: Quotes) = {
    *    import q.reflect.*
    *  //}
    *    class MyTreeMap extends TreeMap {
    *      override def transformTree(tree: Tree)(owner: Symbol): Tree = ???
    *    }
    *  //{
    *  }
    *  //}
    *  ```
    *
    *  Use `Symbol.asQuotes` to create quotes with the correct owner within the TreeMap.
    *
    */
    trait TreeMap:

      def transformTree(tree: Tree)(owner: Symbol): Tree = {
        tree match {
          case tree: PackageClause =>
            PackageClause.copy(tree)(transformTerm(tree.pid)(owner).asInstanceOf[Ref], transformTrees(tree.stats)(tree.symbol))
          case tree: Import =>
            Import.copy(tree)(transformTerm(tree.expr)(owner), tree.selectors)
          case tree: Export =>
            tree
          case tree: Statement =>
            transformStatement(tree)(owner)
          case tree: TypeTree => transformTypeTree(tree)(owner)
          case tree: TypeBoundsTree =>
            TypeBoundsTree.copy(tree)(transformTypeTree(tree.low)(owner), transformTypeTree(tree.hi)(owner))
          case tree: WildcardTypeTree => tree
          case tree: CaseDef =>
            transformCaseDef(tree)(owner)
          case tree: TypeCaseDef =>
            transformTypeCaseDef(tree)(owner)
          case pattern: Bind =>
            Bind.copy(pattern)(pattern.name, pattern.pattern)
          case pattern: Unapply =>
            Unapply.copy(pattern)(transformTerm(pattern.fun)(owner), transformSubTrees(pattern.implicits)(owner), transformTrees(pattern.patterns)(owner))
          case pattern: Alternatives =>
            Alternatives.copy(pattern)(transformTrees(pattern.patterns)(owner))
          case TypedOrTest(inner, tpt) =>
            TypedOrTest.copy(tree)(transformTree(inner)(owner), transformTypeTree(tpt)(owner))
          case _ =>
            throw MatchError(tree.show(using Printer.TreeStructure))
        }
      }

      def transformStatement(tree: Statement)(owner: Symbol): Statement = {
        tree match {
          case tree: Term =>
            transformTerm(tree)(owner)
          case tree: ValDef =>
            val owner = tree.symbol
            val tpt1 = transformTypeTree(tree.tpt)(owner)
            val rhs1 = tree.rhs.map(x => transformTerm(x)(owner))
            ValDef.copy(tree)(tree.name, tpt1, rhs1)
          case tree: DefDef =>
            val owner = tree.symbol
            val newParamClauses = tree.paramss.mapConserve {
              case TypeParamClause(params) => TypeParamClause(transformSubTrees(params)(owner))
              case TermParamClause(params) => TermParamClause(transformSubTrees(params)(owner))
            }
            DefDef.copy(tree)(tree.name, newParamClauses, transformTypeTree(tree.returnTpt)(owner), tree.rhs.map(x => transformTerm(x)(owner)))
          case tree: TypeDef =>
            val owner = tree.symbol
            TypeDef.copy(tree)(tree.name, transformTree(tree.rhs)(owner))
          case tree: ClassDef =>
            val constructor @ DefDef(_, _, _, _) = transformStatement(tree.constructor)(tree.symbol): @unchecked
            val parents = tree.parents.map(transformTree(_)(tree.symbol))
            val self = tree.self.map { slf =>
              transformStatement(slf)(tree.symbol) match
                case self: ValDef => self
            }
            val body = tree.body.map(transformStatement(_)(tree.symbol))
            ClassDef.copy(tree)(tree.name, constructor, parents, self, body)
          case tree: Import =>
            Import.copy(tree)(transformTerm(tree.expr)(owner), tree.selectors)
          case tree: Export =>
            tree
          case _ =>
            throw MatchError(tree.show(using Printer.TreeStructure))
        }
      }

      def transformTerm(tree: Term)(owner: Symbol): Term = {
        tree match {
          case Ident(name) =>
            tree
          case Select(qualifier, name) =>
            Select.copy(tree)(transformTerm(qualifier)(owner), name)
          case This(qual) =>
            tree
          case Super(qual, mix) =>
            Super.copy(tree)(transformTerm(qual)(owner), mix)
          case Apply(fun, args) =>
            Apply.copy(tree)(transformTerm(fun)(owner), transformTerms(args)(owner))
          case TypeApply(fun, args) =>
            TypeApply.copy(tree)(transformTerm(fun)(owner), transformTypeTrees(args)(owner))
          case Literal(const) =>
            tree
          case New(tpt) =>
            New.copy(tree)(transformTypeTree(tpt)(owner))
          case Typed(expr, tpt) =>
            Typed.copy(tree)(transformTerm(expr)(owner), transformTypeTree(tpt)(owner))
          case tree: NamedArg =>
            NamedArg.copy(tree)(tree.name, transformTerm(tree.value)(owner))
          case Assign(lhs, rhs) =>
            Assign.copy(tree)(transformTerm(lhs)(owner), transformTerm(rhs)(owner))
          case Block(stats, expr) =>
            Block.copy(tree)(transformStats(stats)(owner), transformTerm(expr)(owner))
          case If(cond, thenp, elsep) =>
            If.copy(tree)(transformTerm(cond)(owner), transformTerm(thenp)(owner), transformTerm(elsep)(owner))
          case Closure(meth, tpt) =>
            Closure.copy(tree)(transformTerm(meth)(owner), tpt)
          case Match(selector, cases) =>
            Match.copy(tree)(transformTerm(selector)(owner), transformCaseDefs(cases)(owner))
          case Return(expr, from) =>
            Return.copy(tree)(transformTerm(expr)(owner), from)
          case While(cond, body) =>
            While.copy(tree)(transformTerm(cond)(owner), transformTerm(body)(owner))
          case Try(block, cases, finalizer) =>
            Try.copy(tree)(transformTerm(block)(owner), transformCaseDefs(cases)(owner), finalizer.map(x => transformTerm(x)(owner)))
          case Repeated(elems, elemtpt) =>
            Repeated.copy(tree)(transformTerms(elems)(owner), transformTypeTree(elemtpt)(owner))
          case Inlined(call, bindings, expansion) =>
            Inlined.copy(tree)(call, transformSubTrees(bindings)(owner), transformTerm(expansion)(owner))
          case SummonFrom(cases) =>
            SummonFrom.copy(tree)(transformCaseDefs(cases)(owner))
          case _ =>
            throw MatchError(tree.show(using Printer.TreeStructure))
        }
      }

      def transformTypeTree(tree: TypeTree)(owner: Symbol): TypeTree = tree match {
        case Inferred() => tree
        case tree: TypeIdent => tree
        case tree: TypeSelect =>
          TypeSelect.copy(tree)(tree.qualifier, tree.name)
        case tree: TypeProjection =>
          TypeProjection.copy(tree)(tree.qualifier, tree.name)
        case tree: Annotated =>
          Annotated.copy(tree)(tree.arg, tree.annotation)
        case tree: Singleton =>
          Singleton.copy(tree)(transformTerm(tree.ref)(owner))
        case tree: Refined =>
          Refined.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.refinements)(owner).asInstanceOf[List[Definition]])
        case tree: Applied =>
          Applied.copy(tree)(transformTypeTree(tree.tpt)(owner), transformTrees(tree.args)(owner))
        case tree: MatchTypeTree =>
          MatchTypeTree.copy(tree)(tree.bound.map(b => transformTypeTree(b)(owner)), transformTypeTree(tree.selector)(owner), transformTypeCaseDefs(tree.cases)(owner))
        case tree: ByName =>
          ByName.copy(tree)(transformTypeTree(tree.result)(owner))
        case tree: LambdaTypeTree =>
          LambdaTypeTree.copy(tree)(transformSubTrees(tree.tparams)(owner), transformTree(tree.body)(owner))
        case tree: TypeBind =>
          TypeBind.copy(tree)(tree.name, tree.body)
        case tree: TypeBlock =>
          TypeBlock.copy(tree)(tree.aliases, tree.tpt)
        case _ =>
          throw MatchError(tree.show(using Printer.TreeStructure))
      }

      def transformCaseDef(tree: CaseDef)(owner: Symbol): CaseDef = {
        CaseDef.copy(tree)(transformTree(tree.pattern)(owner), tree.guard.map(x => transformTerm(x)(owner)), transformTerm(tree.rhs)(owner))
      }

      def transformTypeCaseDef(tree: TypeCaseDef)(owner: Symbol): TypeCaseDef = {
        TypeCaseDef.copy(tree)(transformTypeTree(tree.pattern)(owner), transformTypeTree(tree.rhs)(owner))
      }

      def transformStats(trees: List[Statement])(owner: Symbol): List[Statement] =
        trees mapConserve (x => transformStatement(x)(owner))

      def transformTrees(trees: List[Tree])(owner: Symbol): List[Tree] =
        trees mapConserve (x => transformTree(x)(owner))

      def transformTerms(trees: List[Term])(owner: Symbol): List[Term] =
        trees mapConserve (x => transformTerm(x)(owner))

      def transformTypeTrees(trees: List[TypeTree])(owner: Symbol): List[TypeTree] =
        trees mapConserve (x => transformTypeTree(x)(owner))

      def transformCaseDefs(trees: List[CaseDef])(owner: Symbol): List[CaseDef] =
        trees mapConserve (x => transformCaseDef(x)(owner))

      def transformTypeCaseDefs(trees: List[TypeCaseDef])(owner: Symbol): List[TypeCaseDef] =
        trees mapConserve (x => transformTypeCaseDef(x)(owner))

      def transformSubTrees[Tr <: Tree](trees: List[Tr])(owner: Symbol): List[Tr] =
        transformTrees(trees)(owner).asInstanceOf[List[Tr]]

    end TreeMap

    /** Type class used in `show` methods to provide customizable `String` representations */
    trait Printer[T]:
      /** Show the arguments as a `String` */
      def show(x: T): String
    end Printer

    /** Default pinter for `Tree` used when calling `tree.show` */
    given TreePrinter: Printer[Tree] = Printer.TreeCode

    /** Default pinter for `TypeRepr` used when calling `tpe.show` */
    given TypeReprPrinter: Printer[TypeRepr] = Printer.TypeReprCode

    /** Default pinter for `Constant` used when calling `const.show` */
    given ConstantPrinter: Printer[Constant] = Printer.ConstantCode

    /** Module object of `type Printer`.
     *  Contains custom printers such as `TreeCode`, `TreeAnsiCode`, `TreeCases`, `TypeReprCode`, ..., `SymbolFullName` and `FlagsCombination`.
     */
    val Printer: PrinterModule

    /** Methods of the module object `val Printer` */
    trait PrinterModule { self: Printer.type =>
      /** Prints fully elaborated version of the source code. */
      def TreeCode: Printer[Tree]

      /** Prints fully elaborated version of the source code.
       *  Same as `TreeCode` but does not print full package prefixes.
       */
      def TreeShortCode: Printer[Tree]

      /** Prints fully elaborated version of the source code using ANSI colors. */
      def TreeAnsiCode: Printer[Tree]

      /** Prints a pattern like representation of the `Tree`.
       *  It displays the structure of the AST.
       */
      def TreeStructure: Printer[Tree]

      /** Prints the type in source code. */
      def TypeReprCode: Printer[TypeRepr]

      /** Prints the type in source code.
       *  Same as `TypeReprCode` but does not print full package prefixes.
       */
      def TypeReprShortCode: Printer[TypeRepr]

      /** Prints the type in source code using ANSI colors. */
      def TypeReprAnsiCode: Printer[TypeRepr]

      /** Prints a pattern like representation of the `TypeRepr`.
       *  It displays the structure of the type.
       */
      def TypeReprStructure: Printer[TypeRepr]

      /** Prints the constant in source code. */
      def ConstantCode: Printer[Constant]

      /** Prints a pattern like representation of the `Constant`. */
      def ConstantStructure: Printer[Constant]
    }

  }

  /** Type of a `Quotes` provided by a splice within a quote that took this context. */
  type Nested = Quotes

}

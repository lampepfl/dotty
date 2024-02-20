package dotty.tools.tasty

/************************************************************
Notation:

We use BNF notation. Terminal symbols start with at least two
consecutive upper case letters. Each terminal is represented as a
single byte tag. Non-terminals are mixed case. Prefixes of the form
lower case letter*_ are for explanation of semantic content only, they
can be dropped without changing the grammar.

Micro-syntax:
```none
  LongInt       = Digit* StopDigit        -- big endian 2's complement, value fits in a Long w/o overflow
  Int           = LongInt                 -- big endian 2's complement, fits in an Int w/o overflow
  Nat           = LongInt                 -- non-negative value, fits in an Int without overflow
  Digit         = 0 | ... | 127
  StopDigit     = 128 | ... | 255         -- value = digit - 128
  Utf8          = Nat UTF8-CodePoint*
```

Macro-format:
```none
  File          = Header majorVersion_Nat minorVersion_Nat experimentalVersion_Nat VersionString UUID
                  nameTable_Length Name* Section*
  Header        = 0x5CA1AB1F
  UUID          = Byte*16                 -- random UUID
  VersionString = Utf8                    -- string that represents the compiler that produced the TASTy

  Section       = NameRef Length Bytes
  Length        = Nat                     -- length of rest of entry in bytes

  Name          = UTF8              Utf8
                  QUALIFIED         Length qualified_NameRef selector_NameRef               -- A.B
                  EXPANDED          Length qualified_NameRef selector_NameRef               -- A$$B, semantically a NameKinds.ExpandedName
                  EXPANDPREFIX      Length qualified_NameRef selector_NameRef               -- A$B, prefix of expanded name, see NamedKinds.ExpandPrefixName

                  UNIQUE            Length separator_NameRef uniqid_Nat underlying_NameRef? -- Unique name A<separator><number>
                  DEFAULTGETTER     Length underlying_NameRef index_Nat                     -- DefaultGetter$<number>

                  SUPERACCESSOR     Length underlying_NameRef                               -- super$A
                  INLINEACCESSOR    Length underlying_NameRef                               -- inline$A
                  OBJECTCLASS       Length underlying_NameRef                               -- A$  (name of the module class for module A)
                  BODYRETAINER      Length underlying_NameRef                               -- A$retainedBody

                  SIGNED            Length original_NameRef resultSig_NameRef ParamSig*     -- name + signature
                  TARGETSIGNED      Length original_NameRef target_NameRef resultSig_NameRef ParamSig*

  ParamSig      = Int // If negative, the absolute value represents the length of a type parameter section
                      // If positive, this is a NameRef for the fully qualified name of a term parameter.

  NameRef       = Nat                    // ordinal number of name in name table, starting from 1.
  Utf8Ref       = Nat                    // ordinal number of UTF8 in name table, starting from 1.
```

Note: Unqualified names in the name table are strings. The context decides whether a name is
a type-name or a term-name. The same string can represent both.


Standard-Section: "ASTs" TopLevelStat*
```none
  TopLevelStat  = PACKAGE        Length Path TopLevelStat*                         -- package path { topLevelStats }
                  Stat

  Stat          = Term
                  ValOrDefDef
                  TYPEDEF        Length NameRef (type_Term | Template) Modifier*   -- modifiers type name (= type | bounds)  |  modifiers class name template
                  IMPORT         Length qual_Term Selector*                        -- import qual selectors
                  EXPORT         Length qual_Term Selector*                        -- export qual selectors
  ValOrDefDef   = VALDEF         Length NameRef type_Term rhs_Term? Modifier*      -- modifiers val name : type (= rhs)?
                  DEFDEF         Length NameRef Param* returnType_Term rhs_Term?
                                        Modifier*                                  -- modifiers def name [typeparams] paramss : returnType (= rhs)?
  Selector      = IMPORTED              name_NameRef                               -- name, "_" for normal wildcards, "" for given wildcards
                  RENAMED               to_NameRef                                 -- => name
                  BOUNDED               type_Term                                  -- type bound

  TypeParam     = TYPEPARAM      Length NameRef type_Term Modifier*                -- modifiers name bounds
  TermParam     = PARAM          Length NameRef type_Term Modifier*                -- modifiers name : type.
                  EMPTYCLAUSE                                                      -- an empty parameter clause ()
                  SPLITCLAUSE                                                      -- splits two non-empty parameter clauses of the same kind
  Param         = TypeParam
                  TermParam
  Template      = TEMPLATE       Length TypeParam* TermParam* parent_Term* Self?
                                        EndParents? Stat*                          -- [typeparams] paramss extends parents { self => stats }, where Stat* always starts with the primary constructor.
  Self          = SELFDEF               selfName_NameRef selfType_Term             -- selfName : selfType
  EndParents    = SPLITCLAUSE                                                      -- explicitly end the template header, e.g. if there is no primary constructor

  Term          = Path                                                             -- Paths represent both types and terms
                  IDENT                 NameRef Type                               -- Used when term ident’s type is not a TermRef
                  SELECT                possiblySigned_NameRef qual_Term           -- qual.name
                  SELECTin       Length possiblySigned_NameRef qual_Term owner_Type -- qual.name, referring to a symbol declared in owner that has the given signature (see note below)
                  QUALTHIS              typeIdent_Tree                             -- id.this, different from THIS in that it contains a qualifier ident with position.
                  NEW                   clsType_Term                               -- new cls
                  ELIDED                exprType_Type                              -- elided expression of the given type
                  THROW                 throwableExpr_Term                         -- throw throwableExpr
                  NAMEDARG              paramName_NameRef arg_Term                 -- paramName = arg
                  APPLY          Length fn_Term arg_Term*                          -- fn(args)
                  APPLYsigpoly   Length fn_Term meth_Type arg_Term*                -- The application of a signature-polymorphic method
                  TYPEAPPLY      Length fn_Term arg_Type*                          -- fn[args]
                  SUPER          Length this_Term mixinTypeIdent_Tree?             -- super[mixin]
                  TYPED          Length expr_Term ascriptionType_Term              -- expr: ascription
                  ASSIGN         Length lhs_Term rhs_Term                          -- lhs = rhs
                  BLOCK          Length expr_Term Stat*                            -- { stats; expr }
                  INLINED        Length expr_Term call_Term? ValOrDefDef*          -- Inlined code from call, with given body `expr` and given bindings
                  LAMBDA         Length meth_Term target_Type?                     -- Closure over method `f` of type `target` (omitted id `target` is a function type)
                  IF             Length [INLINE] cond_Term then_Term else_Term     -- inline? if cond then thenPart else elsePart
                  MATCH          Length (IMPLICIT | [INLINE] sel_Term) CaseDef*    -- (inline? sel | implicit) match caseDefs
                  TRY            Length expr_Term CaseDef* finalizer_Term?         -- try expr catch {casdeDef} (finally finalizer)?
                  RETURN         Length meth_ASTRef expr_Term?                     -- return expr?,  `methASTRef` is method from which is returned
                  WHILE          Length cond_Term body_Term                        -- while cond do body
                  REPEATED       Length elem_Type elem_Term*                       -- Varargs argument of type `elem`
                  SELECTouter    Length levels_Nat qual_Term underlying_Type       -- Follow `levels` outer links, starting from `qual`, with given `underlying` type
    -- patterns:
                  BIND           Length boundName_NameRef patType_Type pat_Term    -- name @ pat, wherev `patType` is the type of the bound symbol
                  ALTERNATIVE    Length alt_Term*                                  -- alt1 | ... | altn   as a pattern
                  UNAPPLY        Length fun_Term ImplicitArg* pat_Type pat_Term*   -- Unapply node `fun(_: pat_Type)(implicitArgs)` flowing into patterns `pat`.
    -- type trees:
                  IDENTtpt              NameRef Type                               -- Used for all type idents
                  SELECTtpt             NameRef qual_Term                          -- qual.name
                  SINGLETONtpt          ref_Term                                   -- ref.type
                  REFINEDtpt     Length underlying_Term refinement_Stat*           -- underlying {refinements}
                  APPLIEDtpt     Length tycon_Term arg_Term*                       -- tycon [args]
                  LAMBDAtpt      Length TypeParam* body_Term                       -- [TypeParams] => body
                  TYPEBOUNDStpt  Length low_Term high_Term?                        -- >: low <: high
                  ANNOTATEDtpt   Length underlying_Term fullAnnotation_Term        -- underlying @ annotation
                  MATCHtpt       Length bound_Term? sel_Term CaseDef*              -- sel match { CaseDef } where `bound` is optional upper bound of all rhs
                  BYNAMEtpt             underlying_Term                            -- => underlying
                  SHAREDterm            term_ASTRef                                -- Link to previously serialized term
    -- pickled quote trees:                                                        -- These trees can only appear in pickled quotes. They will never be in a TASTy file.
                  EXPLICITtpt           tpt_Term                                   -- Tag for a type tree that in a context where it is not explicitly known that this tree is a type.
                  HOLE           Length idx_Nat tpe_Type arg_Tree*                 -- Splice hole with index `idx`, the type of the hole `tpe`, type and term arguments of the hole `arg`s


  CaseDef       = CASEDEF        Length pat_Term rhs_Tree guard_Tree?              -- case pat if guard => rhs
  ImplicitArg   = IMPLICITARG           arg_Term                                   -- implicit unapply argument

  ASTRef        = Nat                                                              -- Byte position in AST payload

  Path          = Constant
                  TERMREFdirect         sym_ASTRef                                 -- A reference to a local symbol (without a prefix). Reference is to definition node of symbol.
                  TERMREFsymbol         sym_ASTRef qual_Type                       -- A reference `qual.sym` to a local member with prefix `qual`
                  TERMREFpkg            fullyQualified_NameRef                     -- A reference to a package member with given fully qualified name
                  TERMREF               possiblySigned_NameRef qual_Type           -- A reference `qual.name` to a non-local member
                  TERMREFin      Length possiblySigned_NameRef qual_Type owner_Type -- A reference `qual.name` referring to a non-local symbol declared in owner that has the given signature (see note below)
                  THIS                  clsRef_Type                                -- cls.this
                  RECthis               recType_ASTRef                             -- The `this` in a recursive refined type `recType`.
                  SHAREDtype            path_ASTRef                                -- link to previously serialized path

  Constant      = UNITconst                                                        -- ()
                  FALSEconst                                                       -- false
                  TRUEconst                                                        -- true
                  BYTEconst             Int                                        -- A byte number
                  SHORTconst            Int                                        -- A short number
                  CHARconst             Nat                                        -- A character
                  INTconst              Int                                        -- An int number
                  LONGconst             LongInt                                    -- A long number
                  FLOATconst            Int                                        -- A float number
                  DOUBLEconst           LongInt                                    -- A double number
                  STRINGconst           NameRef                                    -- A string literal
                  NULLconst                                                        -- null
                  CLASSconst            Type                                       -- classOf[Type]

  Type          = Path                                                             -- Paths represent both types and terms
                  TYPEREFdirect         sym_ASTRef                                 -- A reference to a local symbol (without a prefix). Reference is to definition node of symbol.
                  TYPEREFsymbol         sym_ASTRef qual_Type                       -- A reference `qual.sym` to a local member with prefix `qual`
                  TYPEREFpkg            fullyQualified_NameRef                     -- A reference to a package member with given fully qualified name
                  TYPEREF               NameRef qual_Type                          -- A reference `qual.name` to a non-local member
                  TYPEREFin      Length NameRef qual_Type namespace_Type           -- A reference `qual.name` to a non-local member that's private in `namespace`.
                  RECtype               parent_Type                                -- A wrapper for recursive refined types
                  SUPERtype      Length this_Type underlying_Type                  -- A super type reference to `underlying`
                  REFINEDtype    Length refinement_NameRef underlying_Type info_Type -- underlying { refinement_name : info }
                  APPLIEDtype    Length tycon_Type arg_Type*                       -- tycon[args]
                  TYPEBOUNDS     Length lowOrAlias_Type high_Type? Variance*       -- = alias or >: low <: high, possibly with variances of lambda parameters
                  ANNOTATEDtype  Length underlying_Type annotation_Term            -- underlying @ annotation
                  ANDtype        Length left_Type right_Type                       -- left & right
                  ORtype         Length left_Type right_Type                       -- lefgt | right
                  MATCHtype      Length bound_Type sel_Type case_Type*             -- sel match {cases} with optional upper `bound`
                  MATCHCASEtype  Length pat_type rhs_Type                          -- match cases are MATCHCASEtypes or TYPELAMBDAtypes over MATCHCASEtypes
                  BIND           Length boundName_NameRef bounds_Type Modifier*    -- boundName @ bounds,  for type-variables defined in a type pattern
                  BYNAMEtype            underlying_Type                            -- => underlying
                  PARAMtype      Length binder_ASTRef paramNum_Nat                 -- A reference to parameter # paramNum in lambda type `binder`
                  POLYtype       Length result_Type TypesNames                     -- A polymorphic method type `[TypesNames]result`, used in refinements
                  METHODtype     Length result_Type TypesNames Modifier*           -- A method type `(Modifier* TypesNames)result`, needed for refinements, with optional modifiers for the parameters
                  TYPELAMBDAtype Length result_Type TypesNames                     -- A type lambda `[TypesNames] => result`
                  SHAREDtype            type_ASTRef                                -- link to previously serialized type
  TypesNames    = TypeName*
  TypeName      = typeOrBounds_ASTRef paramName_NameRef                            -- (`termName`: `type`)  or  (`typeName` `bounds`)

  Modifier      = PRIVATE                                                          -- private
                  PROTECTED                                                        -- protected
                  PRIVATEqualified     qualifier_Type                              -- private[qualifier]    (to be dropped(?)
                  PROTECTEDqualified   qualifier_Type                              -- protecred[qualifier]  (to be dropped(?)
                  ABSTRACT                                                         -- abstract
                  FINAL                                                            -- final
                  SEALED                                                           -- sealed
                  CASE                                                             -- case  (for classes or objects)
                  IMPLICIT                                                         -- implicit
                  GIVEN                                                            -- given
                  ERASED                                                           -- erased
                  LAZY                                                             -- lazy
                  OVERRIDE                                                         -- override
                  OPAQUE                                                           -- opaque, also used for classes containing opaque aliases
                  INLINE                                                           -- inline
                  MACRO                                                            -- Inline method containing toplevel splices
                  INLINEPROXY                                                      -- Symbol of binding with an argument to an inline method as rhs (TODO: do we still need this?)
                  STATIC                                                           -- Mapped to static Java member
                  OBJECT                                                           -- An object or its class
                  TRAIT                                                            -- A trait
                  ENUM                                                             -- A enum class or enum case
                  LOCAL                                                            -- private[this] or protected[this], used in conjunction with PRIVATE or PROTECTED
                  SYNTHETIC                                                        -- Generated by Scala compiler
                  ARTIFACT                                                         -- To be tagged Java Synthetic
                  MUTABLE                                                          -- A var
                  FIELDaccessor                                                    -- A getter or setter (note: the corresponding field is not serialized)
                  CASEaccessor                                                     -- A getter for a case class parameter
                  COVARIANT                                                        -- A type parameter marked “+”
                  CONTRAVARIANT                                                    -- A type parameter marked “-”
                  HASDEFAULT                                                       -- Parameter with default arg; method with default parameters (default arguments are separate methods with DEFAULTGETTER names)
                  STABLE                                                           -- Method that is assumed to be stable, i.e. its applications are legal paths
                  EXTENSION                                                        -- An extension method
                  PARAMsetter                                                      -- The setter part `x_=` of a var parameter `x` which itself is pickled as a PARAM
                  PARAMalias                                                       -- Parameter is alias of a superclass parameter
                  EXPORTED                                                         -- An export forwarder
                  OPEN                                                             -- an open class
                  INVISIBLE                                                        -- invisible during typechecking
                  Annotation

  Variance      = STABLE                                                           -- invariant
                | COVARIANT
                | CONTRAVARIANT

  Annotation    = ANNOTATION     Length tycon_Type fullAnnotation_Term             -- An annotation, given (class) type of constructor, and full application tree
```

Note: The signature of a SELECTin or TERMREFin node is the signature of the selected symbol,
      not the signature of the reference. The latter undergoes an asSeenFrom but the former
      does not.

Note: Tree tags are grouped into 5 categories that determine what follows, and thus allow to compute the size of the tagged tree in a generic way.
```none
  Tree Category 1 (tags 1-59)   :  tag
  Tree Category 2 (tags 60-89)  :  tag Nat
  Tree Category 3 (tags 90-109) :  tag AST
  Tree Category 4 (tags 110-127):  tag Nat AST
  Tree Category 5 (tags 128-255):  tag Length <payload>
```

Standard-Section: "Positions" LinesSizes Assoc*
```none
  LinesSizes    = Nat Nat*                 // Number of lines followed by the size of each line not counting the trailing `\n`

  Assoc         = Header offset_Delta? offset_Delta? point_Delta?
                | SOURCE nameref_Int
  Header        = addr_Delta +              // in one Nat: difference of address to last recorded node << 3 +
                  hasStartDiff +            // one bit indicating whether there follows a start address delta << 2
                  hasEndDiff +              // one bit indicating whether there follows an end address delta << 1
                  hasPoint                  // one bit indicating whether the new position has a point (i.e ^ position)
                                            // Nodes which have the same positions as their parents are omitted.
                                            // offset_Deltas give difference of start/end offset wrt to the
                                            // same offset in the previously recorded node (or 0 for the first recorded node)
  Delta         = Int                       // Difference between consecutive offsets,
  SOURCE        = 4                         // Impossible as header, since addr_Delta = 0 implies that we refer to the
                                            // same tree as the previous one, but then hasStartDiff = 1 implies that
                                            // the tree's range starts later than the range of itself.
```

All elements of a position section are serialized as Ints


Standard Section: "Comments" Comment*
```none
  Comment       = Utf8 LongInt              // Raw comment's bytes encoded as UTF-8, followed by the comment's coordinates.
```

Standard Section: "Attributes" Attribute*
```none
  Attribute     = SCALA2STANDARDLIBRARYattr
                  EXPLICITNULLSattr
                  CAPTURECHECKEDattr
                  WITHPUREFUNSattr
                  JAVAattr
                  OUTLINEattr
                  SOURCEFILEattr Utf8Ref
```
Attribute tags cannot be repeated in an attribute section. Attributes are ordered by the tag ordinal.

Note: Attribute tags are grouped into categories that determine what follows, and thus allow to compute the size of the tagged tree in a generic way.
      Unassigned categories can be used to extend and existing category or to add new kinds of attributes
```none
  Attribute Category 1 (tags 1-32)  :  tag
  Attribute Category 2 (tags 33-128): // not assigned yet
  Attribute Category 3 (tags 129-160):  tag Utf8Ref
  Attribute Category 4 (tags 161-255): // not assigned yet
```

**************************************************************************************/

object TastyFormat {

  /** The first four bytes of a TASTy file, followed by four values:
    * - `MajorVersion: Int` - see definition in `TastyFormat`
    * - `MinorVersion: Int` - see definition in `TastyFormat`
    * - `ExperimentalVersion: Int` - see definition in `TastyFormat`
    * - `ToolingVersion: String` - arbitrary length string representing the tool that produced the TASTy.
    */
  final val header: Array[Int] = Array(0x5C, 0xA1, 0xAB, 0x1F)

  /** Natural number. Each increment of the `MajorVersion` begins a
   *  new series of backward compatible TASTy versions.
   *
   *  A TASTy file in either the preceeding or succeeding series is
   *  incompatible with the current value.
   */
  final val MajorVersion: Int = 28

  /** Natural number. Each increment of the `MinorVersion`, within
   *  a series declared by the `MajorVersion`, breaks forward
   *  compatibility, but remains backwards compatible, with all
   *  preceding `MinorVersion`.
   */
  final val MinorVersion: Int = 5

  /** Natural Number. The `ExperimentalVersion` allows for
   *  experimentation with changes to TASTy without committing
   *  to any guarantees of compatibility.
   *
   *  A zero value indicates that the TASTy version is from a
   *  stable, final release.
   *
   *  A strictly positive value indicates that the TASTy
   *  version is experimental. An experimental TASTy file
   *  can only be read by a tool with the same version.
   *  However, tooling with an experimental TASTy version
   *  is able to read final TASTy documents if the file's
   *  `MinorVersion` is strictly less than the current value.
   */
  final val ExperimentalVersion: Int = 1

  /**This method implements a binary relation (`<:<`) between two TASTy versions.
   *
   * We label the lhs `file` and rhs `compiler`.
   * if `file <:< compiler` then the TASTy file is valid to be read.
   *
   * A TASTy version, e.g. `v := 28.0-3` is composed of three fields:
   *   - v.major == 28
   *   - v.minor == 0
   *   - v.experimental == 3
   *
   * TASTy versions have a partial order, for example,
   * `a <:< b` and `b <:< a` are both false if
   *   - `a` and `b` have different `major` fields.
   *   - `a` and `b` have the same `major` & `minor` fields,
   *     but different `experimental` fields, both non-zero.
   *
   * A TASTy version with a zero value for its `experimental` field
   * is considered to be stable. Files with a stable TASTy version
   * can be read by a compiler with an unstable TASTy version,
   * (where the compiler's TASTy version has a higher `minor` field).
   *
   * A compiler with a stable TASTy version can never read a file
   * with an unstable TASTy version.
   *
   * We follow the given algorithm:
   *
   * ```
   * (fileMajor, fileMinor, fileExperimental) match
   *   case (`compilerMajor`, `compilerMinor`, `compilerExperimental`) => true // full equality
   *   case (`compilerMajor`, minor, 0) if minor < compilerMinor       => true // stable backwards compatibility
   *   case _                                                          => false
   * ```
   * @syntax markdown
   */
  def isVersionCompatible(
    fileMajor: Int,
    fileMinor: Int,
    fileExperimental: Int,
    compilerMajor: Int,
    compilerMinor: Int,
    compilerExperimental: Int
  ): Boolean = (
    fileMajor == compilerMajor &&
      (  fileMinor == compilerMinor && fileExperimental == compilerExperimental // full equality
      || fileMinor <  compilerMinor && fileExperimental == 0 // stable backwards compatibility
    )
  )

  final val ASTsSection = "ASTs"
  final val PositionsSection = "Positions"
  final val CommentsSection = "Comments"
  final val AttributesSection = "Attributes"

  /** Tags used to serialize names, should update [[TastyFormat$.nameTagToString]] if a new constant is added */
  class NameTags {
    final val UTF8 = 1               // A simple name in UTF8 encoding.

    final val QUALIFIED = 2          // A fully qualified name `<prefix>.<suffix>`.

    final val EXPANDED = 3           // An expanded name `<prefix>$$<suffix>`,
                                     // used by Scala-2 for private names.

    final val EXPANDPREFIX = 4       // An expansion prefix `<prefix>$<suffix>`,
                                     // used by Scala-2 for private names.

    final val UNIQUE = 10            // A unique name `<name>$<num>` where `<num>`
                                     // is used only once for each `<name>`.

    final val DEFAULTGETTER = 11     // The name `<meth-name>$default$<param-num>`
                                     // of a default getter that returns a default argument.

    final val SUPERACCESSOR = 20     // The name of a super accessor `super$name` created by SuperAccesors.

    final val INLINEACCESSOR = 21    // The name of an inline accessor `inline$name`

    final val BODYRETAINER = 22      // The name of a synthetic method that retains the runtime
                                     // body of an inline method

    final val OBJECTCLASS = 23       // The name of an object class (or: module class) `<name>$`.

    final val SIGNED = 63            // A pair of a name and a signature, used to identify
                                     // possibly overloaded methods.

    final val TARGETSIGNED = 62      // A triple of a name, a targetname and a signature, used to identify
                                     // possibly overloaded methods that carry a @targetName annotation.

    // TODO swap SIGNED and TARGETSIGNED codes on next major version bump
  }
  object NameTags extends NameTags

  /**Should be kept in sync with [[NameTags]]. Converts constants to a String representing their identifier,
   * or NotANameTag(tag) if unrecognised.
   *
   * For debugging purposes when unpickling names in a TASTy file.
   */
  def nameTagToString(tag: Int) = {
    import NameTags._
    tag match {
      case UTF8 => "UTF8"
      case QUALIFIED => "QUALIFIED"
      case EXPANDED => "EXPANDED"
      case EXPANDPREFIX => "EXPANDPREFIX"
      case UNIQUE => "UNIQUE"
      case DEFAULTGETTER => "DEFAULTGETTER"
      case SUPERACCESSOR => "SUPERACCESSOR"
      case INLINEACCESSOR => "INLINEACCESSOR"
      case BODYRETAINER => "BODYRETAINER"
      case OBJECTCLASS => "OBJECTCLASS"
      case SIGNED => "SIGNED"
      case TARGETSIGNED => "TARGETSIGNED"
      case id => s"NotANameTag($id)"
    }
  }

  // Position header

  final val SOURCE = 4

  // AST tags

  // Tree Cat. 1:    tag
  final val firstSimpleTreeTag = UNITconst
  // final val ??? = 1
  final val UNITconst = 2
  final val FALSEconst = 3
  final val TRUEconst = 4
  final val NULLconst = 5
  final val PRIVATE = 6
  // final val ??? = 7
  final val PROTECTED = 8
  final val ABSTRACT = 9
  final val FINAL = 10
  final val SEALED = 11
  final val CASE = 12
  final val IMPLICIT = 13
  final val LAZY = 14
  final val OVERRIDE = 15
  final val INLINEPROXY = 16
  final val INLINE = 17
  final val STATIC = 18
  final val OBJECT = 19
  final val TRAIT = 20
  final val ENUM = 21
  final val LOCAL = 22
  final val SYNTHETIC = 23
  final val ARTIFACT = 24
  final val MUTABLE = 25
  final val FIELDaccessor = 26
  final val CASEaccessor = 27
  final val COVARIANT = 28
  final val CONTRAVARIANT = 29
  // final val ??? = 30
  final val HASDEFAULT = 31
  final val STABLE = 32
  final val MACRO = 33
  final val ERASED = 34
  final val OPAQUE = 35
  final val EXTENSION = 36
  final val GIVEN = 37
  final val PARAMsetter = 38
  final val EXPORTED = 39
  final val OPEN = 40
  final val PARAMalias = 41
  final val TRANSPARENT = 42
  final val INFIX = 43
  final val INVISIBLE = 44
  final val EMPTYCLAUSE = 45
  final val SPLITCLAUSE = 46

  // Tree Cat. 2:    tag Nat
  final val firstNatTreeTag = SHAREDterm
  final val SHAREDterm = 60
  final val SHAREDtype = 61
  final val TERMREFdirect = 62
  final val TYPEREFdirect = 63
  final val TERMREFpkg = 64
  final val TYPEREFpkg = 65
  final val RECthis = 66
  final val BYTEconst = 67
  final val SHORTconst = 68
  final val CHARconst = 69
  final val INTconst = 70
  final val LONGconst = 71
  final val FLOATconst = 72
  final val DOUBLEconst = 73
  final val STRINGconst = 74
  final val IMPORTED = 75
  final val RENAMED = 76

  // Tree Cat. 3:    tag AST
  final val firstASTTreeTag = THIS
  final val THIS = 90
  final val QUALTHIS = 91
  final val CLASSconst = 92
  final val BYNAMEtype = 93
  final val BYNAMEtpt = 94
  final val NEW = 95
  final val THROW = 96
  final val IMPLICITarg = 97
  final val PRIVATEqualified = 98
  final val PROTECTEDqualified = 99
  final val RECtype = 100
  final val SINGLETONtpt = 101
  final val BOUNDED = 102
  final val EXPLICITtpt = 103
  final val ELIDED = 104


  // Tree Cat. 4:    tag Nat AST
  final val firstNatASTTreeTag = IDENT
  final val IDENT = 110
  final val IDENTtpt = 111
  final val SELECT = 112
  final val SELECTtpt = 113
  final val TERMREFsymbol = 114
  final val TERMREF = 115
  final val TYPEREFsymbol = 116
  final val TYPEREF = 117
  final val SELFDEF = 118
  final val NAMEDARG = 119

  // Tree Cat. 5:    tag Length ...
  final val firstLengthTreeTag = PACKAGE
  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAM = 134
  // final val ??? = 135
  final val APPLY = 136
  final val TYPEAPPLY = 137
  final val TYPED = 138
  final val ASSIGN = 139
  final val BLOCK = 140
  final val IF = 141
  final val LAMBDA = 142
  final val MATCH = 143
  final val RETURN = 144
  final val WHILE = 145
  final val TRY = 146
  final val INLINED = 147
  final val SELECTouter = 148
  final val REPEATED = 149
  final val BIND = 150
  final val ALTERNATIVE = 151
  final val UNAPPLY = 152
  final val ANNOTATEDtype = 153
  final val ANNOTATEDtpt = 154
  final val CASEDEF = 155
  final val TEMPLATE = 156
  final val SUPER = 157
  final val SUPERtype = 158
  final val REFINEDtype = 159
  final val REFINEDtpt = 160
  final val APPLIEDtype = 161
  final val APPLIEDtpt = 162
  final val TYPEBOUNDS = 163
  final val TYPEBOUNDStpt = 164
  final val ANDtype = 165
  // final val ??? = 166
  final val ORtype = 167
  // final val ??? = 168
  final val POLYtype = 169
  final val TYPELAMBDAtype = 170
  final val LAMBDAtpt = 171
  final val PARAMtype = 172
  final val ANNOTATION = 173
  final val TERMREFin = 174
  final val TYPEREFin = 175
  final val SELECTin = 176
  final val EXPORT = 177
  // final val ??? = 178
  // final val ??? = 179
  final val METHODtype = 180
  final val APPLYsigpoly = 181

  final val MATCHtype = 190
  final val MATCHtpt = 191
  final val MATCHCASEtype = 192

  final val HOLE = 255

  // Attributes tags

  // Attribute Category 1 (tags 1-32)  :  tag
  def isBooleanAttrTag(tag: Int): Boolean = 1 <= tag && tag <= 32
  final val SCALA2STANDARDLIBRARYattr = 1
  final val EXPLICITNULLSattr = 2
  final val CAPTURECHECKEDattr = 3
  final val WITHPUREFUNSattr = 4
  final val JAVAattr = 5
  final val OUTLINEattr = 6

  // Attribute Category 2 (tags 33-128): unassigned

  // Attribute Category 3 (tags 129-160):  tag Utf8Ref
  def isStringAttrTag(tag: Int): Boolean = 129 <= tag && tag <= 160
  final val SOURCEFILEattr = 129

  // Attribute Category 4 (tags 161-255): unassigned

  // end of Attributes tags


  /** Useful for debugging */
  def isLegalTag(tag: Int): Boolean =
    firstSimpleTreeTag <= tag && tag <= SPLITCLAUSE ||
    firstNatTreeTag <= tag && tag <= RENAMED ||
    firstASTTreeTag <= tag && tag <= BOUNDED ||
    firstNatASTTreeTag <= tag && tag <= NAMEDARG ||
    firstLengthTreeTag <= tag && tag <= MATCHCASEtype ||
    tag == HOLE

  def isParamTag(tag: Int): Boolean = tag == PARAM || tag == TYPEPARAM

  def isModifierTag(tag: Int): Boolean = tag match {
    case PRIVATE
       | PROTECTED
       | ABSTRACT
       | FINAL
       | SEALED
       | CASE
       | IMPLICIT
       | GIVEN
       | ERASED
       | LAZY
       | OVERRIDE
       | INLINE
       | INLINEPROXY
       | MACRO
       | OPAQUE
       | STATIC
       | OBJECT
       | TRAIT
       | TRANSPARENT
       | INFIX
       | ENUM
       | LOCAL
       | SYNTHETIC
       | ARTIFACT
       | MUTABLE
       | FIELDaccessor
       | CASEaccessor
       | COVARIANT
       | CONTRAVARIANT
       | HASDEFAULT
       | STABLE
       | EXTENSION
       | PARAMsetter
       | PARAMalias
       | EXPORTED
       | OPEN
       | INVISIBLE
       | ANNOTATION
       | PRIVATEqualified
       | PROTECTEDqualified => true
    case _ => false
  }

  def isTypeTreeTag(tag: Int): Boolean = tag match {
    case IDENTtpt
       | SELECTtpt
       | SINGLETONtpt
       | REFINEDtpt
       | APPLIEDtpt
       | LAMBDAtpt
       | TYPEBOUNDStpt
       | ANNOTATEDtpt
       | BYNAMEtpt
       | MATCHtpt
       | EXPLICITtpt
       | BIND => true
    case _ => false
  }

  def astTagToString(tag: Int): String = tag match {
    case UNITconst => "UNITconst"
    case FALSEconst => "FALSEconst"
    case TRUEconst => "TRUEconst"
    case NULLconst => "NULLconst"
    case PRIVATE => "PRIVATE"
    case PROTECTED => "PROTECTED"
    case ABSTRACT => "ABSTRACT"
    case FINAL => "FINAL"
    case SEALED => "SEALED"
    case CASE => "CASE"
    case IMPLICIT => "IMPLICIT"
    case ERASED => "ERASED"
    case LAZY => "LAZY"
    case OVERRIDE => "OVERRIDE"
    case INLINE => "INLINE"
    case INLINEPROXY => "INLINEPROXY"
    case MACRO => "MACRO"
    case OPAQUE => "OPAQUE"
    case STATIC => "STATIC"
    case OBJECT => "OBJECT"
    case TRAIT => "TRAIT"
    case TRANSPARENT => "TRANSPARENT"
    case INFIX => "INFIX"
    case ENUM => "ENUM"
    case LOCAL => "LOCAL"
    case SYNTHETIC => "SYNTHETIC"
    case ARTIFACT => "ARTIFACT"
    case MUTABLE => "MUTABLE"
    case FIELDaccessor => "FIELDaccessor"
    case CASEaccessor => "CASEaccessor"
    case COVARIANT => "COVARIANT"
    case CONTRAVARIANT => "CONTRAVARIANT"
    case HASDEFAULT => "HASDEFAULT"
    case STABLE => "STABLE"
    case EXTENSION => "EXTENSION"
    case GIVEN => "GIVEN"
    case PARAMsetter => "PARAMsetter"
    case EXPORTED => "EXPORTED"
    case OPEN => "OPEN"
    case INVISIBLE => "INVISIBLE"
    case PARAMalias => "PARAMalias"
    case EMPTYCLAUSE => "EMPTYCLAUSE"
    case SPLITCLAUSE => "SPLITCLAUSE"

    case SHAREDterm => "SHAREDterm"
    case SHAREDtype => "SHAREDtype"
    case TERMREFdirect => "TERMREFdirect"
    case TYPEREFdirect => "TYPEREFdirect"
    case TERMREFpkg => "TERMREFpkg"
    case TYPEREFpkg => "TYPEREFpkg"
    case RECthis => "RECthis"
    case BYTEconst => "BYTEconst"
    case SHORTconst => "SHORTconst"
    case CHARconst => "CHARconst"
    case INTconst => "INTconst"
    case LONGconst => "LONGconst"
    case FLOATconst => "FLOATconst"
    case DOUBLEconst => "DOUBLEconst"
    case STRINGconst => "STRINGconst"
    case RECtype => "RECtype"

    case IDENT => "IDENT"
    case IDENTtpt => "IDENTtpt"
    case SELECT => "SELECT"
    case SELECTtpt => "SELECTtpt"
    case TERMREFsymbol => "TERMREFsymbol"
    case TERMREF => "TERMREF"
    case TYPEREFsymbol => "TYPEREFsymbol"
    case TYPEREF => "TYPEREF"

    case PACKAGE => "PACKAGE"
    case VALDEF => "VALDEF"
    case DEFDEF => "DEFDEF"
    case TYPEDEF => "TYPEDEF"
    case IMPORT => "IMPORT"
    case EXPORT => "EXPORT"
    case TYPEPARAM => "TYPEPARAM"
    case PARAM => "PARAM"
    case IMPORTED => "IMPORTED"
    case RENAMED => "RENAMED"
    case BOUNDED => "BOUNDED"
    case APPLY => "APPLY"
    case TYPEAPPLY => "TYPEAPPLY"
    case APPLYsigpoly => "APPLYsigpoly"
    case NEW => "NEW"
    case THROW => "THROW"
    case TYPED => "TYPED"
    case NAMEDARG => "NAMEDARG"
    case ASSIGN => "ASSIGN"
    case BLOCK => "BLOCK"
    case IF => "IF"
    case LAMBDA => "LAMBDA"
    case MATCH => "MATCH"
    case RETURN => "RETURN"
    case WHILE => "WHILE"
    case INLINED => "INLINED"
    case SELECTouter => "SELECTouter"
    case TRY => "TRY"
    case REPEATED => "REPEATED"
    case BIND => "BIND"
    case ALTERNATIVE => "ALTERNATIVE"
    case UNAPPLY => "UNAPPLY"
    case ANNOTATEDtype => "ANNOTATEDtype"
    case ANNOTATEDtpt => "ANNOTATEDtpt"
    case CASEDEF => "CASEDEF"
    case IMPLICITarg => "IMPLICITarg"
    case TEMPLATE => "TEMPLATE"
    case SELFDEF => "SELFDEF"
    case THIS => "THIS"
    case QUALTHIS => "QUALTHIS"
    case SUPER => "SUPER"
    case CLASSconst => "CLASSconst"
    case SINGLETONtpt => "SINGLETONtpt"
    case SUPERtype => "SUPERtype"
    case TERMREFin => "TERMREFin"
    case TYPEREFin => "TYPEREFin"
    case SELECTin => "SELECTin"

    case REFINEDtype => "REFINEDtype"
    case REFINEDtpt => "REFINEDtpt"
    case APPLIEDtype => "APPLIEDtype"
    case APPLIEDtpt => "APPLIEDtpt"
    case TYPEBOUNDS => "TYPEBOUNDS"
    case TYPEBOUNDStpt => "TYPEBOUNDStpt"
    case ANDtype => "ANDtype"
    case ORtype => "ORtype"
    case BYNAMEtype => "BYNAMEtype"
    case BYNAMEtpt => "BYNAMEtpt"
    case POLYtype => "POLYtype"
    case METHODtype => "METHODtype"
    case TYPELAMBDAtype => "TYPELAMBDAtype"
    case LAMBDAtpt => "LAMBDAtpt"
    case MATCHtype => "MATCHtype"
    case MATCHCASEtype => "MATCHCASEtype"
    case MATCHtpt => "MATCHtpt"
    case PARAMtype => "PARAMtype"
    case ANNOTATION => "ANNOTATION"
    case PRIVATEqualified => "PRIVATEqualified"
    case PROTECTEDqualified => "PROTECTEDqualified"
    case EXPLICITtpt => "EXPLICITtpt"
    case ELIDED => "ELIDED"
    case HOLE => "HOLE"
  }

  def attributeTagToString(tag: Int): String = tag match {
    case SCALA2STANDARDLIBRARYattr => "SCALA2STANDARDLIBRARYattr"
    case EXPLICITNULLSattr => "EXPLICITNULLSattr"
    case CAPTURECHECKEDattr => "CAPTURECHECKEDattr"
    case WITHPUREFUNSattr => "WITHPUREFUNSattr"
    case JAVAattr => "JAVAattr"
    case OUTLINEattr => "OUTLINEattr"
    case SOURCEFILEattr => "SOURCEFILEattr"
  }

  /** @return If non-negative, the number of leading references (represented as nats) of a length/trees entry.
   *          If negative, minus the number of leading non-reference trees.
   */
  def numRefs(tag: Int): Int = tag match {
    case VALDEF | DEFDEF | TYPEDEF | TYPEPARAM | PARAM | NAMEDARG | RETURN | BIND |
         SELFDEF | REFINEDtype | TERMREFin | TYPEREFin | SELECTin | HOLE => 1
    case RENAMED | PARAMtype => 2
    case POLYtype | TYPELAMBDAtype | METHODtype => -1
    case _ => 0
  }
}

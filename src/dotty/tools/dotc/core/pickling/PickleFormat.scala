package dotty.tools.dotc
package core
package pickling

/************************************************************
Notation:

We use BNF notation. Terminal symbols start with at least two
consecutive upper case letters. Each terminal is represented as a
single byte tag. Non-terminals are mixed case. Prefixes of the form
lower case letter*_ are for explanation of semantic content only, they
can be dropped without changing the grammar.

Micro-syntax:

  LongNat       = Digit* StopDigit        // big endian, value fits in a Long without overflow
  Nat           = LongNat                 // value fits in an Int without overflow
  Digit         = 0 | ... | 127
  StopDigit     = 128 | ... | 255         // value = digit - 128

Macro-format:

  File          = Header majorVersion_Nat minorVersion_Nat nameTable_Length Name* Section*
  Header        = "5CA1AB1F"

  Section       = NameRef Length Bytes
  Length        = Nat                    // length of rest of entry in bytes

  Name          = UTF8	        Length UTF8-CodePoint*
                  QUALIFIED     Length qualified_NameRef selector_NameRef
                  SIGNED        Length original_NameRef resultSig_NameRef paramSig_NameRef*
                  EXPANDED      Length original_NameRef
                  MODULECLASS   Length module_NameRef
                  SUPERACCESSOR Length accessed_NameRef
                  DEFAULTGETTER Length method_NameRef paramNumber_Nat
                  ...

  NameRef       = Nat                    // ordinal number of name in name table, starting from 1.

Note: Unqualified names in the name table are strings. The context decides whether a name is
a type-name or a term-name. The same string can represent both.

Standard-Section: "ASTs" Tree*

  Tree          = PACKAGE       Length Path Tree*
                  Stat

  Stat          = Term
                  VALDEF        Length NameRef Type rhs_Tree Modifier*
                  DEFDEF        Length NameRef TypeParam* Params* return_Type rhs_Tree
                                       Modifier*
                  TYPEDEF       Length NameRef (Type | Template) Modifier*
                  IMPORT        Length qual_Term Selector*

  TypeParam     = TYPEPARAM     Length NameRef Type Modifier*
  Params        = PARAMS        Length Param*
  Param         = PARAM         Length NameRef Type Modifier*
  Selector      = IMPORTED      Length name_NameRef
                  RENAMED       Length from_NameRef to_NameRef

  Term          = Path
                  IDENT                Type NameRef
                  SELECT               qual_Term possiblySigned_NameRef
                  SUPER         Length this_Term mixinTrait_Type?
                  APPLY         Length fn_Term arg_Term*
                  TYPEAPPLY     Length fn_Term arg_Type*
                  NEW           Length cls_Type
                  PAIR          Length left_Term right_Term
                  TYPED         Length expr_Term ascription_Type
                  NAMEDARG      Length paramName_NameRef arg_Term
                  ASSIGN        Length lhs_Term rhs_Term
                  BLOCK         Length expr_Term Stat*
                  IF            Length cond_Term then_Term else_Term
                  CLOSURE       Length meth_Term target_Type env_Term*
                  MATCH         Length sel_Term CaseDef*
                  RETURN        Length meth_ASTRef expr_Term?
                  TRY           Length expr_Term CaseDef* finalizer_Term?
                  THROW         Length expr_Term
                  SEQLITERAL    Length elem_Term*
                  JSEQLITERAL   Length elem_Term*
                  BIND          Length boundName_NameRef patType_Type pat_Term
                  ALTERNATIVE   Length alt_Term*
                  UNAPPLY       Length fun_Term ImplicitArg* pat_Term*
                  ANNOTATED     Length annot_Term underlying_Term
                  EMPTYTREE

  CaseDef       = CASEDEF       Length pat_Tree guard_Tree rhs_Tree
  ImplicitArg   = IMPLICITARG   Length arg_Tree
  Template      = TEMPLATE      Length parent_Tree* SelfDef? Stat*
// if there is a primary constructor, it is the first statement in Stat*..
  SelfDef       = Param
  ASTRef        = Nat                          		// byte position in AST payload

  Path          = Constant
                  TERMREFdirect        sym_ASTRef
                  TERMREFstatic        fullyQualified_NameRef
                  TERMREFsymbol        qual_Type sym_ASTRef
                  TERMREF              qual_Type possiblySigned_NameRef
                  THIS          Length clsRef_Type
                  SHARED               path_ASTRef

  Constant      = UNITconst
                  FALSEconst
                  TRUEconst
                  BYTEconst            Nat
                  BYTEneg              NegNat
                  SHORTconst           Nat
                  SHORTneg             NegNat
                  CHARconst            Nat
                  INTconst             Nat
                  INTneg               NegNat
                  LONGconst            LongNat
                  LONGneg              NegLongNat
                  FLOATconst           LongNat
                  DOUBLEconst          LongNat
                  STRINGconst          NameRef
                  NULLconst
                  CLASSconst    Length Type
                  ENUMconst     Length Path
  NegNat        = Nat          					// negValue = -natValue - 1
  NegLongNat    = LongNat     					// negValue = -natValue - 1

  Type          = Path
                  TYPEREFdirect        sym_ASTRef
                  TYPEREFstatic        fullyQualified_NameRef
                  TYPEREFsymbol        qual_Type sym_ASTRef
                  TYPEREF              qual_Type possiblySigned_NameRef
                  SUPERtype     Length this_Type underlying_Type
                  SKOLEMtype    Length underlying_Type
                  REFINEDtype   Length refinement_NameRef info_Type
                  APPLIEDtype   Length tycon_Type arg_Type*
                  TYPEBOUNDS    Length low_Type high_Type
                  TYPEALIAS     Length alias_Type
                  ANNOTATED     Length annot_Tree underlying_Type
                  ANDtype       Length left_Type right_Type
                  ORtype        Length left_Type right_Type
                  BIND          Length boundName_NameRef underlying_Type selfRef_Type
                  BYNAMEtype    Length underlying_Type
                  POLYtype      Length result_Type NamesTypes      // needed for refinements
                  METHODtype    Length result_Type NamesTypes      // needed for refinements
                  PARAMtype     Length binder_ASTref paramNum_Nat  // needed for refinements
                  NOTYPE
                  NOPREFIX
                  SHARED               type_ASTRef
  NamesTypes    = ParamType*
  NameType      = paramName_NameRef typeOrBounds_ASTRef

  Modifier      = PRIVATE
                  INTERNAL						// package private
                  PROTECTED
                  PRIVATEqualified     Length qualifier_Type		// will be dropped
                  PROTECTEDqualified   Length qualifier_Type 	// will be dropped
                  ABSTRACT
                  FINAL
                  SEALED
                  CASE
                  IMPLICIT
                  LAZY
                  OVERRIDE
                  INLINE
                  ABSOVERRIDE					// abstract override
                  STATIC						// mapped to static Java member
                  MODULE						// an object or its class
                  LOCAL						// private[this] or protected[this]
                  SYNTHETIC					// generated by Scala compiler
                  ARTIFACT						// to be tagged Java Synthetic
                  MUTABLE 						// a var
                  LABEL						// method generated as a label
                  FIELDaccessor					// getter or setter
                  PARAMaccessor					// getter or setter for class param
                  CASEaccessor					// getter for case class param
                  COVARIANT					// type param marked “+”
                  CONTRAVARIANT					// type param marked “-”
                  SCALA2X						// Imported from Scala2.x
                  DEFAULTparameterized				// Method with default params
                  DEFAULTinit					// variable with “_” initializer
                  annotation_Term

Note: Tree tags are grouped into 4 categories that determine what follows, and thus allow to compute the size of the tagged tree in a generic way.

	Category 1 (tags 0-63)   :	tag
	Category 2 (tags 64-99)  :	tag Nat
	Category 3 (tags 100-127):	tag AST Nat
	Category 4 (tags 128-255):  tag Length <payload>

Standard Section: "Positions" startPos_Index endPos_Index

  Index         = Length Assoc*
  Assoc         = Delta ASTRef               		// largest tree starting/ending at offset
  Delta         = Nat                 			// # chars from last offset or start of file

**************************************************************************************/

object PickleFormat {

  final val header = "5CA1AB1F"
  final val MajorVersion = 0
  final val MinorVersion = 3

  // Name tags

  final val UTF8 = 1
  final val QUALIFIED = 2
  final val SIGNED = 3
  final val EXPANDED = 4
  final val MODULECLASS = 5
  final val SUPERACCESSOR = 6
  final val DEFAULTGETTER = 7

// AST tags

  final val EMPTYTREE = 0
  final val NOTYPE = 1
  final val NOPREFIX = 2
  final val UNITconst = 3
  final val FALSEconst = 4
  final val TRUEconst = 5
  final val NULLconst = 6
  final val PRIVATE = 7
  final val INTERNAL = 8
  final val PROTECTED = 9
  final val ABSTRACT = 10
  final val FINAL = 11
  final val SEALED = 12
  final val CASE = 13
  final val IMPLICIT = 14
  final val LAZY = 15
  final val OVERRIDE = 16
  final val INLINE = 17
  final val ABSOVERRIDE = 18
  final val STATIC = 19
  final val MODULE = 20
  final val LOCAL = 21
  final val SYNTHETIC = 22
  final val ARTIFACT = 23
  final val MUTABLE = 24
  final val LABEL = 25
  final val FIELDaccessor = 26
  final val PARAMaccessor = 27
  final val CASEaccessor = 28
  final val COVARIANT = 29
  final val CONTRAVARIANT = 30
  final val SCALA2X = 31
  final val DEFAULTparameterized = 32
  final val DEFAULTinit = 33

  final val SHARED = 64
  final val TERMREFdirect = 65
  final val TYPEREFdirect = 66
  final val TERMREFstatic = 67
  final val TYPEREFstatic = 68
  final val BYTEconst = 69
  final val BYTEneg = 70
  final val SHORTconst = 71
  final val SHORTneg = 72
  final val CHARconst = 73
  final val INTconst = 74
  final val INTneg = 75
  final val LONGconst = 76
  final val LONGneg = 77
  final val FLOATconst = 78
  final val DOUBLEconst = 79
  final val STRINGconst = 80

  final val IDENT = 100
  final val SELECT = 101
  final val TERMREFsymbol = 102
  final val TERMREF = 103
  final val TYPEREFsymbol = 104
  final val TYPEREF = 105

  final val PACKAGE = 128
  final val VALDEF = 129
  final val DEFDEF = 130
  final val TYPEDEF = 131
  final val IMPORT = 132
  final val TYPEPARAM = 133
  final val PARAMS = 134
  final val PARAM = 135
  final val IMPORTED = 136
  final val RENAMED = 137
  final val APPLY = 138
  final val TYPEAPPLY = 139
  final val NEW = 140
  final val PAIR = 141
  final val TYPED = 142
  final val NAMEDARG = 143
  final val ASSIGN = 144
  final val BLOCK = 145
  final val IF = 146
  final val CLOSURE = 147
  final val MATCH = 148
  final val RETURN = 149
  final val TRY = 150
  final val THROW = 151
  final val SEQLITERAL = 152
  final val JSEQLITERAL = 153
  final val BIND = 154
  final val ALTERNATIVE = 155
  final val UNAPPLY = 156
  final val ANNOTATED = 157
  final val CASEDEF = 158
  final val IMPLICITarg = 159
  final val TEMPLATE = 160
  final val THIS = 161
  final val SUPER = 162
  final val CLASSconst = 163
  final val ENUMconst = 164
  final val SUPERtype = 165
  final val SKOLEMtype = 166
  final val REFINEDtype = 167
  final val APPLIEDtype = 168
  final val TYPEBOUNDS = 169
  final val TYPEALIAS = 170
  final val ANDtype = 171
  final val ORtype = 172
  final val BYNAMEtype = 173
  final val METHODtype = 174
  final val POLYtype = 175
  final val PARAMtype = 176
  final val IMPLICITARG = 177
  final val PRIVATEqualified = 178
  final val PROTECTEDqualified = 179

  final val firstSimpleTreeTag = EMPTYTREE
  final val firstNatTreeTag = SHARED
  final val firstTreeNatTreeTag = IDENT
  final val firstLengthTreeTag = PACKAGE

  def nameTagToString(tag: Int): String = tag match {
    case UTF8 => "UTF8"
    case QUALIFIED => "QUALIFIED"
    case SIGNED => "SIGNED"
    case EXPANDED => "EXPANDED"
    case MODULECLASS => "MODULECLASS"
    case SUPERACCESSOR => "SUPERACCESSOR"
    case DEFAULTGETTER => "DEFAULTGETTER"
  }

  def astTagToString(tag: Int): String = tag match {
    case EMPTYTREE => "EMPTYTREE"
    case NOTYPE => "NOTYPE"
    case NOPREFIX => "NOPREFIX"
    case UNITconst => "UNITconst"
    case FALSEconst => "FALSEconst"
    case TRUEconst => "TRUEconst"
    case NULLconst => "NULLconst"
    case PRIVATE => "PRIVATE"
    case INTERNAL => "INTERNAL"
    case PROTECTED => "PROTECTED"
    case ABSTRACT => "ABSTRACT"
    case FINAL => "FINAL"
    case SEALED => "SEALED"
    case CASE => "CASE"
    case IMPLICIT => "IMPLICIT"
    case LAZY => "LAZY"
    case OVERRIDE => "OVERRIDE"
    case INLINE => "INLINE"
    case ABSOVERRIDE => "ABSOVERRIDE"
    case STATIC => "STATIC"
    case MODULE => "MODULE"
    case LOCAL => "LOCAL"
    case SYNTHETIC => "SYNTHETIC"
    case ARTIFACT => "ARTIFACT"
    case MUTABLE => "MUTABLE"
    case LABEL => "LABEL"
    case FIELDaccessor => "FIELDaccessor"
    case PARAMaccessor => "PARAMaccessor"
    case CASEaccessor => "CASEaccessor"
    case COVARIANT => "COVARIANT"
    case CONTRAVARIANT => "CONTRAVARIANT"
    case SCALA2X => "SCALA2X"
    case DEFAULTparameterized => "DEFAULTparameterized"
    case DEFAULTinit => "DEFAULTinit"

    case SHARED => "SHARED"
    case TERMREFdirect => "TERMREFdirect"
    case TYPEREFdirect => "TYPEREFdirect"
    case TERMREFstatic => "TERMREFstatic"
    case TYPEREFstatic => "TYPEREFstatic"
    case BYTEconst => "BYTEconst"
    case BYTEneg => "BYTEneg"
    case SHORTconst => "SHORTconst"
    case SHORTneg => "SHORTneg"
    case CHARconst => "CHARconst"
    case INTconst => "INTconst"
    case INTneg => "INTneg"
    case LONGconst => "LONGconst"
    case LONGneg => "LONGneg"
    case FLOATconst => "FLOATconst"
    case DOUBLEconst => "DOUBLEconst"
    case STRINGconst => "STRINGconst"

    case IDENT => "IDENT"
    case SELECT => "SELECT"
    case TERMREFsymbol => "TERMREFsymbol"
    case TERMREF => "TERMREF"
    case TYPEREFsymbol => "TYPEREFsymbol"
    case TYPEREF => "TYPEREF"

    case PACKAGE => "PACKAGE"
    case VALDEF => "VALDEF"
    case DEFDEF => "DEFDEF"
    case TYPEDEF => "TYPEDEF"
    case IMPORT => "IMPORT"
    case TYPEPARAM => "TYPEPARAM"
    case PARAMS => "PARAMS"
    case PARAM => "PARAM"
    case IMPORTED => "IMPORTED"
    case RENAMED => "RENAMED"
    case APPLY => "APPLY"
    case TYPEAPPLY => "TYPEAPPLY"
    case NEW => "NEW"
    case PAIR => "PAIR"
    case TYPED => "TYPED"
    case NAMEDARG => "NAMEDARG"
    case ASSIGN => "ASSIGN"
    case BLOCK => "BLOCK"
    case IF => "IF"
    case CLOSURE => "CLOSURE"
    case MATCH => "MATCH"
    case RETURN => "RETURN"
    case TRY => "TRY"
    case THROW => "THROW"
    case SEQLITERAL => "SEQLITERAL"
    case JSEQLITERAL => "JSEQLITERAL"
    case BIND => "BIND"
    case ALTERNATIVE => "ALTERNATIVE"
    case UNAPPLY => "UNAPPLY"
    case ANNOTATED => "ANNOTATED"
    case CASEDEF => "CASEDEF"
    case IMPLICITarg => "IMPLICITarg"
    case TEMPLATE => "TEMPLATE"
    case THIS => "THIS"
    case SUPER => "SUPER"
    case CLASSconst => "CLASSconst"
    case ENUMconst => "ENUMconst"
    case SUPERtype => "SUPERtype"
    case SKOLEMtype => "SKOLEMtype"
    case REFINEDtype => "REFINEDtype"
    case APPLIEDtype => "APPLIEDtype"
    case TYPEBOUNDS => "TYPEBOUNDS"
    case TYPEALIAS => "TYPEALIAS"
    case ANDtype => "ANDtype"
    case ORtype => "ORtype"
    case BYNAMEtype => "BYNAMEtype"
    case POLYtype => "POLYtype"
    case METHODtype => "METHODtype"
    case PARAMtype => "PARAMtype"
    case IMPLICITARG => "IMPLICITARG"
    case PRIVATEqualified => "PRIVATEqualified"
    case PROTECTEDqualified => "PROTECTEDqualified"
  }
}

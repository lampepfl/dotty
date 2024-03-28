---
layout: doc-page
title: "Scala 3 Syntax Summary"
---

<!--

This page has a companion page at _docs/reference/syntax.md.

!! Make sure to edit both pages in sync. !!

reference/syntax.md shows the official Scala 3 syntax, without deprecated or experimental features.

internals/syntax.md shows the Scala 3 syntax as supported by the parser, including
deprecated and experimental features. It also gives some indications how
productions map to AST nodes.

-->

The following description of Scala tokens uses literal characters `‘c’` when
referring to the ASCII fragment `\u0000` – `\u007F`.

Informal descriptions are typeset as `“some comment”`.

## Lexical Syntax

The lexical syntax of Scala is given by the following grammar in EBNF form:

```ebnf
whiteSpace       ::=  ‘\u0020’ | ‘\u0009’ | ‘\u000D’ | ‘\u000A’
upper            ::=  ‘A’ | ... | ‘Z’ | ‘$’ and any character in Unicode categories Lu, Lt or Nl,
                      and any character in Unicode categories Lo and Lm that doesn't have
                      contributory property Other_Lowercase
lower            ::=  ‘a’ | ... | ‘z’ | ‘_’ and any character in Unicode category Ll,
                      and any character in Unicode categories Lo or Lm that has contributory
                      property Other_Lowercase
letter           ::=  upper | lower
digit            ::=  ‘0’ | ... | ‘9’
paren            ::=  ‘(’ | ‘)’ | ‘[’ | ‘]’ | ‘{’ | ‘}’
delim            ::=  ‘`’ | ‘'’ | ‘"’ | ‘.’ | ‘;’ | ‘,’
opchar           ::=  ‘!’ | ‘#’ | ‘%’ | ‘&’ | ‘*’ | ‘+’ | ‘-’ | ‘/’ | ‘:’ |
                      ‘<’ | ‘=’ | ‘>’ | ‘?’ | ‘@’ | ‘\’ | ‘^’ | ‘|’ | ‘~’
                      and any character in Unicode categories Sm or So
printableChar    ::=  all characters in [\u0020, \u007E] inclusive
UnicodeEscape    ::=  ‘\’ ‘u’ {‘u’} hexDigit hexDigit hexDigit hexDigit
hexDigit         ::=  ‘0’ | ... | ‘9’ | ‘A’ | ... | ‘F’ | ‘a’ | ... | ‘f’
charEscapeSeq    ::=  ‘\’ (‘b’ | ‘t’ | ‘n’ | ‘f’ | ‘r’ | ‘"’ | ‘'’ | ‘\’)
escapeSeq        ::=  UnicodeEscape | charEscapeSeq

op               ::=  opchar {opchar}
varid            ::=  lower idrest
boundvarid       ::=  varid
                   |  ‘`’ varid ‘`’
plainid          ::=  alphaid
                   |  op
id               ::=  plainid
                   |  ‘`’ { charNoBackQuoteOrNewline | escapeSeq } ‘`’
idrest           ::=  {letter | digit} [‘_’ op]
quoteId          ::=  ‘'’ alphaid
spliceId         ::=  ‘$’ alphaid ;

integerLiteral   ::=  (decimalNumeral | hexNumeral) [‘L’ | ‘l’]
decimalNumeral   ::=  ‘0’ | digit [{digit | ‘_’} digit]
hexNumeral       ::=  ‘0’ (‘x’ | ‘X’) hexDigit [{hexDigit | ‘_’} hexDigit]

floatingPointLiteral
                 ::=  [decimalNumeral] ‘.’ digit [{digit | ‘_’} digit] [exponentPart] [floatType]
                   |  decimalNumeral exponentPart [floatType]
                   |  decimalNumeral floatType
exponentPart     ::=  (‘E’ | ‘e’) [‘+’ | ‘-’] digit [{digit | ‘_’} digit]
floatType        ::=  ‘F’ | ‘f’ | ‘D’ | ‘d’

booleanLiteral   ::=  ‘true’ | ‘false’

characterLiteral ::=  ‘'’ (charNoQuoteOrNewline | escapeSeq) ‘'’

stringLiteral    ::=  ‘"’ {stringElement} ‘"’
                   |  ‘"""’ multiLineChars ‘"""’
stringElement    ::=  charNoDoubleQuoteOrNewline
                   |  escapeSeq
multiLineChars   ::=  {[‘"’] [‘"’] charNoDoubleQuote} {‘"’}

interpolatedString
                 ::=  alphaid ‘"’ {[‘\’] interpolatedStringPart | ‘\\’ | ‘\"’} ‘"’
                   |  alphaid ‘"""’ {[‘"’] [‘"’] char \ (‘"’ | ‘\$’) | escape} {‘"’} ‘"""’
interpolatedStringPart
                 ::= printableChar \ (‘"’ | ‘$’ | ‘\’) | escape
escape           ::=  ‘\$\$’
                   |  ‘\$"’
                   |  ‘\$’ alphaid
                   |  ‘\$’ BlockExpr
alphaid          ::=  upper idrest
                   |  varid

comment          ::=  ‘/*’ “any sequence of characters; nested comments are allowed” ‘*/’
                   |  ‘//’ “any sequence of characters up to end of line”

nl               ::=  “new line character”
semi             ::=  ‘;’ |  nl {nl}
```

## Optional Braces

The principle of optional braces is that any keyword that can be followed by `{` can also be followed by an indented block, without needing an intervening `:`.
(Allowing an optional `:` would be counterproductive since it would introduce several ways to do the same thing.)

The lexical analyzer inserts `indent` and `outdent` tokens that represent regions of indented code [at certain points](../reference/other-new-features/indentation.md).

In the context-free productions below we use the notation `<<< ts >>>`
to indicate a token sequence `ts` that is either enclosed in a pair of braces `{ ts }` or that constitutes an indented region `indent ts outdent`. Analogously, the
notation `:<<< ts >>>` indicates a token sequence `ts` that is either enclosed in a pair of braces `{ ts }` or that constitutes an indented region `indent ts outdent` that follows
a `colon` token.

A `colon` token reads as the standard colon "`:`" but is generated instead of it where `colon` is legal according to the context free syntax, but only if the previous token
is an alphanumeric identifier, a backticked identifier, or one of the tokens `this`, `super`, `new`, "`)`", and "`]`".

```
colon         ::=  ':'    -- with side conditions explained above
 <<< ts >>>   ::=  ‘{’ ts ‘}’
                |  indent ts outdent
:<<< ts >>>   ::=  [nl] ‘{’ ts ‘}’
                |  colon indent ts outdent
```

## Keywords

### Regular keywords

```
abstract  case      catch     class     def       do        else
enum      export    extends   false     final     finally   for
given     if        implicit  import    lazy      match     new
null      object    override  package   private   protected return
sealed    super     then      throw     trait     true      try
type      val       var       while     with      yield
:         =         <-        =>        <:        >:        #
@         =>>       ?=>
```

### Soft keywords

```
as  derives  end  erased  extension  infix  inline  opaque  open  throws transparent  using  |  *  +  -
```

See the [separate section on soft keywords](../reference/soft-modifier.md) for additional
details on where a soft keyword is recognized.

## Context-free Syntax

The context-free syntax of Scala is given by the following EBNF
grammar:

### Literals and Paths
```ebnf
SimpleLiteral     ::=  [‘-’] integerLiteral
                    |  [‘-’] floatingPointLiteral
                    |  booleanLiteral
                    |  characterLiteral
                    |  stringLiteral
Literal           ::=  SimpleLiteral
                    |  interpolatedStringLiteral
                    |  symbolLiteral
                    |  ‘null’

QualId            ::=  id {‘.’ id}
ids               ::=  id {‘,’ id}

SimpleRef         ::=  id
                    |  [id ‘.’] ‘this’
                    |  [id ‘.’] ‘super’ [ClassQualifier] ‘.’ id

ClassQualifier    ::=  ‘[’ id ‘]’
```

### Types
```ebnf
Type              ::=  FunType
                    |  HkTypeParamClause ‘=>>’ Type                             LambdaTypeTree(ps, t)
                    |  FunParamClause ‘=>>’ Type                                TermLambdaTypeTree(ps, t)
                    |  MatchType
                    |  InfixType
FunType           ::=  FunTypeArgs (‘=>’ | ‘?=>’) Type                          Function(ts, t) | FunctionWithMods(ts, t, mods, erasedParams)
                    |  HKTypeParamClause '=>' Type                              PolyFunction(ps, t)
FunTypeArgs       ::=  InfixType
                    |  ‘(’ [ FunArgTypes ] ‘)’
                    |  FunParamClause
FunParamClause    ::=  ‘(’ TypedFunParam {‘,’ TypedFunParam } ‘)’
TypedFunParam     ::=  [`erased`] id ‘:’ Type
MatchType         ::=  InfixType `match` <<< TypeCaseClauses >>>
InfixType         ::=  RefinedType {id [nl] RefinedType}                        InfixOp(t1, op, t2)
RefinedType       ::=  AnnotType {[nl] Refinement}                              RefinedTypeTree(t, ds)
AnnotType         ::=  SimpleType {Annotation}                                  Annotated(t, annot)

SimpleType        ::=  SimpleLiteral                                            SingletonTypeTree(l)
                    |  ‘?’ TypeBounds
                    |  SimpleType1
SimpleType1       ::=  id                                                       Ident(name)
                    |  Singleton ‘.’ id                                         Select(t, name)
                    |  Singleton ‘.’ ‘type’                                     SingletonTypeTree(p)
                    |  ‘(’ Types ‘)’                                            Tuple(ts)
                    |  Refinement                                               RefinedTypeTree(EmptyTree, refinement)
                    |  TypeSplice                                               -- deprecated syntax
                    |  SimpleType1 TypeArgs                                     AppliedTypeTree(t, args)
                    |  SimpleType1 ‘#’ id                                       Select(t, name)
Singleton         ::=  SimpleRef
                    |  SimpleLiteral
                    |  Singleton ‘.’ id
FunArgType        ::=  Type
                    |  ‘=>’ Type                                                PrefixOp(=>, t)
FunArgTypes       ::=  FunArgType { ‘,’ FunArgType }
ParamType         ::=  [‘=>’] ParamValueType
ParamValueType    ::=  Type [‘*’]                                               PostfixOp(t, "*")
                    |  IntoType
                    |  ‘(’ IntoType ‘)’ ‘*’                                     PostfixOp(t, "*")
IntoType          ::=  [‘into’] IntoTargetType                                  Into(t)
                    |  ‘(’ IntoType ‘)’
IntoTargetType    ::=  Type
                    |  FunTypeArgs (‘=>’ | ‘?=>’) IntoType
TypeArgs          ::=  ‘[’ Types ‘]’                                            ts
Refinement        ::=  :<<< [RefineDcl] {semi [RefineDcl]} >>>                  ds
TypeBounds        ::=  [‘>:’ Type] [‘<:’ Type]                                  TypeBoundsTree(lo, hi)
TypeParamBounds   ::=  TypeBounds {‘:’ Type}                                    ContextBounds(typeBounds, tps)
Types             ::=  Type {‘,’ Type}
```

### Expressions
```ebnf
Expr              ::=  FunParams (‘=>’ | ‘?=>’) Expr                            Function(args, expr), Function(ValDef([implicit], id, TypeTree(), EmptyTree), expr)
                    |  HkTypeParamClause ‘=>’ Expr                              PolyFunction(ts, expr)
                    |  Expr1
BlockResult       ::=  FunParams (‘=>’ | ‘?=>’) Block
                    |  HkTypeParamClause ‘=>’ Block
                    |  Expr1
FunParams         ::=  Bindings
                    |  id
                    |  ‘_’
Expr1             ::=  [‘inline’] ‘if’ ‘(’ Expr ‘)’ {nl} Expr [[semi] ‘else’ Expr] If(Parens(cond), thenp, elsep?)
                    |  [‘inline’] ‘if’  Expr ‘then’ Expr [[semi] ‘else’ Expr]    If(cond, thenp, elsep?)
                    |  ‘while’ ‘(’ Expr ‘)’ {nl} Expr                           WhileDo(Parens(cond), body)
                    |  ‘while’ Expr ‘do’ Expr                                   WhileDo(cond, body)
                    |  ‘try’ Expr Catches [‘finally’ Expr]                      Try(expr, catches, expr?)
                    |  ‘try’ Expr [‘finally’ Expr]                              Try(expr, Nil, expr?)
                    |  ‘throw’ Expr                                             Throw(expr)
                    |  ‘return’ [Expr]                                          Return(expr?)
                    |  ForExpr
                    |  [SimpleExpr ‘.’] id ‘=’ Expr                             Assign(expr, expr)
                    |  PrefixOperator SimpleExpr ‘=’ Expr                       Assign(expr, expr)
                    |  SimpleExpr ArgumentExprs ‘=’ Expr                        Assign(expr, expr)
                    |  PostfixExpr [Ascription]
                    |  ‘inline’ InfixExpr MatchClause
Ascription        ::=  ‘:’ InfixType                                            Typed(expr, tp)
                    |  ‘:’ Annotation {Annotation}                              Typed(expr, Annotated(EmptyTree, annot)*)
Catches           ::=  ‘catch’ (Expr | ExprCaseClause)
PostfixExpr       ::=  InfixExpr [id]                                           PostfixOp(expr, op) -- only if language.postfixOperators is enabled
InfixExpr         ::=  PrefixExpr
                    |  InfixExpr id [nl] InfixExpr                              InfixOp(expr, op, expr)
                    |  InfixExpr id ColonArgument
                    |  InfixExpr MatchClause
MatchClause       ::=  ‘match’ <<< CaseClauses >>>                              Match(expr, cases)
PrefixExpr        ::=  [PrefixOperator] SimpleExpr                              PrefixOp(expr, op)
PrefixOperator    ::=  ‘-’ | ‘+’ | ‘~’ | ‘!’                                    -- unless backquoted
SimpleExpr        ::=  SimpleRef
                    |  Literal
                    |  ‘_’
                    |  BlockExpr
                    |  ExprSplice
                    |  Quoted
                    |  quoteId                                                  -- only inside splices
                    |  ‘new’ ConstrApp {‘with’ ConstrApp} [TemplateBody]        New(constr | templ)
                    |  ‘new’ TemplateBody
                    |  ‘(’ ExprsInParens ‘)’                                    Parens(exprs)
                    |  SimpleExpr ‘.’ id                                        Select(expr, id)
                    |  SimpleExpr ‘.’ MatchClause
                    |  SimpleExpr TypeArgs                                      TypeApply(expr, args)
                    |  SimpleExpr ArgumentExprs                                 Apply(expr, args)
                    |  SimpleExpr ColonArgument                                 -- under language.experimental.fewerBraces
                    |  SimpleExpr ‘_’                                           PostfixOp(expr, _) (to be dropped)
                    |  XmlExpr							-- to be dropped
ColonArgument     ::=  colon [LambdaStart]
                       indent (CaseClauses | Block) outdent
LambdaStart       ::=  FunParams (‘=>’ | ‘?=>’)
                    |  HkTypeParamClause ‘=>’
Quoted            ::=  ‘'’ ‘{’ Block ‘}’
                    |  ‘'’ ‘[’ TypeBlock ‘]’
ExprSplice        ::= spliceId                                                  -- if inside quoted block
                    |  ‘$’ ‘{’ Block ‘}’                                        -- unless inside quoted pattern
                    |  ‘$’ ‘{’ Pattern ‘}’                                      -- when inside quoted pattern
TypeSplice        ::= spliceId                                                  -- if inside quoted type -- deprecated syntax
                    |  ‘$’ ‘{’ Block ‘}’                                        -- unless inside quoted type pattern -- deprecated syntax
                    |  ‘$’ ‘{’ Pattern ‘}’                                      -- when inside quoted type pattern -- deprecated syntax
ExprsInParens     ::=  ExprInParens {‘,’ ExprInParens}
ExprInParens      ::=  PostfixExpr ‘:’ Type                                     -- normal Expr allows only RefinedType here
                    |  Expr
ParArgumentExprs  ::=  ‘(’ [ExprsInParens] ‘)’                          exprs
                    |  ‘(’ ‘using’ ExprsInParens ‘)’
                    |  ‘(’ [ExprsInParens ‘,’] PostfixExpr ‘*’ ‘)’              exprs :+ Typed(expr, Ident(wildcardStar))
ArgumentExprs     ::=  ParArgumentExprs
                    |  BlockExpr
BlockExpr         ::=  <<< CaseClauses | Block >>>
Block             ::=  {BlockStat semi} [BlockResult]                           Block(stats, expr?)
BlockStat         ::=  Import
                    |  {Annotation {nl}} {LocalModifier} Def
                    |  Extension
                    |  Expr1
                    |  EndMarker
TypeBlock         ::=  {TypeBlockStat semi} Type
TypeBlockStat     ::=  ‘type’ {nl} TypeDef

ForExpr           ::=  ‘for’ ‘(’ Enumerators0 ‘)’ {nl} [‘do‘ | ‘yield’] Expr     ForYield(enums, expr) / ForDo(enums, expr)
                    |  ‘for’ ‘{’ Enumerators0 ‘}’ {nl} [‘do‘ | ‘yield’] Expr
                    |  ‘for’     Enumerators0          (‘do‘ | ‘yield’) Expr
Enumerators0      ::=  {nl} Enumerators [semi]
Enumerators       ::=  Generator {semi Enumerator | Guard}
Enumerator        ::=  Generator
                    |  Guard {Guard}
                    |  Pattern1 ‘=’ Expr                                        GenAlias(pat, expr)
Generator         ::=  [‘case’] Pattern1 ‘<-’ Expr                              GenFrom(pat, expr)
Guard             ::=  ‘if’ PostfixExpr

CaseClauses       ::=  CaseClause { CaseClause }                                Match(EmptyTree, cases)
CaseClause        ::=  ‘case’ Pattern [Guard] ‘=>’ Block                        CaseDef(pat, guard?, block)   // block starts at =>
ExprCaseClause    ::=  ‘case’ Pattern [Guard] ‘=>’ Expr
TypeCaseClauses   ::=  TypeCaseClause { TypeCaseClause }
TypeCaseClause    ::=  ‘case’ (InfixType | ‘_’) ‘=>’ Type [semi]

Pattern           ::=  Pattern1 { ‘|’ Pattern1 }                                Alternative(pats)
Pattern1          ::=  PatVar ‘:’ RefinedType                                   Bind(name, Typed(Ident(wildcard), tpe))
                    |  [‘-’] integerLiteral ‘:’ RefinedType                     Typed(pat, tpe)
                    |  [‘-’] floatingPointLiteral ‘:’ RefinedType               Typed(pat, tpe)
                    |  Pattern2
Pattern2          ::=  [id ‘@’] InfixPattern                                    Bind(name, pat)
InfixPattern      ::=  SimplePattern { id [nl] SimplePattern }                  InfixOp(pat, op, pat)
SimplePattern     ::=  PatVar                                                   Ident(wildcard)
                    |  Literal                                                  Bind(name, Ident(wildcard))
                    |  ‘(’ [Patterns] ‘)’                                       Parens(pats) Tuple(pats)
                    |  Quoted
                    |  XmlPattern												(to be dropped)
                    |  SimplePattern1 [TypeArgs] [ArgumentPatterns]
                    |  ‘given’ RefinedType
SimplePattern1    ::=  SimpleRef
                    |  SimplePattern1 ‘.’ id
PatVar            ::=  varid
                    |  ‘_’
Patterns          ::=  Pattern {‘,’ Pattern}
ArgumentPatterns  ::=  ‘(’ [Patterns] ‘)’                                       Apply(fn, pats)
                    |  ‘(’ [Patterns ‘,’] PatVar ‘*’ ‘)’
```

### Type and Value Parameters
```ebnf
ClsTypeParamClause::=  ‘[’ ClsTypeParam {‘,’ ClsTypeParam} ‘]’
ClsTypeParam      ::=  {Annotation} [‘+’ | ‘-’]                                 TypeDef(Modifiers, name, tparams, bounds)
                       id [HkTypeParamClause] TypeParamBounds                   Bound(below, above, context)

TypTypeParamClause::=  ‘[’ TypTypeParam {‘,’ TypTypeParam} ‘]’
TypTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeBounds

HkTypeParamClause ::=  ‘[’ HkTypeParam {‘,’ HkTypeParam} ‘]’
HkTypeParam       ::=  {Annotation} [‘+’ | ‘-’] (id [HkTypeParamClause] | ‘_’)
                       TypeBounds

ClsParamClauses   ::=  {ClsParamClause} [[nl] ‘(’ [‘implicit’] ClsParams ‘)’]
ClsParamClause    ::=  [nl] ‘(’ ClsParams ‘)’
                    |  [nl] ‘(’ ‘using’ (ClsParams | FunArgTypes) ‘)’
ClsParams         ::=  ClsParam {‘,’ ClsParam}
ClsParam          ::=  {Annotation}                                             ValDef(mods, id, tpe, expr) -- point of mods on val/var
                       [{Modifier} (‘val’ | ‘var’)] Param

DefParamClauses   ::=  DefParamClause { DefParamClause } -- and two DefTypeParamClause cannot be adjacent
DefParamClause    ::=  DefTypeParamClause
                    |  DefTermParamClause
                    |  UsingParamClause
TypelessClauses   ::=  TypelessClause {TypelessClause}
TypelessClause    ::=  DefTermParamClause
                    |  UsingParamClause

DefTypeParamClause::=  [nl] ‘[’ DefTypeParam {‘,’ DefTypeParam} ‘]’
DefTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeParamBounds
DefTermParamClause::=  [nl] ‘(’ [DefTermParams] ‘)’
UsingParamClause  ::=  [nl] ‘(’ ‘using’ (DefTermParams | FunArgTypes) ‘)’
DefImplicitClause ::=  [nl] ‘(’ ‘implicit’ DefTermParams ‘)’

DefTermParams     ::= DefTermParam {‘,’ DefTermParam}
DefTermParam      ::= {Annotation} [`erased`] [‘inline’] Param                    ValDef(mods, id, tpe, expr) -- point of mods at id.
Param             ::=  id ‘:’ ParamType [‘=’ Expr]
```

### Bindings and Imports
```ebnf
Bindings          ::=  ‘(’ [Binding {‘,’ Binding}] ‘)’
Binding           ::=  [`erased`] (id | ‘_’) [‘:’ Type]                           ValDef(_, id, tpe, EmptyTree)

Modifier          ::=  LocalModifier
                    |  AccessModifier
                    |  ‘override’
                    |  ‘opaque’
LocalModifier     ::=  ‘abstract’
                    |  ‘final’
                    |  ‘sealed’
                    |  ‘open’
                    |  ‘implicit’
                    |  ‘lazy’
                    |  ‘inline’
                    |  ‘transparent’
                    |  ‘infix’
                    |  ‘erased’

AccessModifier    ::=  (‘private’ | ‘protected’) [AccessQualifier]
AccessQualifier   ::=  ‘[’ id ‘]’

Annotation        ::=  ‘@’ SimpleType1 {ParArgumentExprs}                         Apply(tpe, args)

Import            ::=  ‘import’ ImportExpr {‘,’ ImportExpr}
Export            ::=  ‘export’ ImportExpr {‘,’ ImportExpr}
ImportExpr        ::=  SimpleRef {‘.’ id} ‘.’ ImportSpec                          Import(expr, sels)
                    |  SimpleRef ‘as’ id                                          Import(EmptyTree, ImportSelector(ref, id))
ImportSpec        ::=  NamedSelector
                    |  WildCardSelector
                    | ‘{’ ImportSelectors) ‘}’
NamedSelector     ::=  id [‘as’ (id | ‘_’)]
WildCardSelector  ::=  ‘*’ | ‘given’ [InfixType]
ImportSelectors   ::=  NamedSelector [‘,’ ImportSelectors]
                    |  WildCardSelector {‘,’ WildCardSelector}

EndMarker         ::=  ‘end’ EndMarkerTag    -- when followed by EOL
EndMarkerTag      ::=  id | ‘if’ | ‘while’ | ‘for’ | ‘match’ | ‘try’
                    |  ‘new’ | ‘this’ | ‘given’ | ‘extension’ | ‘val’
```

### Definitions
```ebnf
RefineDcl         ::=  ‘val’ ValDcl
                    |  ‘var’ ValDcl
                    |  ‘def’ DefDcl
                    |  ‘type’ {nl} TypeDef
ValDcl            ::=  ids ‘:’ Type
DefDcl            ::=  DefSig ‘:’ Type

Def               ::=  ‘val’ PatDef
                    |  ‘var’ PatDef
                    |  ‘def’ DefDef
                    |  ‘type’ {nl} TypeDef
                    |  TmplDef
PatDef            ::=  ids [‘:’ Type] [‘=’ Expr]
                    |  Pattern2 [‘:’ Type] [‘=’ Expr]                           PatDef(_, pats, tpe?, expr)
DefDef            ::=  DefSig [‘:’ Type] [‘=’ Expr]                             DefDef(_, name, paramss, tpe, expr)
                    |  ‘this’ TypelessClauses [DefImplicitClause] ‘=’ ConstrExpr     DefDef(_, <init>, vparamss, EmptyTree, expr | Block)
DefSig            ::=  id [DefParamClauses] [DefImplicitClause]
TypeDef           ::=  id [TypeParamClause] {FunParamClause} TypeBounds         TypeDefTree(_, name, tparams, bound
                       [‘=’ Type]

TmplDef           ::=  ([‘case’] ‘class’ | ‘trait’) ClassDef
                    |  [‘case’] ‘object’ ObjectDef
                    |  ‘enum’ EnumDef
                    |  ‘given’ GivenDef
ClassDef          ::=  id ClassConstr [Template]                                ClassDef(mods, name, tparams, templ)
ClassConstr       ::=  [ClsTypeParamClause] [ConstrMods] ClsParamClauses        with DefDef(_, <init>, Nil, vparamss, EmptyTree, EmptyTree) as first stat
ConstrMods        ::=  {Annotation} [AccessModifier]
ObjectDef         ::=  id [Template]                                            ModuleDef(mods, name, template)  // no constructor
EnumDef           ::=  id ClassConstr InheritClauses EnumBody
GivenDef          ::=  [GivenSig] (AnnotType [‘=’ Expr] | StructuralInstance)
GivenSig          ::=  [id] [DefTypeParamClause] {UsingParamClause} ‘:’         -- one of `id`, `DefTypeParamClause`, `UsingParamClause` must be present
StructuralInstance ::=  ConstrApp {‘with’ ConstrApp} [‘with’ WithTemplateBody]
Extension         ::=  ‘extension’ [DefTypeParamClause] {UsingParamClause}
                       ‘(’ DefTermParam ‘)’ {UsingParamClause} ExtMethods
ExtMethods        ::=  ExtMethod | [nl] <<< ExtMethod {semi ExtMethod} >>>
ExtMethod         ::=  {Annotation [nl]} {Modifier} ‘def’ DefDef
                    |  Export
Template          ::=  InheritClauses [TemplateBody]
InheritClauses    ::=  [‘extends’ ConstrApps] [‘derives’ QualId {‘,’ QualId}]
ConstrApps        ::=  ConstrApp ({‘,’ ConstrApp} | {‘with’ ConstrApp})
ConstrApp         ::=  SimpleType1 {Annotation} {ParArgumentExprs}
ConstrExpr        ::=  SelfInvocation
                    |  <<< SelfInvocation {semi BlockStat} >>>
SelfInvocation    ::=  ‘this’ ArgumentExprs {ArgumentExprs}

WithTemplateBody  ::=  <<< [SelfType] TemplateStat {semi TemplateStat} >>>
TemplateBody      ::=  :<<< [SelfType] TemplateStat {semi TemplateStat} >>>
TemplateStat      ::=  Import
                    |  Export
                    |  {Annotation [nl]} {Modifier} Def
                    |  Extension
                    |  Expr1
                    |  EndMarker
                    |
SelfType          ::=  id [‘:’ InfixType] ‘=>’                                  ValDef(_, name, tpt, _)
                    |  ‘this’ ‘:’ InfixType ‘=>’

EnumBody          ::=  :<<< [SelfType] EnumStat {semi EnumStat} >>>
EnumStat          ::=  TemplateStat
                    |  {Annotation [nl]} {Modifier} EnumCase
EnumCase          ::=  ‘case’ (id ClassConstr [‘extends’ ConstrApps]] | ids)

TopStats          ::=  TopStat {semi TopStat}
TopStat           ::=  Import
                    |  Export
                    |  {Annotation [nl]} {Modifier} Def
                    |  Extension
                    |  Packaging
                    |  PackageObject
                    |  EndMarker
                    |
Packaging         ::=  ‘package’ QualId :<<< TopStats >>>
PackageObject     ::=  ‘package’ ‘object’ ObjectDef

CompilationUnit   ::=  {‘package’ QualId semi} TopStats
```

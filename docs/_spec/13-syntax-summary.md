---
title: Syntax Summary
layout: default
chapter: 13
---

# Syntax Summary

The following descriptions of Scala tokens uses literal characters `‘c’` when referring to the ASCII fragment `\u0000` – `\u007F`.

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
alphaid          ::=  upper idrest
                   |  varid
plainid          ::=  alphaid
                   |  op
id               ::=  plainid
                   |  ‘`’ { charNoBackQuoteOrNewline | escapeSeq } ‘`’
idrest           ::=  {letter | digit} [‘_’ op]
quoteId          ::=  ‘'’ alphaid
spliceId         ::=  ‘$’ alphaid ;

integerLiteral   ::=  (decimalNumeral | hexNumeral | binaryNumeral) [‘L’ | ‘l’]
decimalNumeral   ::=  ‘0’ | digit [{digit | ‘_’} digit]
hexNumeral       ::=  ‘0’ (‘x’ | ‘X’) hexDigit [{hexDigit | ‘_’} hexDigit]
binaryNumeral    ::=  ‘0’ (‘b’ | ‘B’) binaryDigit [{binaryDigit | ‘_’} binaryDigit]

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

comment          ::=  ‘/*’ “any sequence of characters; nested comments are allowed” ‘*/’
                   |  ‘//’ “any sequence of characters up to end of line”

nl               ::=  ´\mathit{“new line character”}´
semi             ::=  ‘;’ |  nl {nl}
```

## Optional Braces

```
colon         ::=  ':'    -- with side conditions explained in 01-literal-syntax.md
 <<< ts >>>   ::=  ‘{’ ts ‘}’
                |  indent ts outdent
:<<< ts >>>   ::=  [nl] ‘{’ ts ‘}’
                |  colon indent ts outdent
```

## Context-free Syntax

´\color{red}{\text{TODO SCALA3: Once we're done porting the spec, make sure that
the references to grammar productions in the rest of the spec match this.}}´

The context-free syntax of Scala is given by the following EBNF grammar:

```ebnf
RefineDcl         ::=  ‘val’ ValDcl
                    |  ‘def’ DefDcl
                    |  ‘type’ {nl} TypeDcl
Dcl               ::=  RefineDcl
                    |  ‘var’ VarDcl
ValDcl            ::=  ids ‘:’ Type
VarDcl            ::=  ids ‘:’ Type
DefDcl            ::=  DefSig ‘:’ Type
DefSig            ::=  id [DefTypeParamClause] [TypelessClauses] [DefImplicitClause]
TypeDcl           ::=  id [TypeParamClause] {FunParamClause} TypeBounds

Def               ::=  ‘val’ PatDef
                    |  ‘var’ PatDef
                    |  ‘def’ DefDef
                    |  ‘type’ {nl} TypeDcl
                    |  TmplDef
PatDef            ::=  ids [‘:’ Type] ‘=’ Expr
                    |  Pattern2 [‘:’ Type] ‘=’ Expr
DefDef            ::=  DefSig [‘:’ Type] ‘=’ Expr
                    |  ‘this’ TypelessClauses [DefImplicitClause] ‘=’ ConstrExpr

TmplDef           ::=  ([‘case’] ‘class’ | ‘trait’) ClassDef
                    |  [‘case’] ‘object’ ObjectDef
                    |  ‘enum’ EnumDef
                    |  ‘given’ GivenDef
ClassDef          ::=  id ClassConstr [Template]
ClassConstr       ::=  [ClsTypeParamClause] [ConstrMods] ClsParamClauses
ConstrMods        ::=  {Annotation} [AccessModifier]
ObjectDef         ::=  id [Template]
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
                    |  {Annotation [nl]} {Modifier} Dcl
                    |  Extension
                    |  Expr1
                    |  EndMarker
                    |
SelfType          ::=  id [‘:’ InfixType] ‘=>’
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

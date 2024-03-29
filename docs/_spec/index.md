---
title: Scala Language Specification
layout: toc
---

{% include table-of-contents.yml %}

#### Authors and Contributors

Martin Odersky, Philippe Altherr, Vincent Cremet, Sébastien Doeraene, Gilles Dubochet, Burak Emir, Philipp Haller, Stéphane Micheloud, Nikolay Mihaylov, Adriaan Moors, Lukas Rytz, Michel Schinz, Erik Stenman, Matthias Zenger

Markdown Conversion by Iain McGinniss.

#### Disclaimer: Work in Progress

This specification for Scala 3 is still a work in progress.
Some features added in Scala 3 are missing.
Here is a list of the most important missing aspects:

* `extension` methods
* `given` and `using` contextual abstractions
* type-class derivation with `derives`
* behavior of structural types based on `Selectable`
* meta-programming with `inline def`s and quotes
* match types (pending a [Scala Improvement Proposal](https://github.com/scala/improvement-proposals/pull/65) with a full specification)

For those features, the best reference so far remains [the Scala 3 Reference](https://docs.scala-lang.org/scala3/reference/).

#### Preface

Scala is a Java-like programming language which unifies
object-oriented and functional programming.  It is a pure
object-oriented language in the sense that every value is an
object. Types and behavior of objects are described by
classes. Classes can be composed using mixin composition.  Scala is
designed to work seamlessly with less pure but mainstream
object-oriented languages like Java.

Scala is a functional language in the sense that every function is a
value. Nesting of function definitions and higher-order functions are
naturally supported. Scala also supports a general notion of pattern
matching which can model the algebraic types used in many functional
languages.

Scala has been designed to interoperate seamlessly with Java.
Scala classes can call Java methods, create Java objects, inherit from Java
classes and implement Java interfaces. None of this requires interface
definitions or glue code.

Scala has been developed from 2001 in the programming methods laboratory at EPFL.
Version 1.0 was released in November 2003.
This document describes the third version of the language, which was released in May 2021.
It acts as a reference for the language definition and some core library modules.
It is not intended to teach Scala or its concepts; for this there are [other documents](https://docs.scala-lang.org/).

Scala has been a collective effort of many people. The design and the
implementation of version 1.0 was completed by Philippe Altherr,
Vincent Cremet, Gilles Dubochet, Burak Emir, Stéphane Micheloud,
Nikolay Mihaylov, Michel Schinz, Erik Stenman, Matthias Zenger, and
the author. Iulian Dragos, Gilles Dubochet, Philipp Haller, Sean
McDirmid, Lex Spoon, and Geoffrey Washburn joined in the effort to
develop the second version of the language and tools.  Gilad Bracha,
Craig Chambers, Erik Ernst, Matthias Felleisen, Shriram Krishnamurti,
Gary Leavens, Sebastian Maneth, Erik Meijer, Klaus Ostermann, Didier
Rémy, Mads Torgersen, and Philip Wadler have shaped the design of
the language through lively and inspiring discussions and comments on
previous versions of this document.  The contributors to the Scala
mailing list have also given very useful feedback that helped us
improve the language and its tools.

---
layout: default
title: "Docs"
---

Dotty Documentation
===================
The Dotty compiler is currently somewhat lacking in documentation - PRs
welcome! But, we've attempted to gather the most essential knowledge in these
pages.

Index
-----
* [Blog](blog/)
* Usage
    - [Migrating from Scala 2](usage/migrating.md)
    - [Using Dotty with sbt](usage/sbt-projects.md)
* [Contributing](contributing/index.md)
    - [Getting Started](contributing/getting-started.md) details on how to run
      tests, use the cli scripts
    - [Workflow](contributing/workflow.md) common dev patterns and hints
    - [Eclipse](contributing/eclipse.md) setting up dev environment
    - [Intellij-IDEA](contributing/intellij-idea.md) setting up dev environment
* Internals document the compiler internals
    - [Project Structure](internals/overall-structure.md)
      of the project
    - [Backend](internals/backend.md) details on the bytecode backend
    - [Contexts](internals/contexts.md) details the use of `Context` in the
      compiler
    - [Dotty vs Scala2](internals/dotc-scalac.md)
    - [Higher Kinded Type Scheme](internals/higher-kinded-v2.md)
      scheme
    - [Periods](internals/periods.md)
    - [Type System](internals/type-system.md)

//> using options -Yno-experimental

import annotation.experimental

package foo {
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions

  package bar {
    def foo = 1 // marked as @experimental because of the language imports
  }
}

package foo2 {
  // ok: all definitions are top-level @experimental
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions

  package bar {
    @experimental def foo = 1
  }
}

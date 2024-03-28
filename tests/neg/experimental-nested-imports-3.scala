//> using options -Yno-experimental

import annotation.experimental

class Class1:
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // error

object Object1:
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // error

def fun1 =
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // error

val value1 =
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // error

//> using options -Yno-experimental

import scala.language.experimental.namedTypeArguments

def namedTypeArgumentsFun[T, U]: Int =
  namedTypeArgumentsFun[T = Int, U = Int]
  namedTypeArgumentsFun[U = Int, T = Int]

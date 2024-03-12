package user

import defn.Macro

object Inline extends Macro {
  inline def callMacro(): Int =
    ${ impl() }
}

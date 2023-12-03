import language.experimental.erasedDefinitions

object Test {

  type T = (erased x: Int)

}  // error: `=>' expected

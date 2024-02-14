import scala.language.experimental.erasedDefinitions

erased def test1(x: Int): Int = x // error
erased def test2(erased x: Int): Int = x
erased def test3(erased x: Int, erased y: Int): Int = x
erased def test4(erased x: Int, y: Int): Int = x // error
erased def test5(x: Int, erased y: Int): Int = y // error
erased def test6(x: Int, y: Int): Int = y // error // error
erased def test7(erased x: Int)(erased y: Int): Int = x
erased def test8(erased x: Int)(y: Int): Int = x // error
erased def test9(x: Int)(erased y: Int): Int = y // error
erased def test10(x: Int)(y: Int): Int = y // error // error

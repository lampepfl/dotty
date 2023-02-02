import language.experimental.saferExceptions

class A extends Exception
class B extends Exception
class C extends Exception
def multipleErrors() throws A, B, C : Int = ???

def main : Int =
  try
    try
      multipleErrors()
    catch
      case _: A => ???
  catch
    case _: B => ???
    case _: C => ???
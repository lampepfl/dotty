import scala.language.unsafeNulls


def s: String | Null = ???

def tryString = try s catch {
  case _: NullPointerException => null
  case _ => ""
}

def tryString2 = try s catch {
  case _: NullPointerException => ""
  case _ => s
}

def loadClass(classLoader: ClassLoader, name: String): Class[?] =
  try classLoader.loadClass(name)
  catch {
    case _ =>
      throw new Exception()
  }

def loadClass2(classLoader: ClassLoader, name: String): Class[?] =
  try classLoader.loadClass(name)
  catch {
    case _ => null
  }
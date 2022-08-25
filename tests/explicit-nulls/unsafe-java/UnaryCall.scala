import scala.language.experimental.unsafeJavaReturn

import java.lang.reflect.Method

def getMethods(f: String): List[Method] =
  val clazz = Class.forName(f)
  val methods = clazz.getMethods
  if methods == null then List()
  else methods.toList

def getClass(o: AnyRef): Class[?] = o.getClass

type F[A, B] = A => B

type LongComplictedType
type B = LongComplictedType

case class Test1[T](v: T)
case class Test2[T <: B](v: T)
case class Test3[T <: LongComplictedType](v: T)

class MyClass
type MyAlias = MyClass


// these tests check that the inferred types are not dealiased

def a = Test1(??? : F[Int, String])
def b: Int = a // error

def c: F[Int, String] = ???
def d: Int = c // error

def e = { val v = ??? : F[Int, String]; v }
def f: Int = e // error

def g = Test1(??? : B)
def h: Int = g // error

def i = Test2(??? : B)
def j: Int = i // error

def k = Test3(??? : B)
def l: Int = k // error

def m: MyAlias = new MyClass
def n: Int = m // error

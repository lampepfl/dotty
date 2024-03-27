package example

import scala.language/*->scala::language.*/.implicitConversions/*->scala::language.implicitConversions.*/

class Synthetic/*<-example::Synthetic#*/ {
  List/*->scala::package.List.*/(1).map/*->scala::collection::immutable::List#map().*/(_ +/*->scala::Int#`+`(+4).*/ 2)
  Array/*->scala::Array.*/.empty/*->scala::Array.empty().*/[Int/*->scala::Int#*/].headOption/*->scala::collection::ArrayOps#headOption().*/
  "fooo".stripPrefix/*->scala::collection::StringOps#stripPrefix().*/("o")

  // See https://github.com/scalameta/scalameta/issues/977
  val Name/*<-example::Synthetic#Name.*/ = "name:(.*)".r/*->scala::collection::StringOps#r().*/
  val x/*<-example::Synthetic#x.*/ #::/*->scala::package.`#::`.*/ xs/*<-example::Synthetic#xs.*/ = LazyList/*->scala::package.LazyList.*/(1, 2): @unchecked
  val Name/*->example::Synthetic#Name.*/(name/*<-example::Synthetic#name.*/) = "name:foo": @unchecked
  1 #:: 2 #:: LazyList/*->scala::package.LazyList.*/.empty/*->scala::collection::immutable::LazyList.empty().*/

  val a1/*<-example::Synthetic#a1.*/ #::/*->scala::package.`#::`.*/ a2/*<-example::Synthetic#a2.*/ #::/*->scala::package.`#::`.*/ as/*<-example::Synthetic#as.*/ = LazyList/*->scala::package.LazyList.*/(1, 2): @unchecked

  val lst/*<-example::Synthetic#lst.*/ = 1 #:: 2 #:: LazyList/*->scala::package.LazyList.*/.empty/*->scala::collection::immutable::LazyList.empty().*/

  for (x/*<-local0*/ <- 1 to/*->scala::runtime::RichInt#to().*/ 10; y/*<-local1*/ <- 0 until/*->scala::runtime::RichInt#until().*/ 10) println/*->scala::Predef.println(+1).*/(x/*->local0*/ ->/*->scala::Predef.ArrowAssoc#`->`().*/ x/*->local0*/)
  for (i/*<-local2*/ <- 1 to/*->scala::runtime::RichInt#to().*/ 10; j/*<-local3*/ <- 0 until/*->scala::runtime::RichInt#until().*/ 10) yield (i/*->local2*/, j/*->local3*/)
  for (i/*<-local4*/ <- 1 to/*->scala::runtime::RichInt#to().*/ 10; j/*<-local5*/ <- 0 until/*->scala::runtime::RichInt#until().*/ 10 if i/*->local4*/ %/*->scala::Int#`%`(+3).*/ 2 ==/*->scala::Int#`==`(+3).*/ 0) yield (i/*->local4*/, j/*->local5*/)

  object s/*<-example::Synthetic#s.*/ {
    def apply/*<-example::Synthetic#s.apply().*/() = 2
    s()
    s.apply/*->example::Synthetic#s.apply().*/()
    case class Bar/*<-example::Synthetic#s.Bar#*/()
    Bar/*->example::Synthetic#s.Bar.*/()
    null.asInstanceOf/*->scala::Any#asInstanceOf().*/[Int/*->scala::Int#*/ => Int/*->scala::Int#*/](2)
  }

  class J/*<-example::Synthetic#J#*/[T/*<-example::Synthetic#J#[T]*/: /*<-example::Synthetic#J#evidence$1.*/Manifest/*->scala::Predef.Manifest#*//*->example::Synthetic#J#[T]*/] { val arr/*<-example::Synthetic#J#arr.*/ = Array/*->scala::Array.*/.empty/*->scala::Array.empty().*/[T/*->example::Synthetic#J#[T]*/] }

  class F/*<-example::Synthetic#F#*/
  implicit val ordering/*<-example::Synthetic#ordering.*/: Ordering/*->scala::package.Ordering#*/[F/*->example::Synthetic#F#*/] = ???/*->scala::Predef.`???`().*/
  val f/*<-example::Synthetic#f.*/: Ordered/*->scala::package.Ordered#*/[F/*->example::Synthetic#F#*/] = new F/*->example::Synthetic#F#*/

  import scala.concurrent.ExecutionContext/*->scala::concurrent::ExecutionContext.*/.Implicits/*->scala::concurrent::ExecutionContext.Implicits.*/.global/*->scala::concurrent::ExecutionContext.Implicits.global().*/
  for {
    a/*<-local6*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(1)
    b/*<-local7*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(2)
  } println/*->scala::Predef.println(+1).*/(a/*->local6*/)
  for {
    a/*<-local8*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(1)
    b/*<-local9*/ <- scala.concurrent.Future/*->scala::concurrent::Future.*/.successful/*->scala::concurrent::Future.successful().*/(2)
    if a/*->local8*/ </*->scala::Int#`<`(+3).*/ b/*->local9*/
  } yield a/*->local8*/

  object Contexts/*<-example::Synthetic#Contexts.*/ {
    def foo/*<-example::Synthetic#Contexts.foo().*/(x/*<-example::Synthetic#Contexts.foo().(x)*/: Int/*->scala::Int#*/)(using Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
    def m1/*<-example::Synthetic#Contexts.m1().*/(using Int/*->scala::Int#*/) = foo/*->example::Synthetic#Contexts.foo().*/(0)
    def m2/*<-example::Synthetic#Contexts.m2().*/(using x/*<-example::Synthetic#Contexts.m2().(x)*/: Int/*->scala::Int#*/) = foo/*->example::Synthetic#Contexts.foo().*/(0)
    def m3/*<-example::Synthetic#Contexts.m3().*/ =
      given x/*<-local10*/: Int/*->scala::Int#*/ = 1
      foo/*->example::Synthetic#Contexts.foo().*/(x/*->local10*/)
    def m4/*<-example::Synthetic#Contexts.m4().*/ =
      given Int/*->scala::Int#*/ = 1
      foo/*->example::Synthetic#Contexts.foo().*/(0)
  }
}

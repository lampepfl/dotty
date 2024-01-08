package example

import scala.math.Ordering/*->scala::math::Ordering.*//*->scala::math::Ordering#*/
import scala.language/*->scala::language.*/.existentials/*->scala::language.existentials.*/

class Methods/*<-example::Methods#*/[T/*<-example::Methods#[T]*/] {
  class List/*<-example::Methods#List#*/[T/*<-example::Methods#List#[T]*/]
  type AList/*<-example::Methods#AList#*/[T/*<-example::Methods#AList#[T]*/] = List/*->example::Methods#List#*/[T/*->example::Methods#AList#[T]*/]
  def m1/*<-example::Methods#m1().*/ = ???/*->scala::Predef.`???`().*/
  def m2/*<-example::Methods#m2().*/() = ???/*->scala::Predef.`???`().*/
  def m3/*<-example::Methods#m3().*/(x/*<-example::Methods#m3().(x)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m4/*<-example::Methods#m4().*/(x/*<-example::Methods#m4().(x)*/: Int/*->scala::Int#*/)(y/*<-example::Methods#m4().(y)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m5/*<-example::Methods#m5().*/(x/*<-example::Methods#m5().(x)*/: String/*->scala::Predef.String#*/) = ???/*->scala::Predef.`???`().*/
  def m5/*<-example::Methods#m5(+1).*/(x/*<-example::Methods#m5(+1).(x)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m6/*<-example::Methods#m6().*/(x/*<-example::Methods#m6().(x)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m6/*<-example::Methods#m6(+1).*/(x/*<-example::Methods#m6(+1).(x)*/: List/*->example::Methods#List#*/[T/*->example::Methods#[T]*/]) = ???/*->scala::Predef.`???`().*/
  def m6/*<-example::Methods#m6(+2).*/(x/*<-example::Methods#m6(+2).(x)*/: scala.List/*->scala::package.List#*/[T/*->example::Methods#[T]*/]) = ???/*->scala::Predef.`???`().*/
  def m7/*<-example::Methods#m7().*/[U/*<-example::Methods#m7().[U]*/: Ordering/*->example::Methods#m7().[U]*//*<-example::Methods#m7().(evidence$1)*/](c/*<-example::Methods#m7().(c)*/: Methods/*->example::Methods#*/[T/*->example::Methods#[T]*/], l/*<-example::Methods#m7().(l)*/: List/*->example::Methods#List#*/[U/*->example::Methods#m7().[U]*/]) = ???/*->scala::Predef.`???`().*/
  def `m8()./*<-example::Methods#`m8().`().*/`() = ???/*->scala::Predef.`???`().*/
  class `m9()./*<-example::Methods#`m9().`#*/`
  def m9/*<-example::Methods#m9().*/(x/*<-example::Methods#m9().(x)*/: `m9().`/*->example::Methods#`m9().`#*/) = ???/*->scala::Predef.`???`().*/
  def m10/*<-example::Methods#m10().*/(x/*<-example::Methods#m10().(x)*/: AList/*->example::Methods#AList#*/[T/*->example::Methods#[T]*/]) = ???/*->scala::Predef.`???`().*/
  def m11/*<-example::Methods#m11().*/(x/*<-example::Methods#m11().(x)*/: Predef/*->scala::Predef.*/.type) = ???/*->scala::Predef.`???`().*/
  def m11/*<-example::Methods#m11(+1).*/(x/*<-example::Methods#m11(+1).(x)*/: Example/*->example::Example.*/.type) = ???/*->scala::Predef.`???`().*/
  def m12a/*<-example::Methods#m12a().*/(x/*<-example::Methods#m12a().(x)*/: {}) = ???/*->scala::Predef.`???`().*/
  def m12b/*<-example::Methods#m12b().*/(x/*<-example::Methods#m12b().(x)*/: { val x/*<-local0*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
  def m12c/*<-example::Methods#m12c().*/(x/*<-example::Methods#m12c().(x)*/: { val x/*<-local1*/: Int/*->scala::Int#*/; def y/*<-local2*/: Int/*->scala::Int#*/ }) = ???/*->scala::Predef.`???`().*/
  def m13/*<-example::Methods#m13().*/(x/*<-example::Methods#m13().(x)*/: Int/*->scala::Int#*/ @unchecked/*->scala::unchecked#*/) = ???/*->scala::Predef.`???`().*/
  def m15/*<-example::Methods#m15().*/(x/*<-example::Methods#m15().(x)*/: => Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m16/*<-example::Methods#m16().*/(x/*<-example::Methods#m16().(x)*/: Int/*->scala::Int#*/*) = ???/*->scala::Predef.`???`().*/
  object m17/*<-example::Methods#m17.*/ { def m/*<-example::Methods#m17.m().*/() = ???/*->scala::Predef.`???`().*/ }
  def m17/*<-example::Methods#m17().*/(a/*<-example::Methods#m17().(a)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m17/*<-example::Methods#m17(+1).*/(b/*<-example::Methods#m17(+1).(b)*/: String/*->scala::Predef.String#*/) = ???/*->scala::Predef.`???`().*/
  val m18/*<-example::Methods#m18.*/ = m17/*->example::Methods#m17.*/
  def m18/*<-example::Methods#m18().*/(a/*<-example::Methods#m18().(a)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m18/*<-example::Methods#m18(+1).*/(b/*<-example::Methods#m18(+1).(b)*/: String/*->scala::Predef.String#*/) = ???/*->scala::Predef.`???`().*/
  def m19/*<-example::Methods#m19().*/(x/*<-example::Methods#m19().(x)*//*<-example::Methods#m19$default$3().(x)*/: Int/*->scala::Int#*/, y/*<-example::Methods#m19().(y)*//*<-example::Methods#m19$default$3().(y)*/: Int/*->scala::Int#*/ = 2)(z/*<-example::Methods#m19().(z)*/: Int/*->scala::Int#*/ = 3) = ???/*->scala::Predef.`???`().*/
  def m20/*<-example::Methods#m20().*/(a/*<-example::Methods#m20().(a)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/
  def m20/*<-example::Methods#m20(+1).*/(b/*<-example::Methods#m20(+1).(b)*/: String/*->scala::Predef.String#*/) = ???/*->scala::Predef.`???`().*/
  var m20/*<-example::Methods#m20(+2).*/ = m17/*->example::Methods#m17.*/
}

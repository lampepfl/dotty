object Test_depmatch/*<-_empty_::Test_depmatch.*/ {
  type Foo/*<-_empty_::Test_depmatch.Foo#*/ = Int/*->scala::Int#*/ { type U/*<-_empty_::Test_depmatch.Foo#`<refinement>`#U#*/ }
  type Bar/*<-_empty_::Test_depmatch.Bar#*/[T/*<-_empty_::Test_depmatch.Bar#[T]*/] = T/*->_empty_::Test_depmatch.Bar#[T]*/ match {
    case Unit/*->scala::Unit#*/ => Unit/*->scala::Unit#*/
  }
  inline def baz/*<-_empty_::Test_depmatch.baz().*/(foo/*<-_empty_::Test_depmatch.baz().(foo)*/: Foo/*->_empty_::Test_depmatch.Foo#*/): Unit/*->scala::Unit#*/ = {
    val v/*<-local0*/: Bar/*->_empty_::Test_depmatch.Bar#*/[foo/*->_empty_::Test_depmatch.baz().(foo)*/.U/*->_empty_::Test_depmatch.Foo#`<refinement>`#U#*/] = ???/*->scala::Predef.`???`().*/
  }
}

/*<-_empty_::toplevel$package.*/inline val a/*<-_empty_::toplevel$package.a.*/ = ""
extension (x/*<-_empty_::toplevel$package.combine().(x)*/: Int/*->scala::Int#*/) def combine/*<-_empty_::toplevel$package.combine().*/ (y/*<-_empty_::toplevel$package.combine().(y)*/: Int/*->scala::Int#*/) = x/*->_empty_::toplevel$package.combine().(x)*/ +/*->scala::Int#`+`(+4).*/ y/*->_empty_::toplevel$package.combine().(y)*/
def combine/*<-_empty_::toplevel$package.combine(+1).*/(x/*<-_empty_::toplevel$package.combine(+1).(x)*/: Int/*->scala::Int#*/, y/*<-_empty_::toplevel$package.combine(+1).(y)*/: Int/*->scala::Int#*/, z/*<-_empty_::toplevel$package.combine(+1).(z)*/: Int/*->scala::Int#*/) = x/*->_empty_::toplevel$package.combine(+1).(x)*/ +/*->scala::Int#`+`(+4).*/ y/*->_empty_::toplevel$package.combine(+1).(y)*/ +/*->scala::Int#`+`(+4).*/ z/*->_empty_::toplevel$package.combine(+1).(z)*/
def combine/*<-_empty_::toplevel$package.combine(+2).*/ = 0
def foo/*<-_empty_::toplevel$package.foo().*/ = "foo"
/*<-_empty_::MyProgram#*//*->_empty_::toplevel$package.MyProgram().*//*->scala::util::CommandLineParser.parseArgument().*//*->_empty_::MyProgram#main().(args)*//*->scala::util::CommandLineParser.FromString.given_FromString_Int.*//*->scala::util::CommandLineParser.showError().*//*->local0*/@main/*->scala::main#*/ def MyProgram/*<-_empty_::toplevel$package.MyProgram().*/(times/*<-_empty_::toplevel$package.MyProgram().(times)*/: Int/*->scala::Int#*/): Unit/*->scala::Unit#*/ = (/*->scala::LowPriorityImplicits#intWrapper().*/1 to/*->scala::runtime::RichInt#to().*/ times/*->_empty_::toplevel$package.MyProgram().(times)*/) foreach/*->scala::collection::immutable::Range#foreach().*/ (_ => println/*->scala::Predef.println(+1).*/("hello"))

trait Ord/*<-_empty_::Ord#*/[T/*<-_empty_::Ord#[T]*/]:
   def compare/*<-_empty_::Ord#compare().*/(x/*<-_empty_::Ord#compare().(x)*/: T/*->_empty_::Ord#[T]*/, y/*<-_empty_::Ord#compare().(y)*/: T/*->_empty_::Ord#[T]*/): Int/*->scala::Int#*/

given intOrd/*<-_empty_::toplevel$package.intOrd.*/: Ord/*->_empty_::Ord#*/[Int/*->scala::Int#*/] with
   def compare/*<-_empty_::toplevel$package.intOrd.compare().*/(x/*<-_empty_::toplevel$package.intOrd.compare().(x)*/: Int/*->scala::Int#*/, y/*<-_empty_::toplevel$package.intOrd.compare().(y)*/: Int/*->scala::Int#*/) = ???/*->scala::Predef.`???`().*/

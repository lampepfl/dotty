//import scala.language.experimental.saferExceptions

//@throws[Exception]
def test throws Exception : Int = ???

def main : Int =
  try
    test
  catch
    case _: Exception => ???
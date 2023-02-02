import java.io.IOException

def test throws IOException, GenericExc[Int] : Unit = ()
/*
-- [E040] Syntax Error: tests\safer-exceptions\neg\t01.scala:5:9 ---------------
  5 |def test throws IOException, GenericExc[Int] : Unit = ()
  |         ^^^^^^
  |         '=' expected, but identifier found
*/
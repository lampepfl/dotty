class Impl

def (impl: Impl) prop given Int = ???//the[Int]


def main(args: Array[String]): Unit = {
  given as Int = 3
  println(new Impl().prop given 3)
}
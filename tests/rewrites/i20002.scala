object Reactions {
  def main: Unit = {
    Reactions += {
      case 0 =>
      case 1 =>
    }

    def bar: Int = ???

    bar match {
      case 0 =>
      case 1 =>
    }

    def partPartial(i: Int): PartialFunction[Int, Unit] = {
      case `i` =>
    }

    Reactions += {
      val pp1 = partPartial(1)
      val pp2 = partPartial(2)
      def codeBlock = {
        ???
        ???
      }
      pp1 orElse pp2
    }

    val partialFunction = partPartial(1) orElse partPartial(2)
    Reactions += {
      partialFunction
    }
  }
  def +=(f: PartialFunction[Int, Unit]) = {
    ???
  }

}

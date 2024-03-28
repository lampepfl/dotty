import language.experimental.fewerBraces

def test(y: Int) =
  val firstValue = y
    * y
  val secondValue =
    firstValue
    +
      if firstValue < 0 then 1 else 0
    +
      if y < 0 then y else -y

  val result =
    firstValue < secondValue
    ||
      val thirdValue = firstValue * secondValue
      thirdValue > 100
    ||
      def avg(x: Double, y: Double) = (x + y)/2
      avg(firstValue, secondValue) > 0.0
    ||
      firstValue
      * secondValue
      *
        val firstSquare = firstValue * firstValue
        firstSquare + firstSquare
      <=
        firstValue `max` secondValue

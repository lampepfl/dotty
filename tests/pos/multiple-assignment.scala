def tu(): (Int, Boolean) = (1, true)

@main def ma(): Unit =
  var x = 0
  var y = false
  var z = "a"

  ((x, y), z) = (tu(), "b")


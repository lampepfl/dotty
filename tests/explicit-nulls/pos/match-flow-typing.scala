def m(): String = {
  var x: String|Null = "foo"
  1 match {
    case 1 => x = x
  }
  if(x == null) "foo"
  else x
}

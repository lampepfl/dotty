def m(): String = {
  var x: String|Null = "foo"
  1 match {
    case 1 => x = x
  }
  if(x == null) "foo"
  else x
}

def m2(): String = {
  var x: String|Null = "foo"
  try {
    x = x
  } catch {
    case e => x = x
  } finally {
    x = x
  }
  if(x == null) "foo"
  else x
}

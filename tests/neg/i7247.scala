val x = "foo" match
  case _: (a *: (b: Any)) => ???  // error, now OK since (b: Any) is a named tuple
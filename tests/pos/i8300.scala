// https://github.com/scala/scala3/issues/8300

type Bar[X] = X match {
  case List[a] => List[Tuple1[a]]
  case Set[a]  => Set[Tuple1[a]]
}

object Test:
  (Set(1, 2, 3), List("a", "b")).map: 
    a => a.map(Tuple1(_)).asInstanceOf[Bar[a.type]]

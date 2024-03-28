  def get(using Int): String = summon[Int].toString

  def pf2: PartialFunction[String, Int ?=> String] = { // error
    case "hoge" => get
    case "huga" => get
  }

  type IS = Int ?=> String

  def pf3: PartialFunction[String, IS] = { // error
    case "hoge" => get
    case "huga" => get
  }



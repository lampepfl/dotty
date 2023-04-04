
object Use:
  val repo = new Repo[String]
  val v = repo.summonEncoder // error happens here!

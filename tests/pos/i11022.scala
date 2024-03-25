//> using options -Werror -deprecation
@deprecated("no CaseClass")
case class CaseClass(rgb: Int)

case class K(@deprecated("don't use k, ok?","0.1") k: Int)

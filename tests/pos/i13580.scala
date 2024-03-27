//> using options -language:experimental.modularity -source future
trait IntWidth:
  type Out
given IntWidth:
  type Out = 155

trait IntCandidate:
  type Out
given (using tracked val w: IntWidth) => IntCandidate:
  type Out = w.Out

val x = summon[IntCandidate]
val xx = summon[x.Out =:= 155]

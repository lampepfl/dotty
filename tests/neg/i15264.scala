import language.`3.5`
object priority:
    // lower number = higher priority
    class Prio0 extends Prio1
    object Prio0 { given Prio0() }

    class Prio1 extends Prio2
    object Prio1 { given Prio1() }

    class Prio2
    object Prio2 { given Prio2() }

object repro:
    // analogous to cats Eq, Hash, Order:
    class A[V]
    class B[V] extends A[V]
    class C[V] extends A[V]

    class Q[V]

    object context:
        // prios work here, which is cool
        given[V](using priority.Prio0): C[V] = new C[V]
        given[V](using priority.Prio1): B[V] = new B[V]
        given[V](using priority.Prio2): A[V] = new A[V]

    object exports:
        // so will these exports
        export context.given

    // if you import these don't import from 'context' above
    object qcontext:
        // base defs, like what you would get from cats
        given gb: B[Int] = new B[Int]
        given gc: C[Int] = new C[Int]

        // these seem like they should work but don't
        given gcq[V](using p0: priority.Prio0)(using c: C[V]): C[Q[V]] = new C[Q[V]]
        given gbq[V](using p1: priority.Prio1)(using b: B[V]): B[Q[V]] = new B[Q[V]]
        given gaq[V](using p2: priority.Prio2)(using a: A[V]): A[Q[V]] = new A[Q[V]]

object test1:
    import repro.*
    import repro.exports.given

    // these will work
    val a = summon[A[Int]]

object test2:
    import repro.*
    import repro.qcontext.given

    // This one will fail as ambiguous - prios aren't having an effect.
    // Priorities indeed don't have an effect if the result is already decided
    // without using clauses, they onyl act as a tie breaker.
    // With the new resolution rules, it's ambiguous since we pick `gaq` for
    // summon, and that needs an A[Int], but there are only the two competing choices
    // qb and qc.
    val a = summon[A[Q[Int]]] // error: ambiguous between qb and qc for A[Int]

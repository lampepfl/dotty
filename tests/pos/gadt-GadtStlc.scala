import language.experimental.namedTypeArguments
object GadtStlc {
  // creates type-level "strings" like M[M[M[W]]]
  object W
  type W = W.type
  class M[A]


  // variable with name A
  // Var[W]
  // Var[M[W]]
  sealed trait Var[A]
  object VarW extends Var[W]
  case class VarM[A]() extends Var[M[A]]

  // \s.e
  sealed trait Abs[S, E]
  case class AbsC[S, E](v: Var[S], b: E) extends Abs[Var[S], E]

  // e1 e2
  case class App[E1, E2](e1: E1, e2: E2)

  // T1 -> T2
  case class TyFun[T1, T2](t1: T1, t2: T2)

  // arbitrary base literal
  case object Lit
  type Lit = Lit.type

  // arbitrary base type
  case object TyBase
  type TyBase = TyBase.type

  // IN[G, (X, TY)] === evidence that binding (X, TY) is in environment G
  // x: ty \in G
  sealed trait IN[G, P]
  case class INOne[G, X, TY]() extends IN[(G, (X,TY)), (X, TY)]
  // this is wrong - we need evidence that A does not contain a binding for X
  case class INShift[G0, A, X, TY](in: IN[G0, (X, TY)]) extends IN[(G0, A), (X, TY)]

  // DER[G, E, TY] === evidence that G |- E : TY
  sealed trait DER[G, E, TY]
  case class DVar[G, G0, X, TY](
    in: IN[(G, G0), (Var[X], TY)]
  ) extends DER[(G, G0), Var[X], TY]

  case class DApp[G, E1, E2, TY1, TY2](
    d1: DER[G, E1, TyFun[TY1, TY2]],
    d2: DER[G, E2, TY1]
  ) extends DER[G, App[E1, E2], TY2]

  case class DAbs[G, X, E, TY1, TY2](
    d1: DER[(G, (Var[X], TY1)), E, TY2]
  ) extends DER[G, Abs[Var[X], E], TyFun[TY1, TY2]]

  case class DLit[G]() extends DER[G, Lit, TyBase]

  // forall G, a. G |- \x.x : a -> a
  def test1[G, TY]: DER[G, Abs[Var[W], Var[W]], TyFun[TY, TY]] =
    DAbs(DVar(INOne()))

  // forall G. G |- \x.x : Lit -> Lit
  def test2[G]: DER[G,  App[Abs[Var[W], Var[W]], Lit],  TyBase] =
    DApp(DAbs(DVar(INOne())), DLit())

  // forall G, c. G |- \x.\y. x y : (c -> c) -> c -> c
  def test3[G, TY]: DER[G,
    Abs[Var[W],
      Abs[Var[M[W]],
        App[Var[W], Var[M[W]]]
      ]
    ],
    TyFun[TyFun[TY, TY], TyFun[TY, TY]]
  ] = DAbs(DAbs(DApp(DVar(INShift(INOne())), DVar(INOne()))))


  // evidence that E is a value
  sealed trait ISVAL[E]
  case class ISVAL_Abs[X, E]()  extends ISVAL[Abs[Var[X], E]]
  case object ISVAL_Lit extends ISVAL[Lit]

  // evidence that E1 reduces to E2
  sealed trait REDUDER[E1, E2]
  case class EApp1[E1a, E1b, E2](
    ed: REDUDER[E1a, E1b]
  ) extends REDUDER[App[E1a, E2], App[E1b, E2]]

  case class EApp2[V1, E2a, E2b](
    isval: ISVAL[V1],
    ed: REDUDER[E2a, E2b]
  ) extends REDUDER[App[V1, E2a], App[V1, E2b]]

  case class EAppAbs[X, E, V2, R](
    isval: ISVAL[V2]
    // cheating - subst is hard
    // , subst: SUBST[E, X, V2, R]
  ) extends REDUDER[App[Abs[Var[X], E], V2], R]

  // evidence that V is a lambda
  sealed trait ISLAMBDA[V]
  case class ISLAMBDAC[X, E]() extends ISLAMBDA[Abs[Var[X], E]]

  // evidence that E reduces
  type REDUCES[E] = REDUDER[E, ?]

  def followsIsLambda[G, V, TY1, TY2](
    isval: ISVAL[V],
    der: DER[G, V, TyFun[TY1, TY2]]
  ): ISLAMBDA[V] = (isval, der) match {
    case (_: ISVAL_Abs[x, e], _) => ISLAMBDAC[x, e]()
  }

  // \empty |- E : TY ==> E is a value /\ E reduces to some E1
  def progress[E, TY](der: DER[Unit, E, TY]): Either[ISVAL[E], REDUCES[E]] =
    der match {
      case _: DAbs[g, a, e, ty1, ty2] => Left(ISVAL_Abs[a, e]())
      case DLit() => Left(ISVAL_Lit)
      case dapp: DApp[Unit, a, b, ty1, ty2] => progress(dapp.d1) match {
        case Right(r1) => Right(EApp1[E2 = b](r1))
        case Left(isv1) => progress(dapp.d2) match {
          case Right(r2) => Right(EApp2(isv1, r2))
          case Left(isv2) => followsIsLambda(isv1, dapp.d1) match {
            case _: ISLAMBDAC[x, e] => Right(EAppAbs[X = x, E = e, V2 = b](isv2))
          }
        }
      }
    }
}

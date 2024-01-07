object Test:
  type AnyKindMatchType1[X <: AnyKind] = X match // error: the scrutinee of a match type cannot be higher-kinded
    case Option[a] => Int

  type AnyKindMatchType2[X <: AnyKind] = X match // error: the scrutinee of a match type cannot be higher-kinded
    case Option => Int // error: Missing type parameter for Option

  type AnyKindMatchType3[X <: AnyKind] = X match // error: the scrutinee of a match type cannot be higher-kinded
    case _ => Int

  type AnyKindMatchType4[X <: Option] = X match // error: the scrutinee of a match type cannot be higher-kinded
    case _ => Int

  type AnyKindMatchType5[X[_]] = X match // error: the scrutinee of a match type cannot be higher-kinded
    case _ => Int
end Test

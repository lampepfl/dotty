package dotty.tools.dotc.reporting

//////////////////////////////////////////////////////////////////////////
// IMPORTANT                                                            //
// Only add new IDs at end of the enumeration list and never remove IDs //
//////////////////////////////////////////////////////////////////////////

/** Unique IDs identifying the messages, this will be used to reference documentation online.
 *
 *  @param isActive Whether or not the compile still emits this ErrorMessageID
 **/
enum ErrorMessageID(val isActive: Boolean = true) extends java.lang.Enum[ErrorMessageID]:

  case NoExplanationID // errorNumber: -1

  case EmptyCatchOrFinallyBlockID extends ErrorMessageID(isActive = false) // errorNumber: 0
  case EmptyCatchBlockID // errorNumber: 1
  case EmptyCatchAndFinallyBlockID // errorNumber: 2
  case DeprecatedWithOperatorID // errorNumber: 3
  case CaseClassMissingParamListID // errorNumber: 4
  case DuplicateBindID // errorNumber: 5
  case MissingIdentID // errorNumber: 6
  case TypeMismatchID // errorNumber: 7
  case NotAMemberID // errorNumber: 8
  case EarlyDefinitionsNotSupportedID // errorNumber: 9
  case TopLevelImplicitClassID extends ErrorMessageID(isActive = false) // errorNumber: 10
  case ImplicitCaseClassID // errorNumber: 11
  case ImplicitClassPrimaryConstructorArityID // errorNumber: 12
  case ObjectMayNotHaveSelfTypeID // errorNumber: 13
  case TupleTooLongID extends ErrorMessageID(isActive = false) // errorNumber: 14
  case RepeatedModifierID // errorNumber: 15
  case InterpolatedStringErrorID // errorNumber: 16
  case UnboundPlaceholderParameterID // errorNumber: 17
  case IllegalStartSimpleExprID // errorNumber: 18
  case MissingReturnTypeID // errorNumber: 19
  case YieldOrDoExpectedInForComprehensionID // errorNumber: 20
  case ProperDefinitionNotFoundID // errorNumber: 21
  case ByNameParameterNotSupportedID // errorNumber: 22
  case WrongNumberOfTypeArgsID // errorNumber: 23
  case IllegalVariableInPatternAlternativeID // errorNumber: 24
  case IdentifierExpectedID // errorNumber: 25
  case AuxConstructorNeedsNonImplicitParameterID // errorNumber: 26
  case VarArgsParamMustComeLastID // errorNumber: 27
  case IllegalLiteralID // errorNumber: 28
  case PatternMatchExhaustivityID // errorNumber: 29
  case MatchCaseUnreachableID // errorNumber: 30
  case SeqWildcardPatternPosID // errorNumber: 31
  case IllegalStartOfSimplePatternID // errorNumber: 32
  case PkgDuplicateSymbolID // errorNumber: 33
  case ExistentialTypesNoLongerSupportedID // errorNumber: 34
  case UnboundWildcardTypeID // errorNumber: 35
  case DanglingThisInPathID extends ErrorMessageID(isActive = false) // errorNumber: 36
  case OverridesNothingID // errorNumber: 37
  case OverridesNothingButNameExistsID // errorNumber: 38
  case ForwardReferenceExtendsOverDefinitionID // errorNumber: 39
  case ExpectedTokenButFoundID // errorNumber: 40
  case MixedLeftAndRightAssociativeOpsID // errorNumber: 41
  case CantInstantiateAbstractClassOrTraitID // errorNumber: 42
  case UnreducibleApplicationID // errorNumber: 43
  case OverloadedOrRecursiveMethodNeedsResultTypeID // errorNumber: 44
  case RecursiveValueNeedsResultTypeID // errorNumber: 45
  case CyclicReferenceInvolvingID // errorNumber: 46
  case CyclicReferenceInvolvingImplicitID // errorNumber: 47
  case SuperQualMustBeParentID // errorNumber: 48
  case AmbiguousReferenceID // errorNumber: 49
  case MethodDoesNotTakeParametersId // errorNumber: 50
  case AmbiguousOverloadID // errorNumber: 51
  case ReassignmentToValID // errorNumber: 52
  case TypeDoesNotTakeParametersID // errorNumber: 53
  case ParameterizedTypeLacksArgumentsID extends ErrorMessageID(isActive = false) // errorNumber: 54
  case VarValParametersMayNotBeCallByNameID // errorNumber: 55
  case MissingTypeParameterForID // errorNumber: 56
  case DoesNotConformToBoundID // errorNumber: 57
  case DoesNotConformToSelfTypeID // errorNumber: 58
  case DoesNotConformToSelfTypeCantBeInstantiatedID // errorNumber: 59
  case AbstractMemberMayNotHaveModifierID // errorNumber: 60
  case TopLevelCantBeImplicitID extends ErrorMessageID(isActive = false) // errorNumber: 61
  case TypesAndTraitsCantBeImplicitID // errorNumber: 62
  case OnlyClassesCanBeAbstractID // errorNumber: 63
  case AbstractOverrideOnlyInTraitsID // errorNumber: 64
  case TraitsMayNotBeFinalID // errorNumber: 65
  case NativeMembersMayNotHaveImplementationID // errorNumber: 66
  case OnlyClassesCanHaveDeclaredButUndefinedMembersID // errorNumber: 67
  case CannotExtendAnyValID // errorNumber: 68
  case CannotHaveSameNameAsID // errorNumber: 69
  case ValueClassesMayNotDefineInnerID // errorNumber: 70
  case ValueClassesMayNotDefineNonParameterFieldID // errorNumber: 71
  case ValueClassesMayNotDefineASecondaryConstructorID // errorNumber: 72
  case ValueClassesMayNotContainInitalizationID // errorNumber: 73
  case ValueClassesMayNotBeAbstractID // errorNumber: 74
  case ValueClassesMayNotBeContaintedID // errorNumber: 75
  case ValueClassesMayNotWrapAnotherValueClassID // errorNumber: 76
  case ValueClassParameterMayNotBeAVarID // errorNumber: 77
  case ValueClassNeedsExactlyOneValParamID // errorNumber: 78
  case OnlyCaseClassOrCaseObjectAllowedID extends ErrorMessageID(isActive = false) // errorNumber: 79
  case ExpectedTopLevelDefID extends ErrorMessageID(isActive = false) // errorNumber: 80
  case AnonymousFunctionMissingParamTypeID // errorNumber: 81
  case SuperCallsNotAllowedInlineableID // errorNumber: 82
  case NotAPathID // errorNumber: 83
  case WildcardOnTypeArgumentNotAllowedOnNewID // errorNumber: 84
  case FunctionTypeNeedsNonEmptyParameterListID // errorNumber: 85
  case WrongNumberOfParametersID // errorNumber: 86
  case DuplicatePrivateProtectedQualifierID // errorNumber: 87
  case ExpectedStartOfTopLevelDefinitionID // errorNumber: 88
  case MissingReturnTypeWithReturnStatementID // errorNumber: 89
  case NoReturnFromInlineableID // errorNumber: 90
  case ReturnOutsideMethodDefinitionID // errorNumber: 91
  case UncheckedTypePatternID // errorNumber: 92
  case ExtendFinalClassID // errorNumber: 93
  case EnumCaseDefinitionInNonEnumOwnerID // errorNumber: 94
  case ExpectedTypeBoundOrEqualsID // errorNumber: 95
  case ClassAndCompanionNameClashID // errorNumber: 96
  case TailrecNotApplicableID // errorNumber: 97
  case FailureToEliminateExistentialID // errorNumber: 98
  case OnlyFunctionsCanBeFollowedByUnderscoreID // errorNumber: 99
  case MissingEmptyArgumentListID // errorNumber: 100
  case DuplicateNamedTypeParameterID // errorNumber: 101
  case UndefinedNamedTypeParameterID // errorNumber: 102
  case IllegalStartOfStatementID // errorNumber: 1033
  case TraitIsExpectedID // errorNumber: 104
  case TraitRedefinedFinalMethodFromAnyRefID // errorNumber: 105
  case PackageNameAlreadyDefinedID // errorNumber: 106
  case UnapplyInvalidNumberOfArgumentsID // errorNumber: 107
  case UnapplyInvalidReturnTypeID // errorNumber: 108
  case StaticFieldsOnlyAllowedInObjectsID // errorNumber: 109
  case CyclicInheritanceID // errorNumber: 110
  case BadSymbolicReferenceID // errorNumber: 111
  case UnableToExtendSealedClassID // errorNumber: 112
  case SymbolHasUnparsableVersionNumberID // errorNumber: 113
  case SymbolChangedSemanticsInVersionID // errorNumber: 114
  case UnableToEmitSwitchID // errorNumber: 115
  case MissingCompanionForStaticID // errorNumber: 116
  case PolymorphicMethodMissingTypeInParentID // errorNumber: 117
  case ParamsNoInlineID // errorNumber: 118
  case JavaSymbolIsNotAValueID // errorNumber: 119
  case DoubleDefinitionID // errorNumber: 120
  case MatchCaseOnlyNullWarningID // errorNumber: 121
  case ImportedTwiceID // errorNumber: 122
  case TypeTestAlwaysDivergesID // errorNumber: 123
  case TermMemberNeedsNeedsResultTypeForImplicitSearchID // errorNumber: 124
  case ClassCannotExtendEnumID // errorNumber: 125
  case ValueClassParameterMayNotBeCallByNameID // errorNumber: 126
  case NotAnExtractorID // errorNumber: 127
  case MemberWithSameNameAsStaticID // errorNumber: 128
  case PureExpressionInStatementPositionID // errorNumber: 129
  case TraitCompanionWithMutableStaticID // errorNumber: 130
  case LazyStaticFieldID // errorNumber: 131
  case StaticOverridingNonStaticMembersID // errorNumber: 132
  case OverloadInRefinementID // errorNumber: 133
  case NoMatchingOverloadID // errorNumber: 134
  case StableIdentPatternID // errorNumber: 135
  case StaticFieldsShouldPrecedeNonStaticID // errorNumber: 136
  case IllegalSuperAccessorID // errorNumber: 137
  case TraitParameterUsedAsParentPrefixID // errorNumber: 138
  case UnknownNamedEnclosingClassOrObjectID // errorNumber: 139
  case IllegalCyclicTypeReferenceID // errorNumber: 140
  case MissingTypeParameterInTypeAppID // errorNumber: 141
  case SkolemInInferredID // errorNumber: 142
  case ErasedTypesCanOnlyBeFunctionTypesID // errorNumber: 143
  case CaseClassMissingNonImplicitParamListID // errorNumber: 144
  case EnumerationsShouldNotBeEmptyID // errorNumber: 145
  case IllegalParameterInitID // errorNumber: 146
  case RedundantModifierID // errorNumber: 147
  case TypedCaseDoesNotExplicitlyExtendTypedEnumID // errorNumber: 148
  case IllegalRedefinitionOfStandardKindID // errorNumber: 149
  case NoExtensionMethodAllowedID // errorNumber: 150
  case ExtensionMethodCannotHaveTypeParamsID // errorNumber: 151
  case ExtensionCanOnlyHaveDefsID // errorNumber: 152
  case UnexpectedPatternForSummonFromID // errorNumber: 153
  case AnonymousInstanceCannotBeEmptyID // errorNumber: 154
  case TypeSpliceInValPatternID extends ErrorMessageID(isActive = false) // errorNumber: 155
  case ModifierNotAllowedForDefinitionID // errorNumber: 156
  case CannotExtendJavaEnumID // errorNumber: 157
  case InvalidReferenceInImplicitNotFoundAnnotationID // errorNumber: 158
  case TraitMayNotDefineNativeMethodID // errorNumber: 159
  case JavaEnumParentArgsID // errorNumber: 160
  case AlreadyDefinedID // errorNumber: 161
  case CaseClassInInlinedCodeID // errorNumber: 162
  case OverrideTypeMismatchErrorID extends ErrorMessageID(isActive = false) // errorNumber: 163
  case OverrideErrorID // errorNumber: 164
  case MatchableWarningID // errorNumber: 165
  case CannotExtendFunctionID // errorNumber: 166
  case LossyWideningConstantConversionID // errorNumber: 167
  case ImplicitSearchTooLargeID // errorNumber: 168
  case TargetNameOnTopLevelClassID // errorNumber: 169
  case NotClassTypeID // errorNumber 170
  case MissingArgumentID // errorNumer 171
  case MissingImplicitArgumentID // errorNumber 172
  case CannotBeAccessedID // errorNumber 173
  case InlineGivenShouldNotBeFunctionID // errorNumber 174
  case ValueDiscardingID // errorNumber 175
  case UnusedNonUnitValueID // errorNumber 176
  case ConstrProxyShadowsID // errorNumber 177
  case MissingArgumentListID // errorNumber: 178
  case MatchTypeScrutineeCannotBeHigherKindedID // errorNumber: 179
  case AmbiguousExtensionMethodID // errorNumber 180
  case UnqualifiedCallToAnyRefMethodID // errorNumber: 181
  case NotConstantID // errorNumber: 182
  case ClosureCannotHaveInternalParameterDependenciesID // errorNumber: 183
  case MatchTypeNoCasesID // errorNumber: 184
  case UnimportedAndImportedID // errorNumber: 185
  case ImplausiblePatternWarningID // errorNumber: 186
  case SynchronizedCallOnBoxedClassID // errorNumber: 187
  case VarArgsParamCannotBeGivenID // errorNumber: 188
  case ExtractorNotFoundID // errorNumber: 189
  case PureUnitExpressionID // errorNumber: 190
  case MatchTypeLegacyPatternID // errorNumber: 191
  case UnstableInlineAccessorID // errorNumber: 192
  case VolatileOnValID // errorNumber: 193
  case ExtensionNullifiedByMemberID // errorNumber: 194

  def errorNumber = ordinal - 1

object ErrorMessageID:
  def fromErrorNumber(n: Int): Option[ErrorMessageID] =
    val enumId = n + 1
    if enumId >= 1 && enumId < ErrorMessageID.values.length then
      Some(fromOrdinal(enumId))
    else
      None

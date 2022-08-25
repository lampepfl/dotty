
import com.typesafe.tools.mima.core._

object MiMaFilters {
  val Library: Seq[ProblemFilter] = Seq(
    // APIs that must be added in 3.2.0
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleExprV2"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.runtime.QuoteUnpickler.unpickleTypeV2"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.TupleMirror"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.Tuple$package$EmptyTuple$"), // we made the empty tuple a case object
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.Scala3RunTime.nnFail"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.runtime.LazyVals.getOffsetStatic"), // Added for #14780
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.2-migration"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language.3.2"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E2$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$3$u002E2$minusmigration$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.coverage.Invoker"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.coverage.Invoker$"),

    // APIs will be added in 3.2.0
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#AppliedTypeModule.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.quoted.Quotes#reflectModule#AppliedTypeModule.apply"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.asQuotes"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.typeRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#SymbolMethods.termRef"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.quoted.Quotes#reflectModule#TypeTreeModule.ref"),
    ProblemFilters.exclude[MissingFieldProblem]("scala.runtime.stdLibPatches.language#experimental.unsafeJavaReturn"),
    ProblemFilters.exclude[MissingClassProblem]("scala.runtime.stdLibPatches.language$experimental$unsafeJavaReturn$"),

    ProblemFilters.exclude[MissingClassProblem]("scala.annotation.since"),
  )
}

package org.scalameta
package build

import com.typesafe.tools.mima.core._

// More details about Mima:
// https://github.com/typesafehub/migration-manager/wiki/sbt-plugin#basic-usage
object Mima {
  val languageAgnosticCompatibilityPolicy: ProblemFilter = (problem: Problem) => {
    val (ref, fullName) = problem match {
      case problem: TemplateProblem => (problem.ref, problem.ref.fullName)
      case problem: MemberProblem => (problem.ref, problem.ref.fullName)
    }
    val public = ref.isPublic
    val include = fullName.startsWith("scala.meta.")
    val exclude = fullName.contains(".internal.") || fullName.contains(".contrib.")
    public && include && !exclude
  }

  val scalaSpecificCompatibilityPolicy: ProblemFilter = {
    case ReversedMissingMethodProblem(member) =>
      // NOTE: `scala.meta.io.Multipath` is sealed, so by the same logic as above
      // we are free to ignore these warnings.
      member.owner.fullName != "scala.meta.io.Multipath"
    case _ =>
      true
  }

  val apiCompatibilityExceptions: Seq[ProblemFilter] = Seq(
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.apply"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.copy"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("scala.meta.Dialect.copy$default$21"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("scala.meta.Dialect.copy$default$22"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("scala.meta.Dialect.copy$default$23"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("scala.meta.Dialect.copy$default$24"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.copy$default$22"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.copy$default$23"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.copy$default$24"),
    ProblemFilters
      .exclude[DirectMissingMethodProblem]("scala.meta.Dialect.allowImplicitFunctionTypes"),
    ProblemFilters
      .exclude[DirectMissingMethodProblem]("scala.meta.Dialect.withAllowImplicitFunctionTypes"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.allowMethodTypes"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.withAllowMethodTypes"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.allowWithTypes"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.withAllowWithTypes"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.allowWhiteboxMacro"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.withAllowWhiteboxMacro"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.allowLiteralUnitType"),
    ProblemFilters
      .exclude[DirectMissingMethodProblem]("scala.meta.Dialect.withAllowLiteralUnitType"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.allowSuperTrait"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.withAllowSuperTrait"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.allowAsPatternBinding"),
    ProblemFilters
      .exclude[DirectMissingMethodProblem]("scala.meta.Dialect.withAllowAsPatternBinding"),
    ProblemFilters.exclude[DirectMissingMethodProblem]("scala.meta.Dialect.this")
  )
}

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
    // Exceptions for Scala 3. Changes to Scala 3-related AST nodes don't fall
    // under the standard Scalameta binary-compatibility policy. This is done to
    // buy time to refine the design of the Scalameta AST in preparation for the
    // Scala 3 release.
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.ExportGiven$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.ExportGiven$ExportGivenImpl"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.ExportGiven$Quasi$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.ExportGiven$Quasi"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.ExportGiven$sharedClassifier$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.ExportGiven$Quasi$sharedClassifier$"),
    ProblemFilters.exclude[MissingClassProblem](
      "scala.meta.ExportGiven$Quasi$ExportGivenQuasiImpl"
    ),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.ExportGiven")
  )
}

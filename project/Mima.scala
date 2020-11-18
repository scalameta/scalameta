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
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.testkit.DiffAssertions"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.testkit.DiffAssertions$DiffFailure"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.testkit.DiffAssertions$DiffFailure$"),
    ProblemFilters.exclude[MissingTypesProblem]("scala.meta.testkit.DiffAssertions"),
    // Dotty related exceptions - To be removed upon releasing 4.3.25 or 4.4
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Template.derives"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Template.setDerives"),
    ProblemFilters.exclude[MissingClassProblem](
      "scala.meta.tokens.Token$ColonEol$sharedClassifier$"
    ),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.tokens.Token$ColonEol"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.tokens.Token$ColonEol$"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem](
      "scala.meta.Importee#Given#ImporteeGivenImpl._importee"
    ),
    ProblemFilters.exclude[IncompatibleMethTypeProblem](
      "scala.meta.Importee#Given#ImporteeGivenImpl._importee_="
    ),
    ProblemFilters.exclude[IncompatibleResultTypeProblem](
      "scala.meta.Importee#Given#ImporteeGivenImpl.importee"
    ),
    ProblemFilters.exclude[IncompatibleMethTypeProblem](
      "scala.meta.Importee#Given#ImporteeGivenImpl.copy"
    ),
    ProblemFilters.exclude[IncompatibleResultTypeProblem](
      "scala.meta.Importee#Given#ImporteeGivenImpl.copy$default$1"
    ),
    ProblemFilters.exclude[IncompatibleMethTypeProblem](
      "scala.meta.Importee#Given#ImporteeGivenImpl.this"
    ),
    ProblemFilters.exclude[IncompatibleMethTypeProblem](
      "scala.meta.Importee#Given#Quasi#ImporteeGivenQuasiImpl.copy"
    ),
    ProblemFilters.exclude[IncompatibleResultTypeProblem](
      "scala.meta.Importee#Given#Quasi#ImporteeGivenQuasiImpl.copy$default$1"
    ),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.meta.Importee#Given.apply"),
    ProblemFilters.exclude[IncompatibleSignatureProblem]("scala.meta.Importee#Given.unapply"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("scala.meta.Importee#Given.importee"),
    ProblemFilters.exclude[IncompatibleMethTypeProblem]("scala.meta.Importee#Given.copy"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem](
      "scala.meta.Importee#Given.copy$default$1"
    ),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Importee#Given.importee"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Importee#Given.copy"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.Defn$OpaqueTypeAlias$Quasi"),
    ProblemFilters.exclude[MissingClassProblem](
      "scala.meta.Defn$OpaqueTypeAlias$Quasi$sharedClassifier$"
    ),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.Defn$OpaqueTypeAlias"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.Defn$OpaqueTypeAlias$Quasi$"),
    ProblemFilters.exclude[MissingClassProblem]("scala.meta.Defn$OpaqueTypeAlias$"),
    ProblemFilters.exclude[MissingClassProblem](
      "scala.meta.Defn$OpaqueTypeAlias$sharedClassifier$"
    ),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Defn#Type.bounds"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Defn#Type.setBounds"),
    ProblemFilters.exclude[MissingClassProblem](
      "scala.meta.Defn$OpaqueTypeAlias$DefnOpaqueTypeAliasImpl"
    ),
    ProblemFilters.exclude[MissingClassProblem](
      "scala.meta.Defn$OpaqueTypeAlias$Quasi$DefnOpaqueTypeAliasQuasiImpl"
    ),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Term#If.mods"),
    ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.Term#If.setMods")
  )
}

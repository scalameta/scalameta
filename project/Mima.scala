package org.scalameta
package build

import com.typesafe.tools.mima.core._

import scala.reflect.ClassTag

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

  @inline private def exclude[A <: ProblemRef: ClassTag](metaType: String): ProblemFilter =
    ProblemFilters.exclude[A]("scala.meta." + metaType)

  val apiCompatibilityExceptions: Seq[ProblemFilter] = Seq(
    // implicit classes
    exclude[IncompatibleResultTypeProblem]("package.XtensionDialectApply"),
    exclude[IncompatibleResultTypeProblem]("package.XtensionDialectTokenSyntax"),
    exclude[IncompatibleResultTypeProblem]("package.XtensionDialectTokensSyntax"),
    exclude[IncompatibleResultTypeProblem]("package.XtensionDialectTreeSyntax"),
    exclude[FinalClassProblem]("package$XtensionDialectApply"),
    exclude[FinalClassProblem]("package$XtensionDialectTokenSyntax"),
    exclude[FinalClassProblem]("package$XtensionDialectTokensSyntax"),
    exclude[FinalClassProblem]("package$XtensionDialectTreeSyntax")
  )
}

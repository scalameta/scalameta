package org.scalameta
package build

import com.typesafe.tools.mima.core._

import scala.reflect.ClassTag

// More details about Mima:
// https://github.com/typesafehub/migration-manager/wiki/sbt-plugin#basic-usage
object Mima {
  val languageAgnosticCompatibilityPolicy: ProblemFilter = (problem: Problem) => {
    val (ref, fullName, notScopedPrivate) = problem match {
      case problem: TemplateProblem =>
        val ref = problem.ref
        (ref, ref.fullName, ref.scopedPrivateSuff.isEmpty)
      case problem: MemberProblem =>
        val ref = problem.ref
        (ref, ref.fullName, ref.scopedPrivatePrefix.isEmpty)
    }
    val public = ref.isPublic && notScopedPrivate
    def include = fullName.startsWith("scala.meta.")
    def exclude = fullName.contains(".internal.") || fullName.contains(".contrib.")
    public && include && !exclude
  }

  val scalaSpecificCompatibilityPolicy: ProblemFilter = {
    case ReversedMissingMethodProblem(member) => // ignore sealed types
      // trees are sealed
      !member.owner.annotations.exists(_.name == "scala.meta.internal.trees.Metadata.astClass")
    case _ =>
      true
  }

  @inline private def exclude[A <: ProblemRef: ClassTag](metaType: String): ProblemFilter =
    ProblemFilters.exclude[A]("scala.meta." + metaType)

  val apiCompatibilityExceptions: Seq[ProblemFilter] = Seq(
    // newField; these methods should have been package-private
    exclude[DirectMissingMethodProblem]("Defn#Type.setBounds"),
    exclude[DirectMissingMethodProblem]("Template.setDerives"),
    exclude[DirectMissingMethodProblem]("Term#If.setMods"),
    exclude[DirectMissingMethodProblem]("Term#Match.setMods"),
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

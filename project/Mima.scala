package org.scalameta
package build

import com.typesafe.tools.mima.core._

// More details about Mima:
// https://github.com/typesafehub/migration-manager/wiki/sbt-plugin#basic-usage
object Mima {
  val languageAgnosticCompatibilityPolicy: ProblemFilter = (problem: Problem) => {
    val (ref, fullName) = problem match {
      case problem: TemplateRef => (problem.ref, problem.ref.fullName)
      case problem: MemberRef => (problem.ref, problem.ref.fullName)
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
  )
}

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
    val include = fullName.startsWith("scala.meta.") || fullName.startsWith("org.langmeta.")
    val exclude = fullName.contains(".internal.") || fullName.contains(".contrib.")
    public && include && !exclude
  }

  val scalaSpecificCompatibilityPolicy: ProblemFilter = {
    case ReversedMissingMethodProblem(member) =>
      // NOTE: `org.langmeta.semanticdb.Flags/HasFlags` are package-private,
      // so they can't be extended by Scala programs that compile/link against us.
      // Therefore, we are free to ignore these warnings according to our
      // compatibility policy.
      member.owner.fullName != "org.langmeta.semanticdb.Flags" &&
      member.owner.fullName != "org.langmeta.semanticdb.HasFlags" &&
      // NOTE: `org.langmeta.io.Multipath` is sealed, so by the same logic as above
      // we are free to ignore these warnings.
      member.owner.fullName != "org.langmeta.io.Multipath"
    case _ =>
      true
  }

  val apiCompatibilityExceptions: Seq[ProblemFilter] = Seq(
    // Symbol.Multi.{syntax,toString} previously returned `Nothing` and now it returns `String`.
    // These are binary breaking changes only for clients that statically resolved to
    // `Symbol.Multi.{toString,syntax}`, which would have unconditionally resulted in an exception.
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.langmeta.semanticdb.Symbol#Multi.toString"),
    ProblemFilters.exclude[IncompatibleResultTypeProblem]("org.langmeta.semanticdb.Symbol#Multi.syntax")
  )
}

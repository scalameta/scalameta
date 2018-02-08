package org.scalameta
package build

import com.typesafe.tools.mima.core._

// More details about Mima:
// https://github.com/typesafehub/migration-manager/wiki/sbt-plugin#basic-usage
object Mima {
  val ignoredABIProblems: Seq[ProblemFilter] = {
    Seq(
      ProblemFilters.exclude[Problem]("org.langmeta.internal.*"),
      ProblemFilters.exclude[Problem]("scala.meta.internal.*"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.parsers.Parsed.*"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.contrib.*"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("scala.meta.tokenizers.Tokenized.*")
    )
  }
}

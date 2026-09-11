package org.scalameta
package build

import scala.reflect.ClassTag

import com.typesafe.tools.mima.core._

// More details about Mima:
// https://github.com/typesafehub/migration-manager/wiki/sbt-plugin#basic-usage
object Mima {
  val languageAgnosticCompatibilityPolicy: ProblemFilter = _.matchName.exists(isPublicAndNotExcluded)

  private def isPublicAndNotExcluded(fullName: String): Boolean = {
    def exclude(parts: Iterable[String]) = parts.exists {
      case "internal" | "contrib" => true
      case _ => false
    }
    def excludeSemantic(relName: String, parts: Iterable[String]) = // semantic packages
      relName == "cli.Reporter" || relName == "cli.Reporter$" ||
        parts.headOption.exists(Set("metap", "metacp").contains) ||
        parts.lastOption.exists(Set("Metap", "Metacp").contains)

    val relName = fullName.stripPrefix("scala.meta.")
    (relName ne fullName) && ! {
      val parts = relName.split(Array('.', '#', '$'))
      exclude(parts) || excludeSemantic(relName, parts)
    }
  }

  private val treeAnnotations =
    Set("scala.meta.internal.trees.Metadata.astClass", "scala.meta.internal.trees.Metadata.branch")
  private def belongsToTree(member: MemberInfo): Boolean = // trees are sealed
    member.owner.annotations.exists(x => treeAnnotations.contains(x.name))

  val scalaSpecificCompatibilityPolicy: ProblemFilter = {
    case ReversedMissingMethodProblem(member) => // ignore sealed types
      !belongsToTree(member)
    case InheritedNewAbstractMethodProblem(absmeth, newmeth) => !belongsToTree(absmeth) &&
      !belongsToTree(newmeth)
    case _ => true
  }

  @inline
  private def exclude[A <: ProblemRef: ClassTag](metaType: String): ProblemFilter = ProblemFilters
    .exclude[A]("scala.meta." + metaType)

  val apiCompatibilityExceptions: Seq[ProblemFilter] = Seq(
    // Tree
  )
}

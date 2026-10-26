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
    // signatures in 3.3.9 and above are more exact
    exclude[IncompatibleSignatureProblem](
      "package.XtensionDialectTokenSyntax(scala.Tuple2)scala.Tuple2",
    ),
    exclude[IncompatibleSignatureProblem](
      "package.XtensionDialectTokensSyntax(scala.Tuple2)scala.Tuple2",
    ),
    exclude[IncompatibleSignatureProblem](
      "package.XtensionDialectTreeSyntax(scala.Tuple2)scala.Tuple2",
    ),
    exclude[IncompatibleSignatureProblem]("package.XtensionTreeT(scala.meta.Tree)scala.meta.Tree"),
    exclude[IncompatibleSignatureProblem]("package#XtensionDialectTokensSyntax.parse$extension(scala.Tuple2,scala.meta.parsers.Parse)scala.meta.parsers.Parsed"),
    exclude[IncompatibleSignatureProblem](
      "package#XtensionTreeT.hashCode$extension(scala.meta.Tree)Int",
    ),
    exclude[IncompatibleSignatureProblem](
      "package#XtensionTreeT.equals$extension(scala.meta.Tree,java.lang.Object)Boolean",
    ),
    exclude[IncompatibleSignatureProblem]("package#XtensionTreeT.maybeParse$extension(scala.meta.Tree,scala.meta.Dialect,scala.meta.parsers.Parse)scala.meta.parsers.Parsed"),
    // Tree
    // Show is private[meta], and mima still checks its nested classes
    exclude[Problem]("prettyprinters.Show$*"),
    exclude[Problem]("prettyprinters.Show#*"),
    // Origin: its subtypes are now sealed or final
    exclude[HierarchyNoLongerCheckedProblem]("trees.Origin$*"),
    exclude[FinalClassProblem]("trees.Origin$*"),
    // Origin: every subtype already declares or defines these
    exclude[ReversedMissingMethodProblem]("trees.Origin.begTokenIdx"),
    exclude[ReversedMissingMethodProblem]("trees.Origin.endTokenIdx"),
    // FunctionType: the extractor no longer returns the private `params`
    exclude[IncompatibleSignatureProblem]("Type#FunctionType.unapply"),
  )
}

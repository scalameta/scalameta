package org.scalameta
package build

import scala.reflect.ClassTag

import com.typesafe.tools.mima.core._

// More details about Mima:
// https://github.com/typesafehub/migration-manager/wiki/sbt-plugin#basic-usage
object Mima {
  val languageAgnosticCompatibilityPolicy: ProblemFilter = (problem: Problem) => {
    val (fullName, accessible) = problem match {
      case problem: TemplateProblem =>
        val ref = problem.ref
        (ref.fullName, ref.isPublic && ref.scopedPrivateSuff.isEmpty)
      case problem: MemberProblem =>
        val ref = problem.ref
        val accessible = ScalametaMimaUtils.isPublic(ref) &&
          (ref.fullName match {
            case "scala.meta.Dialect.this" =>
              // exclude ctor with the longest method signature (aka, primary)
              // for some reason, `private[meta]` on the primary ctor is not visible
              val descriptorLength = ref.descriptor.length
              !ref.owner.methods.get(MemberInfo.ConstructorName)
                .forall(ctor => (ctor eq ref) || ctor.descriptor.length < descriptorLength)
            case _ => true
          })
        (ref.fullName, accessible)
    }

    def exclude(parts: Seq[String]) = parts.exists {
      case "internal" | "contrib" => true
      case _ => false
    }
    def excludeSemantic(relName: String, parts: Seq[String]) = // semantic packages
      relName == "cli.Reporter" || relName == "cli.Reporter$" ||
        parts.headOption.exists(Set("metap", "metacp").contains) ||
        parts.lastOption.exists(Set("Metap", "Metacp").contains)

    accessible && {
      val relName = fullName.stripPrefix("scala.meta.")
      (relName ne fullName) && ! {
        val parts = relName.split(Array('.', '#', '$'))
        exclude(parts) || excludeSemantic(relName, parts)
      }
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
    // XXX: io split off from trees, to remove after release
    exclude[MissingClassProblem]("io.*")
    // Tree
  )
}

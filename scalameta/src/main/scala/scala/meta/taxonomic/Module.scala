package scala.meta
package taxonomic

import java.net.URI
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._

@root trait Module

trait ArtifactId
@branch trait Artifact extends Module
object Artifact {
  @leaf class Managed(id: ArtifactId) extends Artifact
  @leaf class Adhoc(classes: Multipath, sources: Multipath) extends Artifact

  def apply(id: ArtifactId): Artifact = Managed(id)
  def apply(classes: Multipath): Artifact = Adhoc(classes, Nil)
  def apply(classes: Multipath, sources: Multipath): Artifact = Adhoc(classes, sources)
}

trait ProjectId
@branch trait Project extends Module
object Project {
  @leaf class Managed(id: ProjectId) extends Project
  @leaf class Adhoc(sources: Multipath, flags: String) extends Project

  def apply(id: ProjectId): Project = Managed(id)
  def apply(sources: Multipath): Project = Adhoc(sources, "")
  def apply(sources: Multipath, flags: String): Project = Adhoc(sources, flags)
}

@leaf class Worksheet(dependencies: Module*) extends Module

@leaf class Domain(modules: Module*) extends Module
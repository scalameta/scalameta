package scala.meta
package artifacts

import java.io.File
import java.net.URI
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._

// Artifacts are taxonomic units that group together scala.meta sources (along with resources, for that matter).
// The purpose for the concept of artifacts is to serve as a building block that defines environments.
// Without artifacts, there wouldn't exist a standalone entry point to scala.meta's semantic APIs.
// NOTE: See Ecosystem.scala for an explanation of why Artifact is sealed.

@root trait Artifact
object Artifact {
  @leaf class Adhoc(sources: Seq[Source], resources: Seq[Resource] = Nil, deps: Seq[Artifact] = Nil) extends Artifact {
    override def toString = {
      if (resources.isEmpty && deps.isEmpty) s"Artifact(${sources.mkString(", ")})"
      else s"Artifact($sources, $resources, $deps)"
    }
  }

  @leaf class Unmanaged(binpath: Multipath) extends Artifact {
    override def toString = {
      def s_multipath(multipath: Multipath) = "\"" + multipath.paths.map(_.path).mkString(File.pathSeparatorChar.toString) + "\""
      s"Artifact(${s_multipath(binpath)})"
    }
  }

  @leaf class Maven(id: MavenId) extends Artifact {
    override def toString = s"Artifact($id)"
  }

  def apply(sources: Source*): Artifact = Adhoc(sources.toList, Nil, Nil)
  def apply(sources: Seq[Source], resources: Seq[Resource] = Nil, deps: Seq[Artifact] = Nil): Artifact = Adhoc(sources, resources, deps)
  def apply(binpath: Multipath): Artifact = Unmanaged(binpath)
  def apply(mavenId: MavenId): Artifact = Maven(mavenId)
}

package scala.meta
package artifacts

import java.io.File
import java.net.URI
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import scala.meta.artifacts.{Context => ArtifactContext}

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

  @leaf class Unmanaged(binpath: Multipath, sourcepath: Multipath, dialect: Dialect) extends Artifact {
    override def toString = {
      def s_multipath(multipath: Multipath) = "\"" + multipath.paths.map(_.path).mkString(File.pathSeparatorChar.toString) + "\""
      def s_dialect(dialect: Dialect) = dialect.name
      if (sourcepath.paths.isEmpty) s"Artifact(${s_multipath(binpath)})"
      else s"Artifact(${s_multipath(binpath)}, ${s_multipath(sourcepath)}, ${s_dialect(dialect)})"
    }
  }

  @leaf class Maven(id: MavenId) extends Artifact {
    override def toString = s"Artifact($id)"
  }

  def apply(sources: Source*): Artifact = Adhoc(sources.toList, Nil, Nil)
  def apply(sources: Seq[Source], resources: Seq[Resource] = Nil, deps: Seq[Artifact] = Nil): Artifact = Adhoc(sources, resources, deps)
  def apply(binpath: Multipath): Artifact = Unmanaged(binpath, "", scala.meta.dialects.Scala211) // NOTE: don't care about the dialect here
  def apply(binpath: Multipath, sourcepath: Multipath)(implicit dialect: Dialect): Artifact = Unmanaged(binpath, sourcepath, dialect)
  def apply(mavenId: MavenId): Artifact = Maven(mavenId)
}

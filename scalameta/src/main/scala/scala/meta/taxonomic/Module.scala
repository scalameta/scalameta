package scala.meta
package taxonomic

import java.io.File
import java.net.URI
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import scala.meta.taxonomic.{Context => TaxonomicContext}

// Modules are taxonomic units that group together scala.meta sources (along with resources, for that matter).
// The purpose for the concept of modules is to serve as a building block that defines environments.
// Without modules, there wouldn't exist a standalone entry point to scala.meta's semantic APIs.
// NOTE: See Taxonomy.scala for an explanation of why Module is sealed.

@root trait Module
object Module {
  @leaf class Adhoc(sources: Seq[Source], resources: Seq[Resource] = Nil, deps: Seq[Module] = Nil) extends Module {
    override def toString = {
      if (resources.isEmpty && deps.isEmpty) s"Module(${sources.mkString(", ")})"
      else s"Module($sources, $resources, $deps)"
    }
  }

  def apply(sources: Source*): Module = Adhoc(sources.toList, Nil, Nil)
  def apply(sources: Seq[Source], resources: Seq[Resource] = Nil, deps: Seq[Module] = Nil): Module = Adhoc(sources, resources, deps)
  def unapply(module: Module)(implicit c: TaxonomicContext): Some[(Seq[Source], Seq[Resource], Seq[Module])] = Some((module.sources, module.resources, module.deps))
}

// Artifacts are modules that are: 1) immutable, 2) already compiled.
// They can be either adhoc, i.e. thrown together manually from binpath and sources.
// or managed, i.e. belong to some dependency management system like Maven or Ivy,

@branch trait Artifact extends Module
object Artifact {
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

  def apply(binpath: Multipath): Artifact = Unmanaged(binpath, "", scala.meta.dialects.Scala211) // NOTE: don't care about the dialect here
  def apply(binpath: Multipath, sourcepath: Multipath)(implicit dialect: Dialect): Artifact = Unmanaged(binpath, sourcepath, dialect)
  def apply(mavenId: MavenId): Artifact = Maven(mavenId)
}

// Projects are modules that are: 1) immutable, 2) may or may not be compiled.
// They can be either adhoc, i.e. thrown together manually from a list of sources and compiler options.
// or managed, i.e. belong to some build system like Sbt or Maven,

// NOTE: For a short period of time, scalameta/scalameta used to support projects.
// However, a few days after projects were introduced, I decided to remove them, because:
// a) I don't want to support different build systems, especially given that some of them (e.g. sbt)
// don't have a simple way to be invoked programmatically, b) the notion of "maybe compiled"
// is a completely new concept that needs to be explored and modelled adequately in order not to be a gimmick.

// Worksheet are modules that are mutable, which is a scenario that is often required
// to support dynamic code evaluation, runtime code generation and the like.

// NOTE: For a short period of time, scalameta/scalameta used to support projects.
// However, they have suffered the same fate as projects did.
// The notions of: a) being standalone, i.e. stateless, and b) being mutable didn't coexist very well.
// Ultimately, I couldn't find a design that would combine the two.
// What was supposed to be expressed via worksheets is now moved to a separate flavor of Context.

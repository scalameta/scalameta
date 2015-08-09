package scala.meta
package taxonomic

import java.net.URI
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._

// Modules are taxonomic units that group together scala.meta sources (along with resources, for that matter).
// The purpose for the concept of modules is to serve as a building block that defines environments.
// Without modules, there wouldn't exist a standalone entry point to scala.meta's semantic APIs.
//
// NOTE: Until we are sure that we've figured out a contract between modules and contexts
// (i.e. what exactly does a context need from a module to initialize its internal data structures,
// e.g. a list of TASTY sections or maybe even something infrastructural like caching its content),
// modules are going to be sealed, and their handling is going to be hardcoded on per-host basis.
// An example of where I'd like modules to evolve is dialects, which are open to inheritance, because
// we know exactly what comprises a dialect.

@root trait Module
object Module {
  @leaf class Adhoc(sources: Seq[Source], resources: Seq[Resource] = Nil, dependencies: Seq[Module] = Nil) extends Module
  def apply(sources: Source*): Module = Adhoc(sources.toList, Nil, Nil)
  def apply(sources: Seq[Source], resources: Seq[Resource] = Nil, dependencies: Seq[Module] = Nil): Module = Adhoc(sources, resources, dependencies)
}

// Artifacts are modules that are: 1) immutable, 2) already compiled.
// They can be either adhoc, i.e. thrown together manually from a classpath and (optionally) a sourcepath.
// or managed, i.e. belong to some dependency management system like Maven or Ivy,

@branch trait Artifact extends Module
object Artifact {
  @leaf class Unmanaged(classpath: Multipath, sourcepath: Multipath) extends Artifact
  @leaf class Maven(id: MavenId) extends Artifact
  def apply(classes: Multipath, sources: Multipath): Artifact = Unmanaged(classes, sources)
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

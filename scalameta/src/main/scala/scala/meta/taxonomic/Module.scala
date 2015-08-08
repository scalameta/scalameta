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

// Artifacts are modules that are: 1) immutable, 2) already compiled.
// They can be either managed, i.e. belong to some dependency management system like Maven or Ivy,
// or adhoc, i.e. thrown together manually from a classpath and (optionally) a sourcepath.
//
// NOTE: As a notational convenience, we provide multiple Artifact.apply methods that let users
// avoid writing Artifact.XXX(...) or Artifact.YYY(...). This is possible arguments used to instantiate
// this or that artifact unambiguously determine the type of the artifact. Later on, when we add more types
// or artifacts, the situation may change. upd. On a second thought, I find these methods to be confusing
// and going to comment them out.

@branch trait Artifact extends Module
object Artifact {
  @leaf class Maven(id: MavenId) extends Artifact
  @leaf class Adhoc(classes: Multipath, sources: Multipath) extends Artifact

  // def apply(mavenId: MavenId): Artifact = Maven(mavenId)
  // def apply(classes: Multipath, sources: Multipath = Nil): Artifact = Adhoc(classes, sources)
}

// Projects are modules that are: 1) immutable, 2) may or may not be compiled.
// They can be either managed, i.e. belong to some build system like Sbt or Maven,
// or adhoc, i.e. thrown together manually from a list of sources and compiler options.
//
// NOTE: Unfortunately, we can't define a single Project.apply method whose overloads would be enough
// for all types of supported projects. Even if we have only adhoc and sbt projects, passing a directory
// is already going to be ambiguous (is it a directory where the sources live or is it the root directory of a build?).

@branch trait Project extends Module
object Project {
  @leaf class Sbt(name: String, root: Path) extends Project
  @leaf class Adhoc(sources: Multipath, options: String) extends Project
}

// Worksheets are modules that are mutable, which is a scenario that is often required
// to support dynamic code evaluation, runtime code generation and the like.

@leaf class Worksheet(dependencies: Module*) extends Module

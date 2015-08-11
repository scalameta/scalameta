package scala.meta
package taxonomic

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.{immutable, mutable}
import scala.meta.taxonomic.{Context => TaxonomicContext}
import org.apache.ivy.plugins.resolver._
import org.scalameta.contexts._

// NOTE: I've been thinking a lot whether I can put this class here, effectively proclaiming
// that classpath-based modules, possibly hosted via maven, are the standard.
// It is the standard now, but obviously once we have new and exciting language dialects
// or maybe additional backends, the situation may change.
//
// Over the last week, I've made a few design attempts that involved abstracting
// all kinds of details that may be deemed platform-specific. I'll mention two prominent results here.
//
// 1) Taxonomic contexts are implemented in hosts, and every host requires a taxonomic context to
// instantiate a platform-dependent semantic context. All was fine until I realized that one may
// erroneously mix taxonomic and semantic contexts from different platforms, which will lead to fail.
//
// 2) Taxonomic contexts are removed, and Module becomes a plain data structure that carries around
// everything that might be needed from it (path to binaries, sequence of sources, etc).
// With enough pressure, it is possible to even make such modules lazy, but then the data model
// becomes so bland that it's just stupid - there's no way to create different kinds of modules,
// there's no way to encapsulate anything, there's even no way to create custom platform-dependent tostrings!
//
// All in all, I was left really unsatisfied after all these attempts to abstract away something
// that's hasn't even materialized yet. Therefore, I've decided to hardcode the JVM-based reality for now
// and deal with the possible future once it actually happens.

@context(translateExceptions = true) case class Taxonomy(resolvers: DependencyResolver*) extends TaxonomicContext {
  private case class ResolvedModule(binaries: Seq[Path], sources: Seq[Source], resources: Seq[Resource], deps: Seq[Module])
  private val cache = mutable.Map[Module, ResolvedModule]()

  private def resolveUnmanaged(module: Artifact.Unmanaged): ResolvedModule = cache.getOrElseUpdate(module, {
    // TODO: we'll have to implement this for the demo
    ???
  })

  private def resolveMaven(module: Artifact.Maven): ResolvedModule = cache.getOrElseUpdate(module, {
    // NOTE: Parts of this file are originally taken from lihaoyi/ammonite:
    // https://github.com/lihaoyi/Ammonite/blob/cd5de73b5601735093f4f80a775423b7a0102b37/repl/src/main/scala/ammonite/repl/IvyThing.scala
    ???
  })

  def binaries(module: Module): Seq[Path] = module match {
    case Module.Adhoc(_, _, _) =>
      Nil
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).binaries
    case module: Artifact.Maven =>
      resolveMaven(module).binaries
  }

  def sources(module: Module): Seq[Source] = module match {
    case Module.Adhoc(sources, _, _) =>
      sources
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).sources
    case module: Artifact.Maven =>
      resolveMaven(module).sources
  }

  def resources(module: Module): Seq[Resource] = module match {
    case Module.Adhoc(_, resources, _) =>
      resources
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).resources
    case module: Artifact.Maven =>
      resolveMaven(module).resources
  }

  def deps(module: Module): Seq[Module] = module match {
    case Module.Adhoc(_, _, deps) =>
      deps
    case module: Artifact.Unmanaged =>
      resolveUnmanaged(module).deps
    case module: Artifact.Maven =>
      resolveMaven(module).deps
  }
}
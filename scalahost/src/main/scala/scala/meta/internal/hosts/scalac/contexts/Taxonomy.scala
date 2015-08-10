// Parts of this file are originally taken from lihaoyi/ammonite:
// https://github.com/lihaoyi/Ammonite/blob/cd5de73b5601735093f4f80a775423b7a0102b37/repl/src/main/scala/ammonite/repl/IvyThing.scala

package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import org.apache.ivy.plugins.resolver._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.{immutable, mutable}
import scala.meta.taxonomic
import scala.meta.taxonomic.{Context => ScalametaTaxonomicContext}

@context(translateExceptions = true)
class Taxonomy(resolvers: DependencyResolver*) extends ScalametaTaxonomicContext {
  private case class ResolvedModule(sources: Seq[Source], resources: Seq[Resource], deps: Seq[Module])
  private val cache = mutable.Map[Module, ResolvedModule]()

  private def resolveUnmanaged(module: Artifact.Unmanaged): ResolvedModule = cache.getOrElseUpdate(module, {
    // TODO: we'll have to implement this for the demo
    ???
  })

  private def resolveMaven(module: Artifact.Maven): ResolvedModule = cache.getOrElseUpdate(module, {
    // TODO: implement this along the lines of:
    // https://github.com/lihaoyi/Ammonite/blob/cd5de73b5601735093f4f80a775423b7a0102b37/repl/src/main/scala/ammonite/repl/IvyThing.scala
    ???
  })

  def sources(module: Module): Seq[Source] = module match {
    case taxonomic.Module.Adhoc(sources, _, _) =>
      sources
    case module @ taxonomic.Artifact.Unmanaged(classes, sources) =>
      resolveUnmanaged(module).sources
    case module @ taxonomic.Artifact.Maven(mavenId) =>
      resolveMaven(module).sources
  }

  def resources(module: Module): Seq[Resource] = module match {
    case taxonomic.Module.Adhoc(_, resources, _) =>
      resources
    case module @ taxonomic.Artifact.Unmanaged(classes, sources) =>
      resolveUnmanaged(module).resources
    case module @ taxonomic.Artifact.Maven(mavenId) =>
      resolveMaven(module).resources
  }

  def deps(module: Module): Seq[Module] = module match {
    case taxonomic.Module.Adhoc(_, _, deps) =>
      deps
    case module @ taxonomic.Artifact.Unmanaged(classes, sources) =>
      resolveUnmanaged(module).deps
    case module @ taxonomic.Artifact.Maven(mavenId) =>
      resolveMaven(module).deps
  }
}
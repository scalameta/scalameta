package scala.meta
package hosts.scalac

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.taxonomic.{Context => TaxonomicContext}
import scala.meta.hosts.scalac.{DefaultTaxonomy => DefaultTaxonomicContext}

final class TaxonomicMagnet private(underlying: TaxonomicContext) extends TaxonomicContext {
  private[meta] def sources(module: Module): Seq[Source] = underlying.sources(module)
  private[meta] def resources(module: Module): Seq[Resource] = underlying.resources(module)
  private[meta] def deps(module: Module): Seq[Module] = underlying.deps(module)
}

object TaxonomicMagnet {
  implicit def enabledWhenNormalContextInScope(implicit t: TaxonomicContext): TaxonomicMagnet = {
    new TaxonomicMagnet(t)
  }

  trait Gatekeeper
  object Gatekeeper {
    implicit def enabledAlways: Gatekeeper = null
    implicit def enabledWhenNormalContextInScope(implicit t: TaxonomicContext): Gatekeeper = null
  }
  implicit def enabledWhenNoNormalContextInScope(implicit h: Gatekeeper): TaxonomicMagnet = {
    new TaxonomicMagnet(DefaultTaxonomicContext)
  }
}

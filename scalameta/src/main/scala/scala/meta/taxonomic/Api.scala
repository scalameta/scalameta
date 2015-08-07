package scala.meta
package taxonomic

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.meta.taxonomic.{Context => TaxonomicContext}

private[meta] trait Api {
  type Domain = scala.meta.taxonomic.Domain
  // val Domain = scala.meta.taxonomic.Domain

  type Module = scala.meta.taxonomic.Module
  // val Module = scala.meta.taxonomic.Module

  type Resource = scala.meta.taxonomic.Resource
  // val Resource = scala.meta.taxonomic.Resource

  @hosted def domain: Domain = implicitly[TaxonomicContext].domain

  @hosted def sources: Seq[Source] = implicitly[TaxonomicContext].domain.sources

  @hosted def resources: Seq[Resource] = implicitly[TaxonomicContext].domain.resources
}

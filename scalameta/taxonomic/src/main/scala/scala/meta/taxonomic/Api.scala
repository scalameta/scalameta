package scala.meta
package taxonomic

import java.io._
import org.scalameta.adt._
import org.scalameta.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.taxonomic.{Context => TaxonomicContext}

private[meta] trait Api extends MavenDsl {
  implicit class XtensionTaxonomicDomain(domain: Domain) {
    @hosted def sources: Seq[Source] = domain.artifacts.flatMap(_.sources).toList
    @hosted def resources: Seq[Resource] = domain.artifacts.flatMap(_.resources).toList
  }

  implicit class XtensionTaxonomicArtifact(artifact: Artifact) {
    @hosted def binaries: Seq[Path] = implicitly[TaxonomicContext].binaries(artifact)
    @hosted def sources: Seq[Source] = implicitly[TaxonomicContext].sources(artifact)
    @hosted def resources: Seq[Resource] = implicitly[TaxonomicContext].resources(artifact)
    @hosted def deps: Seq[Artifact] = implicitly[TaxonomicContext].deps(artifact)
  }
}

private[meta] trait Aliases {
  type TaxonomicContext = scala.meta.taxonomic.Context
  val TaxonomicContext = scala.meta.taxonomic.Context

  type TaxonomicException = scala.meta.taxonomic.TaxonomicException
  val TaxonomicException = scala.meta.taxonomic.TaxonomicException

  type Taxonomy = scala.meta.taxonomic.Taxonomy
  val Taxonomy = scala.meta.taxonomic.Taxonomy

  type Domain = scala.meta.taxonomic.Domain
  val Domain = scala.meta.taxonomic.Domain


  type Artifact = scala.meta.taxonomic.Artifact
  val Artifact = scala.meta.taxonomic.Artifact

  type CrossVersion = scala.meta.taxonomic.CrossVersion
  val CrossVersion = scala.meta.taxonomic.CrossVersion

  type Resource = scala.meta.taxonomic.Resource
  val Resource = scala.meta.taxonomic.Resource
}

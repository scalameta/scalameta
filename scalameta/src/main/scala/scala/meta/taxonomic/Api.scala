package scala.meta
package taxonomic

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.meta.taxonomic.{Context => TaxonomicContext}

private[meta] trait Api extends MavenIdDsl {
  type Module = scala.meta.taxonomic.Module
  // val Module = scala.meta.taxonomic.Module

  type Artifact = scala.meta.taxonomic.Artifact
  val Artifact = scala.meta.taxonomic.Artifact

  type CrossVersion = scala.meta.taxonomic.CrossVersion
  val CrossVersion = scala.meta.taxonomic.CrossVersion

  type Project = scala.meta.taxonomic.Project
  val Project = scala.meta.taxonomic.Project

  // type SbtProject = scala.meta.taxonomic.SbtProject
  val SbtProject = scala.meta.taxonomic.SbtProject

  type Worksheet = scala.meta.taxonomic.Worksheet
  val Worksheet = scala.meta.taxonomic.Worksheet

  type Domain = scala.meta.taxonomic.Domain
  val Domain = scala.meta.taxonomic.Domain

  type Resource = scala.meta.taxonomic.Resource
  // val Resource = scala.meta.taxonomic.Resource

  @hosted def domain: Domain = implicitly[TaxonomicContext].domain

  implicit class XtensionTaxonomicModule(module: Module) {
    @hosted def sources: Seq[Source] = implicitly[TaxonomicContext].sources(module)
    @hosted def resources: Seq[Resource] = implicitly[TaxonomicContext].resources(module)
    @hosted def dependencies: Seq[Module] = implicitly[TaxonomicContext].dependencies(module)
  }
}

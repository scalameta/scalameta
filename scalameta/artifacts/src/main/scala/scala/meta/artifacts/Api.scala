package scala.meta
package artifacts

import java.io._
import org.scalameta.adt._
import org.scalameta.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.artifacts.{Context => ArtifactContext}

private[meta] trait Api extends MavenDsl {
  implicit class XtensionDomain(domain: Domain) {
    @hosted def sources: Seq[Source] = domain.artifacts.flatMap(_.sources).toList
    @hosted def resources: Seq[Resource] = domain.artifacts.flatMap(_.resources).toList
  }

  implicit class XtensionArtifact(artifact: Artifact) {
    @hosted def binaries: Seq[Path] = implicitly[ArtifactContext].binaries(artifact)
    @hosted def sources: Seq[Source] = implicitly[ArtifactContext].sources(artifact)
    @hosted def resources: Seq[Resource] = implicitly[ArtifactContext].resources(artifact)
    @hosted def deps: Seq[Artifact] = implicitly[ArtifactContext].deps(artifact)
  }
}

private[meta] trait Aliases {
  type ArtifactContext = scala.meta.artifacts.Context
  val ArtifactContext = scala.meta.artifacts.Context

  type ArtifactException = scala.meta.artifacts.ArtifactException
  val ArtifactException = scala.meta.artifacts.ArtifactException

  type Ecosystem = scala.meta.artifacts.Ecosystem
  val Ecosystem = scala.meta.artifacts.Ecosystem

  type Domain = scala.meta.artifacts.Domain
  val Domain = scala.meta.artifacts.Domain

  type Artifact = scala.meta.artifacts.Artifact
  val Artifact = scala.meta.artifacts.Artifact

  type CrossVersion = scala.meta.artifacts.CrossVersion
  val CrossVersion = scala.meta.artifacts.CrossVersion

  type Resource = scala.meta.artifacts.Resource
  val Resource = scala.meta.artifacts.Resource
}

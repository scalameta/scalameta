package scala.meta
package artifacts

import java.io._
import org.scalameta.adt._
import org.scalameta.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq

private[meta] trait Api extends MavenDsl {
  implicit class XtensionDomain(domain: Domain) {
    def sources(implicit r: Resolver): Seq[Source] = domain.artifacts.flatMap(_.sources).toList
    def resources(implicit r: Resolver): Seq[Resource] = domain.artifacts.flatMap(_.resources).toList
  }

  implicit class XtensionArtifact(artifact: Artifact) {
    def binaries(implicit r: Resolver): Seq[Path] = r.binaries(artifact)
    def sources(implicit r: Resolver): Seq[Source] = r.sources(artifact)
    def resources(implicit r: Resolver): Seq[Resource] = r.resources(artifact)
    def deps(implicit r: Resolver): Seq[Artifact] = r.deps(artifact)
  }
}

private[meta] trait Aliases {
  type ArtifactException = scala.meta.artifacts.ArtifactException
  val ArtifactException = scala.meta.artifacts.ArtifactException

  type Resolver = scala.meta.artifacts.Resolver
  val Resolver = scala.meta.artifacts.Resolver

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

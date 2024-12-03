package scala.meta.tests.metacp

import scala.meta.AbsolutePath

import java.io.OutputStreamWriter
import java.io.PrintStream

import scala.concurrent.ExecutionContext.Implicits.global

import coursier._

case class ModuleID(organization: String, name: String, version: String) {
  def toCoursier: Dependency =
    Dependency(Module(Organization(organization), ModuleName(name)), version)
  override def toString: String = s"$organization:$name:$version"
}
object ModuleID {
  def scalaReflect(scalaVersion: String): ModuleID =
    ModuleID("org.scala-lang", "scala-reflect", scalaVersion)
  def fromString(string: String): List[ModuleID] = string.split(";").iterator.flatMap { moduleId =>
    moduleId.split(":") match {
      case Array(org, name, rev) => ModuleID(org, name, rev) :: Nil
      case _ => Nil
    }
  }.toList
}
object Jars {
  def fetch(
      org: String,
      artifact: String,
      version: String,
      out: PrintStream = System.out,
      // If true, fetches the -sources.jar files instead of regular jar with classfiles.
      fetchSourceJars: Boolean = false,
      provided: List[ModuleID] = Nil
  ): List[AbsolutePath] = fetch(ModuleID(org, artifact, version) :: provided, out, fetchSourceJars)

  def fetch(
      modules: Iterable[ModuleID],
      out: PrintStream,
      fetchSourceJars: Boolean
  ): List[AbsolutePath] = Fetch().addDependencies(modules.map(_.toCoursier).toList: _*)
    .addClassifiers((if (fetchSourceJars) Classifier.sources :: Nil else Nil): _*)
    .addRepositories(MavenRepository("https://scala-ci.typesafe.com/artifactory/scala-integration/"))
    .run().map(AbsolutePath(_)).toList
}

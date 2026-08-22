package org.scalameta
package build

import sbt.Keys._
import sbt._

import scala.scalanative.build.Mode
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.linker.interface.StandardConfig
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import com.jsuereth.sbtpgp.PgpKeys
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._

import coursier.ShadingPlugin
import coursier.ShadingPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtcrossproject.CrossProject
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport._

/**
 * Settings a project gets for the platform it builds for, and the combinators that hand them out.
 *
 * These live here rather than in build.sbt because an implicit class declared in build.sbt is not
 * applied to the other statements in build.sbt.
 */
object Extensions {

  import Versions._

  def isScalaBinaryVersion(version: String) = Def.setting(scalaBinaryVersion.value == version)
  lazy val isScala213 = isScalaBinaryVersion("2.13")
  lazy val isScala3 = isScalaBinaryVersion("3")
  def isScala213or3 = Def.setting(isScala213.value || isScala3.value)

  /**
   * Which row a setting is being evaluated in. Only the source roots of a merged module need to
   * name it; everything else gets its platform's settings handed to it directly.
   */
  val platformAxis = settingKey[Platforms.Platform]("platform this project builds for")

  val commonJsSettings = Seq(
    platformAxis := Platforms.JS,
    crossScalaVersions := crossScalaVersions.value.flatMap(v =>
      CrossVersion.binaryScalaVersion(v) match {
        case "2.12" => Some(LatestScala212)
        case "2.13" => Some(LatestScala213ForJS)
        case "3" => Some(v)
        case _ => None
      },
    ).distinct,
    scalaVersion := LatestScala213ForJS,
    bspEnabled := false,
    scalaJSLinkerConfig := StandardConfig().withBatchMode(true),
    scalacOptions ++= {
      // scala3 specifically will invoke scala3TreeLiftsCodeGen which is a JVM project
      if (isSnapshot.value || !isPlatform(Platforms.JS).value) Seq.empty
      else {
        val localDir = (ThisBuild / baseDirectory).value.toURI.toString
        val githubDir = "https://raw.githubusercontent.com/scalameta/scalameta"
        val prefix = if (isScala3.value) "-scalajs-mapSourceURI" else "-P:scalajs:mapSourceURI"
        Seq(s"$prefix:$localDir->$githubDir/v${version.value}/")
      }
    },
  )

  lazy val nativeSettings = Seq(
    platformAxis := Platforms.Native,
    bspEnabled := false,
    nativeConfig ~= {
      _.withMode(scalanative.build.Mode.releaseFast)
      /*
        .withServiceProviders(Map(
          "scala.meta.tokenizers.Tokenize" ->
            Seq("scala.meta.internal.tokenizers.ScalametaTokenizer$AsTokenize$")
        ))
       */
    },
  )

  /** What a JVM row gets whether or not it is cross-built; a JVM-only project takes it by hand. */
  lazy val jvmPlatformSettings = Def.settings(
    platformAxis := Platforms.JVM,
    // Target Java 8 bytecode for Scala 2 JVM artifacts regardless of the build
    // JDK, so releases built on newer JDKs still run on JDK 8. Scala 3 (3.8+) is
    // built with JDK 17 and needs no -release flag.
    scalacOptions ++= { if (isScala3.value) Nil else Seq("-release", "8") },
  )

  def isPlatform(platform: Platforms.Platform) = Def.setting(platformAxis.value == platform)

  lazy val adhocRepoUri = sys.props("scalameta.repository.uri")
  lazy val adhocRepoCredentials = sys.props("scalameta.repository.credentials")
  lazy val isCustomRepository = adhocRepoUri != null && adhocRepoCredentials != null
  lazy val publishableSettings = Def.settings(
    credentials ++= {
      val credentialsFile =
        if (adhocRepoCredentials != null) new File(adhocRepoCredentials) else null
      if (credentialsFile != null) List(new FileCredentials(credentialsFile)) else Nil
    },
    Compile / publishArtifact := true,
    Test / publishArtifact := false,
    publishMavenStyle := true,
    pomIncludeRepository := { x => false },
    versionScheme := Some("semver-spec"),
    mimaPreviousArtifacts := {
      if (organization.value == "org.scalameta" && isPlatform(Platforms.JVM).value) {
        val rxVersion = """^(\d+)\.(\d+)\.(\d+)(.+)?$""".r
        val previousVersion = version.value match {
          case rxVersion(major, "0", "0", suffix) if suffix != null =>
            if (suffix.startsWith("-M")) None else Some(s"$major.0.0")
          case rxVersion(major, minor, patch, suffix) if suffix != null =>
            Some(s"$major.$minor.$patch")
          case rxVersion(major, "0", "0", null) => Some(s"$major.0.0")
          case rxVersion(major, minor, "0", null) => Some(s"$major.${minor.toInt - 1}.0")
          case rxVersion(major, minor, patch, null) => Some(s"$major.$minor.0")
          case _ => sys.error(s"Invalid version number: ${version.value}")
        }
        previousVersion.map(organization.value % moduleName.value % _ cross crossVersion.value)
          .toSet
      } else Set()
    },
    mimaBinaryIssueFilters += Mima.languageAgnosticCompatibilityPolicy,
    mimaBinaryIssueFilters += Mima.scalaSpecificCompatibilityPolicy,
    mimaBinaryIssueFilters ++= Mima.apiCompatibilityExceptions,
    licenses += "BSD" -> url("https://github.com/scalameta/scalameta/blob/main/LICENSE.md"),
    pomExtra :=
      <url>https://github.com/scalameta/scalameta</url>
      <inceptionYear>2014</inceptionYear>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalameta/scalameta/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>xeno-by</id>
          <name>Eugene Burmako</name>
          <url>http://xeno.by</url>
        </developer>
        <developer>
          <id>DavidDudson</id>
          <name>David Dudson</name>
          <url>https://daviddudson.github.io/</url>
        </developer>
        <developer>
          <id>olafurpg</id>
          <name>Ólafur Páll Geirsson</name>
          <url>https://geirsson.com/</url>
        </developer>
        <developer>
          <id>kpbochenek</id>
          <name>Krzysztof Bochenek</name>
          <url>https://github.com/kpbochenek</url>
        </developer>
        <developer>
          <id>mutcianm</id>
          <name>Mikhail Mutcianko</name>
          <url>https://github.com/mutcianm</url>
        </developer>
        <developer>
          <id>maxov</id>
          <name>Max Ovsiankin</name>
          <url>https://github.com/maxov</url>
        </developer>
        <developer>
          <id>gabro</id>
          <name>Gabriele Petronella</name>
          <url>http://buildo.io</url>
        </developer>
        <developer>
          <id>densh</id>
          <name>Denys Shabalin</name>
          <url>http://den.sh</url>
        </developer>
      </developers>,
  )

  lazy val nonPublishableSettings = Seq(
    publish / skip := true,
    mimaPreviousArtifacts := Set.empty,
    //  mimaPreviousClassfiles := Map.empty,
    Compile / packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq.empty,
    publishArtifact := false,
    PgpKeys.publishSigned := {},
    publish := {},
  )

  lazy val shadingSettings = Def.settings(
    shadedDependencies ++= ShadedDependency.all.map(x =>
      if (x.isPlatformSpecific) x.groupID %%% x.artifactID % "foo"
      else x.groupID %% x.artifactID % "foo",
    ).toSet,
    shadingRules ++=
      ShadedDependency.all.map(x => ShadingRule.moveUnder(x.namespace, "scala.meta.shaded.internal")),
    validNamespaces ++= Set("org", "scala", "java"),
  )

  def platformPublishSettings(platform: sbtcrossproject.Platform) =
    if (Platforms.shouldBuildPlatform(Platforms(platform.identifier))) publishableSettings
    else nonPublishableSettings
  def crossPlatformPublishSettings(project: sbtcrossproject.CrossProject) = project.projects.keys
    .foldLeft(project) { case (res, platform) =>
      val settings = platformPublishSettings(platform)
      if (settings.isEmpty) res else res.configurePlatform(platform)(_.settings(settings))
    }
  val publishJVMSettings = platformPublishSettings(JVMPlatform)

  implicit class CrossProjectExtensions(private val self: CrossProject) extends AnyVal {

    def crossJvm(ss: Def.SettingsDefinition*): CrossProject = self
      .jvmSettings((jvmPlatformSettings: Def.SettingsDefinition) +: ss: _*)

    def crossJs(ss: Def.SettingsDefinition*): CrossProject = self
      .jsSettings((commonJsSettings: Def.SettingsDefinition) +: ss: _*)

    def crossNative(ss: Def.SettingsDefinition*): CrossProject = self
      .nativeSettings((nativeSettings: Def.SettingsDefinition) +: ss: _*)

    /** Every row gets the settings for the platform it is. */
    def crossAll: CrossProject = self.crossJvm().crossJs().crossNative()

    /** Per row, publishable or not, as SCALAMETA_PLATFORM selects. */
    def published: CrossProject = self.jvmSettings(publishJVMSettings)
      .jsSettings(platformPublishSettings(JSPlatform))
      .nativeSettings(platformPublishSettings(NativePlatform))

    def shaded: CrossProject =
      if (shadingSettings.isEmpty) self
      else self.enablePlugins(ShadingPlugin).settings(shadingSettings)

  }

}

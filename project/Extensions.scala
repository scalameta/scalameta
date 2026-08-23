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

  val commonJsSettings = Def.settings(
    platformAxis := Platforms.JS,
    crossScalaVersions := jsScalaVersions(crossScalaVersions.value),
    scalaVersion := PublishedScala213ForJS,
    bspEnabled := false,
    scalaJSLinkerConfig := StandardConfig().withBatchMode(true),
    scalacOptions ++= {
      if (isSnapshot.value) Seq.empty
      else {
        val localDir = (ThisBuild / baseDirectory).value.toURI.toString
        val githubDir = "https://raw.githubusercontent.com/scalameta/scalameta"
        val prefix = if (isScala3.value) "-scalajs-mapSourceURI" else "-P:scalajs:mapSourceURI"
        Seq(s"$prefix:$localDir->$githubDir/v${version.value}/")
      }
    },
  )

  lazy val nativeSettings = Def.settings(
    platformAxis := Platforms.Native,
    crossScalaVersions := nativeScalaVersions(crossScalaVersions.value),
    scalaVersion := PublishedScala213ForNative,
    bspEnabled := false,
    nativeConfig ~= {
      _.withMode(Mode.releaseFast)
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

  lazy val adhocRepoUri = sys.props("scalameta.repository.uri")
  lazy val adhocRepoCredentials = sys.props("scalameta.repository.credentials")
  lazy val isCustomRepository = adhocRepoUri != null && adhocRepoCredentials != null

  /** The artifact MiMa compares against: the release this module published before. */
  private def getMimaPreviousArtifacts() = Def.setting {
    if (organization.value == "org.scalameta") {
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
      previousVersion.map(v => (organization.value % moduleName.value % v).cross(crossVersion.value))
        .toSet
    } else Set.empty[ModuleID]
  }

  lazy val jvmMimaSettings = Def.settings(mimaPreviousArtifacts := getMimaPreviousArtifacts().value)

  lazy val publishableSettings = Def.settings(
    mimaPreviousArtifacts := Set.empty,
    credentials ++= {
      val credentialsFile =
        if (adhocRepoCredentials != null) new File(adhocRepoCredentials) else null
      if (credentialsFile != null) List(Credentials(credentialsFile)) else Nil
    },
    Compile / publishArtifact := true,
    Test / publishArtifact := false,
    publishMavenStyle := true,
    pomIncludeRepository := { x => false },
    versionScheme := Some("semver-spec"),
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

  /** A published JVM row, cross-built or not. */
  lazy val publishJvmSettings =
    if (Platforms.shouldBuildPlatform(Platforms.JVM)) Def
      .settings(publishableSettings, jvmMimaSettings)
    else nonPublishableSettings

  /**
   * Which patch a Scala.js build uses, given the list a JVM build uses. PublishedScala213ForJS
   * gives the reason.
   */
  def jsScalaVersions(versions: Seq[String]): Seq[String] = versions.flatMap(v =>
    CrossVersion.binaryScalaVersion(v) match {
      case "2.12" => Some(PublishedScala212ForJS)
      case "2.13" => Some(PublishedScala213ForJS)
      case "3" => Some(v)
      case _ => None
    },
  ).distinct

  /** The same for a Scala Native build. See PublishedScala213ForNative. */
  def nativeScalaVersions(versions: Seq[String]): Seq[String] = versions.map(v =>
    CrossVersion.binaryScalaVersion(v) match {
      case "2.12" => PublishedScala212ForNative
      case "2.13" => PublishedScala213ForNative
      case _ => v
    },
  ).distinct

  def platformPublishSettings(platform: Platforms.Platform) =
    if (Platforms.shouldBuildPlatform(platform)) publishableSettings else nonPublishableSettings

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
    def published: CrossProject = self.jvmSettings(publishJvmSettings)
      .jsSettings(platformPublishSettings(Platforms.JS))
      .nativeSettings(platformPublishSettings(Platforms.Native))

    def shaded: CrossProject =
      if (shadingSettings.isEmpty) self
      else self.enablePlugins(ShadingPlugin).settings(shadingSettings)

  }

}

package org.scalameta
package build

import sbt.Keys._
import sbt._

import scala.scalanative.build.Mode
import scala.scalanative.sbtplugin.ScalaNativeCrossVersion
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport._

import org.scalajs.linker.interface.StandardConfig
import org.scalajs.sbtplugin.ScalaJSCrossVersion
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import com.jsuereth.sbtpgp.PgpKeys
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport._

import coursier.ShadingPlugin
import coursier.ShadingPlugin.autoImport._

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

  lazy val nativeSettings = Seq(
    platformAxis := Platforms.Native,
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

  /** MiMa has predecessors to compare against only on a JVM row that is published. */
  lazy val jvmMimaSettings = Def.settings {
    mimaPreviousArtifacts := {
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
        previousVersion.map(organization.value % moduleName.value % _ cross crossVersion.value)
          .toSet
      } else Set()
    }
  }

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
    licenses += License("BSD", url("https://github.com/scalameta/scalameta/blob/main/LICENSE.md")),
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
    // the platform comes from scalaModuleInfo (sbt/sbt#9621), so a CrossVersion
    // naming it here would name it twice
    shadedDependencies ++=
      ShadedDependency.all.map(x => (x.groupID % x.artifactID % "foo").cross(CrossVersion.binary))
        .toSet,
    shadingRules ++=
      ShadedDependency.all.map(x => ShadingRule.moveUnder(x.namespace, "scala.meta.shaded.internal")),
    validNamespaces ++= Set("org", "scala", "java"),
  )

  /** A published JVM row, cross-built or not. */
  /*
   * projectMatrix matches a dependency row by its exact Scala version, so a library needs a row for
   * every version anything is built at -- for scalameta, every version the tests run at. Publishing
   * stays restricted to the versions published before the conversion; otherwise two rows of one
   * binary version would compete for the same artifact name.
   */
  private def onlyPublishFor(versions: Seq[String]) = Def.settings(
    publish / skip := !versions.contains(scalaVersion.value),
    publishArtifact := versions.contains(scalaVersion.value),
    mimaPreviousArtifacts :=
      { if (versions.contains(scalaVersion.value)) mimaPreviousArtifacts.value else Set.empty },
  )

  private def publishFor(platform: Platforms.Platform, versions: Seq[String]): Seq[Setting[_]] =
    if (!Platforms.shouldBuildPlatform(platform)) nonPublishableSettings
    else {
      val base =
        if (platform == Platforms.JVM) Def.settings(publishableSettings, jvmMimaSettings)
        // MiMa compares only JVM artifacts, and on sbt 2 it fails rather than skips when
        // mimaPreviousArtifacts has no value at all
        else Def.settings(publishableSettings, mimaPreviousArtifacts := Set.empty)
      base ++ onlyPublishFor(versions)
    }

  def publishJvmFor(versions: Seq[String]) = publishFor(Platforms.JVM, versions)
  def publishJsFor(versions: Seq[String]) = publishFor(Platforms.JS, jsScalaVersions(versions))
  def publishNativeFor(versions: Seq[String]) = publishFor(Platforms.Native, versions)

  /** A JVM-only project, which has no rows to select between. */
  lazy val publishJvmSettings =
    if (Platforms.shouldBuildPlatform(Platforms.JVM)) Def
      .settings(publishableSettings, jvmMimaSettings)
    else nonPublishableSettings

  /**
   * The Scala versions a Scala.js row is built for. Scala.js artifacts exist only for the newest
   * patch of each binary version, so a JS row cannot simply reuse the JVM list. This was a setting
   * that rewrote crossScalaVersions; a matrix row cannot rewrite its own axis, so it is applied
   * when the row is built.
   */
  def jsScalaVersions(versions: Seq[String]): Seq[String] = versions.flatMap(v =>
    CrossVersion.binaryScalaVersion(v) match {
      case "2.12" => Some(LatestScala212)
      case "2.13" => Some(LatestScala213ForJS)
      case "3" => Some(v)
      case _ => None
    },
  ).distinct

  /**
   * A matrix has one base directory, so each row has to name every tree it reads. Directories that
   * do not exist are harmless.
   */
  private def roots(base: File, dirs: String*): Seq[Setting[_]] = {
    def under(conf: String, leaf: String => Seq[String]) = Def.setting {
      // a matrix base may be relative, and a relative source root resolves against the wrong one
      val root = IO.resolve((ThisBuild / baseDirectory).value, base)
      for (dir <- dirs.toList; name <- leaf(scalaBinaryVersion.value)) yield root / dir / "src" /
        conf / name
    }
    def sources(sbv: String) = Seq("scala", "java", s"scala-$sbv", s"scala-${sbv.head}").distinct
    Def.settings(
      Compile / unmanagedSourceDirectories ++= under("main", sources).value,
      Test / unmanagedSourceDirectories ++= under("test", sources).value,
      Compile / unmanagedResourceDirectories ++= under("main", _ => Seq("resources")).value,
      Test / unmanagedResourceDirectories ++= under("test", _ => Seq("resources")).value,
    )
  }

  private def rowSettings(
      dir: String,
      platform: Seq[Setting[_]],
      base: File,
      extra: Seq[Def.SettingsDefinition],
  ): Seq[Setting[_]] = platform ++ roots(base, "shared", dir) ++ extra.flatMap(_.settings)

  /**
   * Which row gets the unsuffixed id. sbt 1's projectmatrix left JVM/2.13 bare; sbt 2 leaves
   * JVM/Scala 3 bare, which would rename every id CI and the aliases refer to.
   */
  private def bareAxes: Seq[VirtualAxis] =
    Seq(VirtualAxis.jvm, VirtualAxis.scalaABIVersion(LatestScala213))

  implicit class ProjectMatrixExtensions(private val self: ProjectMatrix) extends AnyVal {

    def crossJvm(versions: Seq[String], ss: Def.SettingsDefinition*): ProjectMatrix = self
      .defaultAxes(bareAxes: _*)
      .jvmPlatform(versions, rowSettings("jvm", jvmPlatformSettings, self.base, ss))

    def crossJs(versions: Seq[String], ss: Def.SettingsDefinition*): ProjectMatrix = self
      .defaultAxes(bareAxes: _*)
      .jsPlatform(jsScalaVersions(versions), rowSettings("js", commonJsSettings, self.base, ss))

    def crossNative(versions: Seq[String], ss: Def.SettingsDefinition*): ProjectMatrix = self
      .defaultAxes(bareAxes: _*)
      .nativePlatform(versions, rowSettings("native", nativeSettings, self.base, ss))

    /** Every platform, nothing published. */
    def crossAll(versions: Seq[String]): ProjectMatrix = self.crossJvm(versions).crossJs(versions)
      .crossNative(versions)

    /** Every platform, publishing every row it builds. */
    def crossAllPublished(versions: Seq[String]): ProjectMatrix = self
      .crossAllPublished(versions, versions)

    /** Every platform, each row publishable or not as SCALAMETA_PLATFORM selects. */
    def crossAllPublished(versions: Seq[String], publish: Seq[String]): ProjectMatrix = self
      .crossJvm(versions, publishJvmFor(publish)).crossJs(versions, publishJsFor(publish))
      .crossNative(versions, publishNativeFor(publish))

    def shaded: ProjectMatrix =
      if (shadingSettings.isEmpty) self
      else self.enablePlugins(ShadingPlugin).settings(shadingSettings)

  }

}

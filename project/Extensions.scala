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
  lazy val isScala212 = isScalaBinaryVersion("2.12")
  lazy val isScala213 = isScalaBinaryVersion("2.13")
  lazy val isScala3 = isScalaBinaryVersion("3")

  /**
   * Which row a setting is being evaluated in. Only the source roots of a merged module need to
   * name it; everything else gets its platform's settings handed to it directly.
   */
  val platformAxis = settingKey[Platforms.Platform]("platform this project builds for")

  val commonJsSettings = Def.settings(
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

  lazy val nativeSettings = Def.settings(
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
    licenses += License("BSD", uri("https://github.com/scalameta/scalameta/blob/main/LICENSE.md")),
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
    // nothing to compare, and the default task would compile the row to find that out
    mimaReportBinaryIssues := {},
    Compile / packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq.empty,
    publishArtifact := false,
    PgpKeys.publishSigned := {},
    publish := {},
  )

  lazy val shadingSettings = Def.settings(
    /* sbt 2 takes the platform from scalaModuleInfo (sbt/sbt#9621), so a CrossVersion that named
     * the platform here would name it twice */
    shadedDependencies ++=
      ShadedDependency.all.map(x => (x.groupID % x.artifactID % "foo").cross(CrossVersion.binary))
        .toSet,
    shadingRules ++=
      ShadedDependency.all.map(x => ShadingRule.moveUnder(x.namespace, "scala.meta.shaded.internal")),
    validNamespaces ++= Set("org", "scala", "java"),
  )

  /* Two rows of one binary version would write the same artifact name, so only the versions this
   * build published before the conversion keep the publishing settings. */
  private def publishFor(platform: Platforms.Platform, versions: Seq[String]): Seq[Setting[?]] =
    if (!Platforms.shouldBuildPlatform(platform)) nonPublishableSettings
    else {
      val keep = Def.setting(versions.contains(scalaVersion.value))
      Def.settings(
        publish / skip := !keep.value,
        publishArtifact := keep.value,
        publishableSettings,
        mimaPreviousArtifacts := {
          if (platform == Platforms.JVM && keep.value) getMimaPreviousArtifacts().value
          else Set.empty
        },
      )
    }

  def publishJvmFor(versions: Seq[String]) = publishFor(Platforms.JVM, versions)
  def publishJsFor(versions: Seq[String]) = publishFor(Platforms.JS, jsScalaVersions(versions))
  def publishNativeFor(versions: Seq[String]) =
    publishFor(Platforms.Native, nativeScalaVersions(versions))

  /** A published JVM row, cross-built or not. */
  lazy val publishJvmSettings =
    if (Platforms.shouldBuildPlatform(Platforms.JVM)) Def
      .settings(publishableSettings, jvmMimaSettings)
    else nonPublishableSettings

  /**
   * Maps the list a JVM row uses to the patch a Scala.js row builds at, see PublishedScala213ForJS.
   * A row cannot rewrite its own axis, so the build maps it at creation.
   */
  def jsScalaVersions(versions: Seq[String]): Seq[String] = versions.flatMap(v =>
    CrossVersion.binaryScalaVersion(v) match {
      case "2.12" => Some(PublishedScala212ForJS)
      case "2.13" => Some(PublishedScala213ForJS)
      case "3" => Some(v)
      case _ => None
    },
  ).distinct

  /** Maps the same list to the patch a Native row builds at. See PublishedScala213ForNative. */
  def nativeScalaVersions(versions: Seq[String]): Seq[String] = versions.map(v =>
    CrossVersion.binaryScalaVersion(v) match {
      case "2.12" => PublishedScala212ForNative
      case "2.13" => PublishedScala213ForNative
      case _ => v
    },
  ).distinct

  implicit class ProjectMatrixExtensions(private val self: ProjectMatrix) extends AnyVal {

    def crossJvm(versions: Seq[String], ss: Def.SettingsDefinition*): ProjectMatrix = {
      val (scala2, scala3) = splitScala3(versions)
      val settings = rowSettings("jvm", jvmPlatformSettings, ss)
      val proj = self.defaultAxes(bareAxes *).jvmPlatform(scala2, settings)
      scala3.foldLeft(proj) { case (m, (v, axis, ss)) => m.jvmPlatform(v, axis, settings ++ ss) }
    }

    def crossJs(versions: Seq[String], ss: Def.SettingsDefinition*): ProjectMatrix = {
      val (scala2, scala3) = splitScala3(versions)
      val settings = rowSettings("js", commonJsSettings, ss)
      val proj = self.defaultAxes(bareAxes *).jsPlatform(jsScalaVersions(scala2), settings)
      scala3.foldLeft(proj) { case (m, (v, axis, ss)) => m.jsPlatform(v, axis, settings ++ ss) }
    }

    def crossNative(versions: Seq[String], ss: Def.SettingsDefinition*): ProjectMatrix = {
      val (scala2, scala3) = splitScala3(versions)
      val settings = rowSettings("native", nativeSettings, ss)
      val proj = self.defaultAxes(bareAxes *).nativePlatform(nativeScalaVersions(scala2), settings)
      scala3.foldLeft(proj) { case (m, (v, axis, ss)) => m.nativePlatform(v, axis, settings ++ ss) }
    }

    /** Every platform, nothing published. */
    def crossAll(versions: Seq[String]): ProjectMatrix = self.crossJvm(versions).crossJs(versions)
      .crossNative(versions)

    /** Every platform, publishing every row it builds. */
    def crossAllPublished(versions: Seq[String]): ProjectMatrix = self
      .crossAllPublished(versions, versions)

    /** Every platform. SCALAMETA_PLATFORM decides which rows publish. */
    def crossAllPublished(versions: Seq[String], publish: Seq[String]): ProjectMatrix = self
      .crossJvm(versions, publishJvmFor(publish)).crossJs(versions, publishJsFor(publish))
      .crossNative(versions, publishNativeFor(publish))

    def shaded: ProjectMatrix =
      if (shadingSettings.isEmpty) self
      else self.enablePlugins(ShadingPlugin).settings(shadingSettings)

    /**
     * A matrix has one base directory, so each row lists every source directory it reads. An absent
     * directory is harmless.
     */
    private def roots(dirs: String*): Seq[Setting[?]] = {
      def under(conf: String, leaf: String => Seq[String]) = Def.setting {
        // a matrix base can be relative, so resolve a source directory against the build root
        val root = IO.resolve((ThisBuild / baseDirectory).value, self.base)
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
        platform: Seq[Setting[?]],
        ss: Seq[Def.SettingsDefinition],
    ): Seq[Setting[?]] = platform ++ roots("shared", dir) ++ ss.flatMap(_.settings)

  }

  /**
   * Names the row that gets the id without a suffix. sbt 2 would leave the Scala 3 JVM row bare and
   * rename every id CI and the aliases use.
   */
  private def bareAxes: Seq[VirtualAxis] = Seq(
    VirtualAxis.jvm,
    VirtualAxis.scalaABIVersion(LatestScala213),
    /* projectMatrix leaves out an axis whose value a default repeats, and it counts two axes as
     * equal by version — so this default names the binary version and no real one. */
    VirtualAxis.scalaVersionAxis("3", "3"),
  )

  /**
   * Splits a version list into the Scala 2 versions and the Scala 3 ones. Every Scala 3 version
   * gets a row of its own, keyed by an axis, and only Scala3Published publishes.
   */
  private def splitScala3(versions: Seq[String]) = {
    val s3 = Seq.newBuilder[(Seq[String], Seq[VirtualAxis.ScalaVersionAxis], Seq[Setting[_]])]
    val s2 = Seq.newBuilder[String]
    versions.foreach(v =>
      Scala3RowIds.get(v) match {
        case Some(id) =>
          val settings =
            if (v == Scala3Published) Nil
            else Def.settings(nonPublishableSettings, allowMismatchScala := true)
          s3 += ((Seq(v), Seq(VirtualAxis.ScalaVersionAxis(v, id)), settings))
        case None => s2 += v
      },
    )
    (s2.result(), s3.result())
  }

}

import org.scalameta.build.Versions._
import org.scalameta.build._

import sbt.io.IO

import java.io._

import scala.xml.transform.RewriteRule
import scala.xml.transform.RuleTransformer
import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}

import org.scalajs.linker.interface.StandardConfig

import complete.DefaultParsers._
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import sbtcrossproject.Platform

def customVersion = sys.props.get("scalameta.version")
def parseTagVersion: String = {
  import scala.sys.process._
  // drop `v` prefix
  "git describe --abbrev=0 --tags".!!.drop(1).trim
}
def localSnapshotVersion: String = s"$parseTagVersion-SNAPSHOT"
def isCI = System.getenv("CI") != null

// ==========================================
// Projects
// ==========================================

sharedSettings
name := {
  println(s"[info] Welcome to scalameta ${version.value}")
  "scalametaRoot"
}
nonPublishableSettings
crossScalaVersions := Nil
enablePlugins(ScalaUnidocPlugin)
addCommandAlias("benchAll", benchAll.command)
addCommandAlias("benchLSP", benchLSP.command)
addCommandAlias("benchQuick", benchQuick.command)
commands += Command.command("mima")(s => "mimaReportBinaryIssues" :: "doc" :: s)
commands += Command.command("download-scala-library") { s =>
  val out = file("target/scala-library")
  IO.unzipURL(
    url(s"https://github.com/scala/scala/archive/v$LatestScala213.zip"),
    toDirectory = out,
    filter = s"scala-$LatestScala213/src/library/*"
  )
  s
}
commands += Command.command("save-expect")(s =>
  List(LatestScala212, LatestScala213).foldLeft(s) { case (s, version) =>
    s"++$version" :: "semanticdbScalacPlugin/compile" :: "semanticdbIntegration/clean" ::
      "semanticdbIntegration/compile" ::
      "testsSemanticdb/Test/runMain scala.meta.tests.semanticdb.SaveExpectTest" :: s
  }
)
commands += Command.command("save-manifest")(s =>
  "testsJVM/test:runMain scala.meta.tests.semanticdb.SaveManifestTest" :: s
)
def helloContributor(): Unit = println(
  """Welcome to the Scalameta build! You probably don't want to run `sbt test` since
    |that will take a long time to complete.  More likely, you want to run `testsJVM/test`.
    |For more productivity tips, please read CONTRIBUTING.md.
    |""".stripMargin
)
test := helloContributor()
test / aggregate := false
testOnly := helloContributor()
testOnly / aggregate := false
packagedArtifacts := Map.empty
ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject
console := (scalameta.jvm / Compile / console).value
Global / resolvers +=
  "scala-integration".at("https://scala-ci.typesafe.com/artifactory/scala-integration/")

val commonJsSettings = Seq(
  crossScalaVersions := List(LatestScala213ForJS, LatestScala212),
  scalaVersion := LatestScala213ForJS,
  bspEnabled := false,
  scalaJSLinkerConfig := StandardConfig().withBatchMode(true),
  scalacOptions ++= {
    if (isSnapshot.value) Seq.empty
    else {
      val localDir = (ThisBuild / baseDirectory).value.toURI.toString
      val githubDir = "https://raw.githubusercontent.com/scalameta/scalameta"
      Seq(s"-P:scalajs:mapSourceURI:$localDir->$githubDir/v${version.value}/")
    }
  }
)

lazy val nativeSettings = Seq(
  crossScalaVersions := List(LatestScala213, LatestScala212),
  scalaVersion := LatestScala213,
  bspEnabled := false,
  nativeConfig ~= { _.withMode(scalanative.build.Mode.releaseFast) }
)

val allPlatforms = Seq(JSPlatform, JVMPlatform, NativePlatform)

/* ======================== SEMANTICDB ======================== */
lazy val semanticdbScalacCore = project.in(file("semanticdb/scalac/library")).settings(
  moduleName := "semanticdb-scalac-core",
  sharedSettings,
  publishJVMSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "Library to generate SemanticDB from Scalac 2.x internal data structures",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
).dependsOn(semanticdbShared.jvm, io.jvm)

lazy val semanticdbShared = crossProject(allPlatforms: _*).in(file("semanticdb/semanticdb"))
  .settings(
    moduleName := "semanticdb-shared",
    sharedSettings,
    publishJVMSettings,
    libraryDependencies += "org.scala-lang" % "scalap" % scalaVersion.value,
    crossScalaVersions := EarliestScala2Versions,
    protobufSettings,
    mimaPreviousArtifacts := Set.empty,
    description := "Library defining SemanticDB data structures"
  ).dependsOn(scalameta).nativeSettings(nativeSettings).jsSettings(commonJsSettings)

lazy val semanticdbScalacPlugin = project.in(file("semanticdb/scalac/plugin")).settings(
  moduleName := "semanticdb-scalac",
  description := "Scalac 2.x compiler plugin that generates SemanticDB on compile",
  sharedSettings,
  publishJVMSettings,
  mimaPreviousArtifacts := Set.empty,
  mergeSettings,
  fullCrossVersionSettings,
  pomPostProcess := { node =>
    new RuleTransformer(new RewriteRule {
      private def isAbsorbedDependency(node: XmlNode): Boolean = {
        def isArtifactId(node: XmlNode, fn: String => Boolean) = node.label == "artifactId" &&
          fn(node.text)
        node.label == "dependency" &&
        node.child.exists(child => isArtifactId(child, _.startsWith("semanticdb-scalac-core")))
      }
      override def transform(node: XmlNode): XmlNodeSeq = node match {
        case e: Elem if isAbsorbedDependency(node) =>
          Comment("the dependency that was here has been absorbed via sbt-assembly")
        case _ => node
      }
    }).transform(node).head
  }
).dependsOn(semanticdbScalacCore)

lazy val semanticdbMetac = project.in(file("semanticdb/metac")).settings(
  moduleName := "metac", // that was name chosen originally, must keep it
  sharedSettings,
  publishJVMSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "Scalac 2.x launcher that generates SemanticDB on compile",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  mainClass := Some("scala.meta.cli.Metac")
).dependsOn(semanticdbScalacCore)

lazy val semanticdbMetap = project.in(file("semanticdb/metap")).settings(
  moduleName := "semanticdb-metap",
  sharedSettings,
  publishJVMSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "Prints SemanticDB files",
  mainClass := Some("scala.meta.cli.Metap")
).dependsOn(semanticdbShared.jvm)

lazy val semanticdbMetacp = project.in(file("semanticdb/metacp")).settings(
  moduleName := "semanticdb-metacp",
  sharedSettings,
  publishJVMSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "Generates SemanticDB files for a classpath",
  mainClass := Some("scala.meta.cli.Metacp")
).dependsOn(semanticdbScalacCore)

/* ============== CODEGEN FOR SCALA 3 QUASIQUOTES ============= */
lazy val scala3TreeLiftsMacro = project.in(file("scala3-tree-lifts/macro")).settings(
  crossScalaVersions := List(LatestScala213),
  scalaVersion := LatestScala213,
  enableMacros,
  nonPublishableSettings
).dependsOn(trees.jvm, common.jvm)

lazy val scala3TreeLiftsCodeGen = project.in(file("scala3-tree-lifts/impl")).settings(
  crossScalaVersions := List(LatestScala213),
  scalaVersion := LatestScala213,
  nonPublishableSettings
).dependsOn(scala3TreeLiftsMacro)

/* ======================== SCALAMETA ======================== */
lazy val common = crossProject(allPlatforms: _*).in(file("scalameta/common")).settings(
  moduleName := "common",
  sharedSettings,
  libraryDependencies += {
    val sourceCodeVersion = if (isScala211.value) "0.3.1" else "0.4.2"
    "com.lihaoyi" %%% "sourcecode" % sourceCodeVersion
  },
  description := "Bag of private and public helpers used in scalameta APIs and implementations",
  enableMacros,
  buildInfoPackage := "scala.meta.internal",
  buildInfoKeys := Seq[BuildInfoKey](version),
  crossScalaVersions := EarliestScala2Versions
).configureCross(crossPlatformPublishSettings).jsSettings(commonJsSettings)
  .enablePlugins(BuildInfoPlugin).nativeSettings(nativeSettings)

lazy val io = crossProject(allPlatforms: _*).in(file("scalameta/io"))
  .configureCross(crossPlatformPublishSettings).settings(
    moduleName := "io",
    sharedSettings,
    description := "Scalameta IO abstractions",
    mimaPreviousArtifacts := Set.empty, // XXX: io split off from trees, to remove after release
    crossScalaVersions := EarliestScala2Versions
  ).jsSettings(commonJsSettings).nativeSettings(nativeSettings)

lazy val trees = crossProject(allPlatforms: _*).in(file("scalameta/trees")).settings(
  moduleName := "trees",
  sharedSettings,
  description := "Scalameta abstract syntax trees",
  crossScalaVersions := EarliestScala2Versions,
  // NOTE: uncomment this to update ast.md
  // scalacOptions += "-Xprint:typer",
  enableHardcoreMacros,
  libraryDependencies ++= {
    val fastparseVersion =
      if (isScala211.value) "3.0.2"
      else if (VersionNumber(scalaVersion.value).matchesSemVer(SemanticSelector("<2.13.14")))
        "3.1.0"
      else "3.1.1"
    List("com.lihaoyi" %%% "fastparse" % fastparseVersion)
  },
  mergedModule(projects2 = { base =>
    val scalameta = base / "scalameta"
    List("tokenizers", "tokens", "dialects", "inputs").map(scalameta / _)
  })
) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty
  .configureCross(crossPlatformPublishSettings).configureCross(crossPlatformShading)
  .jsSettings(commonJsSettings).nativeSettings(nativeSettings).dependsOn(common, io)

lazy val parsers = crossProject(allPlatforms: _*).in(file("scalameta/parsers")).settings(
  moduleName := "parsers",
  sharedSettings,
  description := "Scalameta APIs for parsing and their baseline implementation",
  enableHardcoreMacros,
  crossScalaVersions := EarliestScalaVersions,
  mergedModule(
    base => List(base / "scalameta" / "quasiquotes"),
    base => List(base / "scalameta" / "transversers")
  ),
  Compile / sourceGenerators += Def.taskDyn {
    val outFile = (Compile / sourceManaged).value / "generated" / "TreeLifts.scala"
    Def.task(if (scalaVersion.value.startsWith("3")) {
      (Compile / (scala3TreeLiftsCodeGen / run)).toTask(" " + outFile.getAbsolutePath).value
      Seq(outFile)
    } else Seq())
  }.taskValue
).configureCross(crossPlatformPublishSettings).configureCross(crossPlatformShading)
  .jsConfigure(_.enablePlugins(NpmPackagePlugin)).jsSettings(
    commonJsSettings,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    npmPackageName := "scalameta-parsers",
    npmPackageDescription := "Library to parse Scala programs",
    npmPackageRepository := Some("https://github.com/scalameta/scalameta"),
    npmPackageAuthor := "scalameta",
    npmPackageLicense := Some("BSD-3-Clause"),
    npmPackageKeywords := Seq("scala", "parser"),
    npmPackageStage := org.scalajs.sbtplugin.Stage.FullOpt,
    npmPackageAdditionalNpmConfig :=
      Map("homepage" -> _root_.io.circe.Json.fromString("https://scalameta.org/")),
    npmPackageREADME := Some(file("README.npm.md"))
  ).nativeSettings(nativeSettings).dependsOn(trees)

def mergedModule(
    projects: File => List[File] = _ => Nil,
    projects2: File => List[File] = _ => Nil,
    projects3: File => List[File] = _ => Nil
): List[Setting[_]] = List {
  Compile / unmanagedSourceDirectories ++= {
    val base = (ThisBuild / baseDirectory).value
    val scalaBinary = "scala-" + scalaBinaryVersion.value
    val scalaMajor = if (isScala3.value) "scala-3" else "scala-2"
    val allProjects = Iterable
      .concat(projects(base), if (isScala3.value) projects3(base) else projects2(base))
    val res = Seq.newBuilder[File]
    allProjects.foreach { project =>
      res += project / "shared" / "src" / "main" / scalaBinary
      res += project / "shared" / "src" / "main" / scalaMajor
      res += project / "shared" / "src" / "main" / "scala"
      res += project / crossProjectPlatform.value.identifier / "src" / "main" / "scala"
    }
    res.result()
  }
}

lazy val scalameta = crossProject(allPlatforms: _*).in(file("scalameta/scalameta")).settings(
  moduleName := "scalameta",
  sharedSettings,
  description := "Scalameta umbrella module that includes all public APIs",
  crossScalaVersions := EarliestScalaVersions,
  mergedModule(base => List(base / "scalameta" / "contrib"))
).configureCross(crossPlatformPublishSettings).configureCross(crossPlatformShading)
  .jsSettings(commonJsSettings).nativeSettings(nativeSettings).dependsOn(parsers)

/* ======================== TESTS ======================== */
lazy val semanticdbIntegration = project.in(file("semanticdb/integration")).settings(
  description := "Sources to compile to build SemanticDB for tests.",
  sharedSettings,
  crossScalaVersions := AllScala2Versions,
  nonPublishableSettings,
  // the sources in this project intentionally produce warnings to test the
  // diagnostics pipeline in semanticdb-scalac.
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions -= "-Wconf:cat=deprecation:is",
  scalacOptions += "-deprecation",
  scalacOptions ++= {
    if (scalaVersion.value >= "2.13.14") Seq(
      // "-Xsource:3",
      "-Xsource-features:leading-infix"
    )
    else Nil
  },
  scalacOptions ++= {
    val pluginJar = (semanticdbScalacPlugin / Compile / Keys.`package`).value.getAbsolutePath
    val warnUnusedImports = if (isScala213.value) "-Wunused:imports" else "-Ywarn-unused-import"
    Seq(
      s"-Xplugin:$pluginJar",
      "-Xplugin-require:semanticdb",
      warnUnusedImports,
      "-Yrangepos",
      "-P:semanticdb:text:on", // include text to print occurrences in expect suite
      "-P:semanticdb:failures:error", // fail fast during development.
      "-P:semanticdb:exclude:Exclude.scala",
      s"-P:semanticdb:sourceroot:${(ThisBuild / baseDirectory).value}",
      "-P:semanticdb:synthetics:on"
    )
  },
  Compile / javaHome := {
    // force javac to fork by setting javaHome to workaround https://github.com/sbt/zinc/issues/520
    val home = file(sys.props("java.home"))
    val actualHome =
      if (System.getProperty("java.version").startsWith("1.8")) home.getParentFile else home
    Some(actualHome)
  },
  javacOptions += "-parameters"
).dependsOn(semanticdbIntegrationMacros, semanticdbScalacPlugin)

lazy val semanticdbIntegrationMacros = project.in(file("semanticdb/integration-macros")).settings(
  sharedSettings,
  crossScalaVersions := AllScala2Versions,
  nonPublishableSettings,
  enableMacros
)

lazy val testkit = crossProject(allPlatforms: _*).in(file("scalameta/testkit")).settings(
  moduleName := "testkit",
  sharedSettings,
  crossScalaVersions := EarliestScalaVersions,
  hasLargeIntegrationTests,
  description := "Testing utilities for scalameta APIs"
).dependsOn(scalameta, io).configureCross(crossPlatformPublishSettings)
  .jvmSettings(libraryDependencies += "org.rauschig" % "jarchivelib" % "1.2.0")
  .jsSettings(commonJsSettings).nativeSettings(nativeSettings)

lazy val tests = crossProject(allPlatforms: _*).in(file("tests")).settings(testSettings).jvmSettings(
  crossScalaVersions := AllScalaVersions,
  libraryDependencies ++= {
    if (!isScala3.value) List("org.scala-lang" % "scala-reflect" % scalaVersion.value) else Nil
  },
  dependencyOverrides += {
    val scalaXmlVersion = if (isScala211.value) "1.3.0" else "2.1.0"
    "org.scala-lang.modules" %%% "scala-xml" % scalaXmlVersion
  },
  libraryDependencies ++= {
    if (isScala213.value) List(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0" % Test
    )
    else Nil
  },
  scalacOptions ++= {
    if (isScala3.value)
      List("-Wconf:msg=pattern binding uses refutable extractor:s", "-Xcheck-macros")
    else Nil
  }
).jsSettings(
  commonJsSettings,
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
).nativeSettings(
  nativeSettings,
  nativeConfig ~= { _.withMode(scalanative.build.Mode.debug).withLinkStubs(true) }
).enablePlugins(BuildInfoPlugin).dependsOn(scalameta, testkit)

lazy val testsSemanticdb = project.in(file("tests-semanticdb")).settings(
  crossScalaVersions := AllScala2Versions,
  testSettings,
  Test / fullClasspath := {
    val semanticdbScalacJar = (semanticdbScalacPlugin / Compile / Keys.`package`).value
      .getAbsolutePath
    sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar") = semanticdbScalacJar
    (Test / fullClasspath).value
  },
  // Needed because some tests rely on the --usejavacp option
  Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
).dependsOn(
  scalameta.jvm,
  testkit.jvm,
  semanticdbMetac,
  semanticdbMetacp,
  semanticdbMetap,
  semanticdbIntegration
).enablePlugins(BuildInfoPlugin)

lazy val sharedTestSettings = Def.settings(
  sharedSettings,
  nonPublishableSettings,
  testFrameworks := List(new TestFramework("munit.Framework")),
  dependencyOverrides ++= {
    if (isScala3.value) Nil else Seq("org.scala-lang" % "scala-library" % scalaVersion.value)
  },
  libraryDependencies += {
    val munitV = if (isScala211.value) "1.0.0-M10" else munit.sbtmunit.BuildInfo.munitVersion
    "org.scalameta" %%% "munit" % munitV
  }
)

lazy val testSettings = Def.settings(
  sharedTestSettings,
  Test / unmanagedSourceDirectories ++= {
    val base = (Compile / baseDirectory).value
    List(base / "src" / "test" / ("scala-" + scalaVersion.value))
  },
  libraryDependencies += {
    val coursierVersion = if (isScala211.value) "2.0.0-RC5-6" else "2.1.24"
    ("io.get-coursier" %% "coursier" % coursierVersion).cross(CrossVersion.for3Use2_13)
  },
  exposePaths("tests", Test),
  buildInfoKeys := Seq[BuildInfoKey](
    scalaVersion,
    scalaBinaryVersion,
    "latestScala212Version" -> LatestScala212,
    "latestScala213Version" -> LatestScala213,
    "databaseSourcepath" -> (ThisBuild / baseDirectory).value.getAbsolutePath,
    "resourcesDirectory" -> (Test / resourceDirectory).value.getAbsolutePath,
    "commonJVMClassDirectory" -> (common.jvm / Compile / classDirectory).value.getAbsolutePath,
    "databaseClasspath" -> (semanticdbIntegration / Compile / classDirectory).value.getAbsolutePath,
    "integrationSourceDirectories" -> (semanticdbIntegration / Compile / sourceDirectories).value
  ),
  buildInfoPackage := "scala.meta.tests"
)

lazy val communitytest = project.in(file("community-test"))
  .settings(sharedTestSettings, crossScalaVersions := LatestScala2Versions).dependsOn(scalameta.jvm)

/* ======================== BENCHES ======================== */
lazy val bench = project.in(file("bench/suite")).enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin).settings(
    sharedSettings,
    crossScalaVersions := LatestScala2Versions,
    nonPublishableSettings,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    buildInfoKeys := Seq[BuildInfoKey]("sourceroot" -> (ThisBuild / baseDirectory).value),
    buildInfoPackage := "scala.meta.internal.bench",
    Jmh / run := Def.inputTaskDyn {
      val args = spaceDelimited("<arg>").parsed
      val semanticdbScalacJar = (semanticdbScalacPlugin / Compile / Keys.`package`).value
        .getAbsolutePath
      val buf = List.newBuilder[String]
      buf += "org.openjdk.jmh.Main"
      buf ++= args
      buf += "-p"
      buf += s"semanticdbScalacJar=$semanticdbScalacJar"
      (Jmh / runMain).toTask(s"  ${buf.result.mkString(" ")}")
    }.evaluated
  ).dependsOn(testsSemanticdb)

// ==========================================
// Settings
// ==========================================

def isScalaBinaryVersion(version: String) = Def.setting(scalaBinaryVersion.value == version)
lazy val isScala211 = isScalaBinaryVersion("2.11")
lazy val isScala213 = isScalaBinaryVersion("2.13")
lazy val isScala3 = isScalaBinaryVersion("3")
def isScala213or3 = Def.setting(isScala213.value || isScala3.value)

// NOTE: Here's what I'd like to do, but I can't because of deprecations:
//   val isJVM = crossPlatform.value == JVMPlatform
// Here's my second best guess, but it doesn't work due to some reason:
//   val isJVM = platformDepsCrossVersion.value == CrossVersion.binary
def isPlatform(platform: Platform) = Def.settingDyn(
  if (crossProjectPlatform.?.value.isEmpty) Def.setting(platform == JVMPlatform)
  else Def.setting(crossProjectPlatform.value == platform)
)

lazy val sharedSettings = Def.settings(
  version ~= { dynVer =>
    customVersion.getOrElse(
      if (isCI) dynVer else localSnapshotVersion // only for local publishing
    )
  },
  scalaVersion := LatestScala213,
  organization := "org.scalameta",
  libraryDependencies ++= {
    if (isScala213or3.value) Nil
    else List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  },
  scalacOptions ++= { if (isScala213.value) List("-Ymacro-annotations") else Nil },
  scalacOptions ++= { if (isScala213or3.value) List("-Xfatal-warnings") else Nil },
  scalacOptions ++= { if (isScala213.value) List("-Wconf:cat=deprecation:is") else Nil },
  scalacOptions ++= { if (isScala3.value) List("-Wconf:cat=deprecation:silent") else Nil },
  scalacOptions ++= Seq("-feature", "-unchecked"),
  Compile / doc / scalacOptions ++= {
    if (!isScala3.value) Seq("-implicits", "-implicits-hide:.", "-groups") else Seq("-groups")
  },
  Test / parallelExecution := false, // hello, reflection sync!!
  logBuffered := false,
  updateOptions := updateOptions.value.withCachedResolution(true),
  ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger,
  evictionErrorLevel := sbt.util.Level.Warn,
  incOptions := incOptions.value.withLogRecompileOnMacro(false)
)

lazy val mergeSettings = Def.settings(
  sharedSettings,
  assembly / test := {},
  assembly / logLevel := Level.Error,
  assembly / assemblyJarName :=
    name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
  assembly / assemblyOption ~= { _.withIncludeScala(false) },
  Compile / Keys.`package` := {
    val slimJar = (Compile / Keys.`package`).value
    val fatJar = new File(crossTarget.value + "/" + (assembly / assemblyJarName).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), CopyOptions().withOverwrite(true))
    slimJar
  },
  Compile / packageBin / packagedArtifact := {
    val temp = (Compile / packageBin / packagedArtifact).value
    val (art, slimJar) = temp
    val fatJar = new File(crossTarget.value + "/" + (assembly / assemblyJarName).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), CopyOptions().withOverwrite(true))
    (art, slimJar)
  },
  assembly / assemblyMergeStrategy := {
    case PathList("com", "sun", _*) => MergeStrategy.discard
    case PathList("sun", _*) => MergeStrategy.discard
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
  mimaCurrentClassfiles := (Compile / Keys.`package`).value
)

lazy val protobufSettings = Def.settings(
  Compile / packageSrc / mappings ++= {
    val base = (Compile / sourceManaged).value
    val files = (Compile / managedSources).value
    files.map(f => (f, f.relativeTo(base).get.getPath))
  },
  Compile / PB.targets := Seq(protocbridge.Target(
    generator = PB.gens.plugin("scala"),
    outputPath = (Compile / sourceManaged).value / "protobuf",
    options = scalapb.gen(flatPackage =
      true // Don't append filename to package
    )._2
  )),
  Compile / PB.protoSources := Seq(file("semanticdb/semanticdb/shared/src/main/proto")),
  PB.additionalDependencies := Nil,
  libraryDependencies ++= {
    val scalapbVersion =
      if (isScala211.value) "0.9.8"
      // for SIP-51, freeze version to the latest ScalaPB built against the earliest Scala 2.13.x version we support
      else if (EarliestScala213 == "2.13.11") "0.11.13"
      else if (EarliestScala213 == "2.13.12" || scalaVersion.value == "2.13.13") "0.11.15"
      else scalapb.compiler.Version.scalapbVersion
    Seq(
      "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapbVersion,
      "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapbVersion % "protobuf",
      ("com.thesamet.scalapb" % "protoc-gen-scala" % scalapbVersion % "protobuf").artifacts(
        if (scala.util.Properties.isWin)
          Artifact("protoc-gen-scala", PB.ProtocPlugin, "bat", "windows")
        else Artifact("protoc-gen-scala", PB.ProtocPlugin, "sh", "unix")
      )
    )
  }
)

lazy val adhocRepoUri = sys.props("scalameta.repository.uri")
lazy val adhocRepoCredentials = sys.props("scalameta.repository.credentials")
lazy val isCustomRepository = adhocRepoUri != null && adhocRepoCredentials != null

lazy val publishableSettings = Def.settings(
  credentials ++= {
    val credentialsFile = if (adhocRepoCredentials != null) new File(adhocRepoCredentials) else null
    if (credentialsFile != null) List(new FileCredentials(credentialsFile)) else Nil
  },
  Compile / publishArtifact := true,
  Test / publishArtifact := false,
  publishMavenStyle := true,
  pomIncludeRepository := { x => false },
  versionScheme := Some("semver-spec"),
  mimaPreviousArtifacts := {
    if (organization.value == "org.scalameta" && isPlatform(JVMPlatform).value) {
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
      previousVersion.map(organization.value % moduleName.value % _ cross crossVersion.value).toSet
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
    </developers>
)

lazy val nonPublishableSettings = Seq(
  publish / skip := true,
  mimaPreviousArtifacts := Set.empty,
  Compile / packageDoc / publishArtifact := false,
  Compile / doc / sources := Seq.empty,
  publishArtifact := false,
  PgpKeys.publishSigned := {},
  publish := {}
)

def compatibilityPolicyViolation(ticket: String) = Seq(mimaPreviousArtifacts := Set.empty)

lazy val fullCrossVersionSettings = Seq(
  crossVersion := CrossVersion.full,
  crossScalaVersions := AllScala2Versions,
  Compile / unmanagedSourceDirectories += {
    // NOTE: sbt 0.13.8 provides cross-version support for Scala sources
    // (http://www.scala-sbt.org/0.13/docs/sbt-0.13-Tech-Previews.html#Cross-version+support+for+Scala+sources).
    // Unfortunately, it only includes directories like "scala_2.11" or "scala_2.12",
    // not "scala_2.11.8" or "scala_2.12.1" that we need.
    // That's why we have to work around here.
    val base = (Compile / sourceDirectory).value
    val versionDir = scalaVersion.value.replaceAll("-.*", "")
    base / ("scala-" + versionDir)
  }
)

lazy val hasLargeIntegrationTests =
  Seq(Test / run / fork := true, Test / run / javaOptions += "-Xss4m")

def exposePaths(projectName: String, config: Configuration) = {
  def uncapitalize(s: String) =
    if (s.length == 0) ""
    else {
      val chars = s.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }
  val prefix = "sbt.paths." + projectName + "." + uncapitalize(config.name) + "."
  Seq(
    config / scalacOptions := {
      val defaultValue = (config / scalacOptions).value
      System.setProperty(prefix + "options", defaultValue.mkString(" "))
      defaultValue
    },
    config / sourceDirectory := {
      val defaultValue = (config / sourceDirectory).value
      System.setProperty(prefix + "sources", defaultValue.getAbsolutePath)
      defaultValue
    },
    config / resourceDirectory := {
      val defaultValue = (config / resourceDirectory).value
      System.setProperty(prefix + "resources", defaultValue.getAbsolutePath)
      defaultValue
    },
    config / fullClasspath := {
      val defaultValue = (config / fullClasspath).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      System.setProperty(prefix + "classes", classpath.mkString(java.io.File.pathSeparator))
      defaultValue
    }
  )
}

lazy val enableMacros = macroDependencies(hardcore = false)

lazy val enableHardcoreMacros = macroDependencies(hardcore = true)

def macroDependencies(hardcore: Boolean) = libraryDependencies ++= {
  if (isScala3.value) Nil
  else {
    val scalaReflect = "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    val scalaCompiler =
      if (hardcore) List("org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided")
      else Nil
    scalaReflect :: scalaCompiler
  }
}

lazy val docs = project.in(file("scalameta-docs")).settings(
  sharedSettings,
  crossScalaVersions := List(LatestScala213),
  scalaVersion := LatestScala213,
  nonPublishableSettings,
  buildInfoKeys := Seq[BuildInfoKey]("scalameta" -> scalameta),
  buildInfoPackage := "docs",
  moduleName := "scalameta-docs",
  mdocExtraArguments := List("--no-link-hygiene"),
  mdocVariables := Map(
    "VERSION" -> version.value.replaceFirst("\\+.*", ""),
    "SCALA_BINARY_VERSION" -> scalaBinaryVersion.value,
    "SCALA_VERSION" -> scalaVersion.value
  ),
  mdocOut := (ThisBuild / baseDirectory).value / "website" / "target" / "docs",
  mimaPreviousArtifacts := Set.empty
).enablePlugins(BuildInfoPlugin, DocusaurusPlugin)

lazy val shadingSettings = Def.settings(
  shadedDependencies ++= {
    if (isScala211.value) Set.empty
    else ShadedDependency.all.map(x =>
      if (x.isPlatformSpecific) x.groupID %%% x.artifactID % "foo"
      else x.groupID %% x.artifactID % "foo"
    ).toSet
  },
  shadingRules ++= {
    if (isScala211.value) Seq.empty
    else ShadedDependency.all
      .map(x => ShadingRule.moveUnder(x.namespace, "scala.meta.shaded.internal"))
  },
  validNamespaces ++= Set("org", "scala", "java")
)

def platformPublishSettings(platform: sbtcrossproject.Platform) =
  if (Platforms.shouldBuildPlatform(platform)) publishableSettings else nonPublishableSettings

def crossPlatformPublishSettings(project: sbtcrossproject.CrossProject) = project.projects.keys
  .foldLeft(project) { case (res, platform) =>
    val settings = platformPublishSettings(platform)
    if (settings.isEmpty) res else res.configurePlatform(platform)(_.settings(settings))
  }

def crossPlatformShading(project: sbtcrossproject.CrossProject) =
  if (shadingSettings.nonEmpty) project.enablePlugins(ShadingPlugin).settings(shadingSettings)
  else project

val publishJVMSettings = platformPublishSettings(JVMPlatform)

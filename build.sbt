import org.scalameta.build.Extensions._
import org.scalameta.build.Versions._
import org.scalameta.build._

import sbt.io.IO

import java.io._

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}

import complete.DefaultParsers._

def isCI = System.getenv("CI") != null

// ==========================================
// Projects
// ==========================================

lazy val scalametaRoot = rootProject.withId("scalameta-root").autoAggregate
  .enablePlugins(ScalaUnidocPlugin).settings(
    sharedSettings,
    name := {
      println(s"[info] Welcome to scalameta ${version.value}")
      "scalametaRoot"
    },
    nonPublishableSettings,
    crossScalaVersions := Nil,
    addCommandAlias("benchAll", benchAll.command),
    addCommandAlias("benchLSP", benchLSP.command),
    addCommandAlias("benchQuick", benchQuick.command),
    commands += Command.command("releaseSemanticdb")(s =>
      List(
        "semanticdbShared",
        "semanticdbScalacPlugin",
        "semanticdbMetac",
        "semanticdbMetap",
        "semanticdbMetacp",
        "semanticdbScalacCore",
      ).map(s => s + "/publishSigned") ::: s,
    ),
    commands += Command.command("mima")(s => "mimaReportBinaryIssues" :: "doc" :: s),
    commands += Command.command("download-scala-library") { s =>
      val out = file("target/scala-library")
      IO.unzipURL(
        url(s"https://github.com/scala/scala/archive/v$LatestScala213.zip").toURL,
        toDirectory = out,
        filter = s"scala-$LatestScala213/src/library/*",
        preserveLastModified = false,
      )
      s
    },
    commands += Command.command("save-expect")(s =>
      List(LatestScala212, LatestScala213).foldLeft(s) { case (s, version) =>
        s"++$version" :: "semanticdbScalacPlugin/compile" :: "semanticdbIntegration/clean" ::
          "semanticdbIntegration/compile" ::
          "testsSemanticdb/Test/runMain scala.meta.tests.semanticdb.SaveExpectTest" :: s
      },
    ),
    commands += Command.command("save-manifest")(s =>
      "tests/Test/runMain scala.meta.tests.semanticdb.SaveManifestTest" :: s,
    ),
    // `sbt test` here would take hours; say so rather than starting it
    Test / test := {
      helloContributor()
      TestResult.Passed
    },
    Test / testOnly / aggregate := false,
    Test / test / aggregate := false,
    packagedArtifacts := Def.uncached(Map.empty),
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject,
    console := (scalameta.jvm(EarliestScala213) / Compile / console).value,
  )

def helloContributor(): Unit = println(
  """Welcome to the Scalameta build! You probably don't want to run `sbt test` since
    |that will take a long time to complete.  More likely, you want to run `tests/test`.
    |For more productivity tips, please read CONTRIBUTING.md.
    |""".stripMargin,
)
Global / resolvers +=
  "scala-integration".at("https://scala-ci.typesafe.com/artifactory/scala-integration/")

/* ======================== SEMANTICDB ======================== */
lazy val semanticdbScalacCore = project.in(file("semanticdb/scalac/library")).settings(
  moduleName := "semanticdb-scalac-core",
  sharedJvmSettings,
  publishJvmSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  buildInfoPackage := "scala.meta.internal.semanticdb.scalac",
  buildInfoKeys := Seq[BuildInfoKey](scalaVersion),
  description := "Library to generate SemanticDB from Scalac 2.x internal data structures",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  // @unroll keeps SemanticdbConfig's synthetic apply/copy/ctor binary-compatible
  // when fields are added, so mismatched semanticdb-scalac versions (e.g. scalafix
  // vs. sbt) don't fail with NoSuchMethodError. See #4640.
  libraryDependencies +=
    compilerPlugin("com.lihaoyi" % "unroll-plugin" % "0.3.0" cross CrossVersion.full),
  // exclude scala-library: the annotation pom pins a newer patch that would trip
  // SIP-51 on the earliest cross-built Scala versions.
  libraryDependencies +=
    ("com.lihaoyi" %% "unroll-annotation" % "0.3.0").exclude("org.scala-lang", "scala-library"),
).dependsOn(semanticdbShared.jvm(EarliestScala213), io.jvm(EarliestScala213))
  .enablePlugins(BuildInfoPlugin)

lazy val semanticdbShared = projectMatrix.in(file("semanticdb/semanticdb")).settings(
  moduleName := "semanticdb-shared",
  sharedSettings,
  libraryDependencies += {
    val ver = if (isScala3.value) EarliestScala213 else scalaVersion.value
    "org.scala-lang" % "scalap" % ver
  },
  protobufSettings,
  description := "Library defining SemanticDB data structures",
).dependsOn(scalameta).crossAllPublished(EarliestScalaVersions, EarliestScalaVersions)

lazy val semanticdbScalacPlugin = project.in(file("semanticdb/scalac/plugin")).settings(
  moduleName := "semanticdb-scalac",
  description := "Scalac 2.x compiler plugin that generates SemanticDB on compile",
  sharedJvmSettings,
  publishJvmSettings,
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
  },
).dependsOn(semanticdbScalacCore)

lazy val semanticdbMetac = project.in(file("semanticdb/metac")).settings(
  moduleName := "metac", // that was name chosen originally, must keep it
  sharedJvmSettings,
  publishJvmSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "Scalac 2.x launcher that generates SemanticDB on compile",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  mainClass := Some("scala.meta.cli.Metac"),
).dependsOn(semanticdbScalacPlugin)

lazy val semanticdbMetap = project.in(file("semanticdb/metap")).settings(
  moduleName := "semanticdb-metap",
  sharedJvmSettings,
  publishJvmSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "Prints SemanticDB files",
  mainClass := Some("scala.meta.cli.Metap"),
).dependsOn(semanticdbShared.jvm(EarliestScala213))

lazy val semanticdbMetacp = project.in(file("semanticdb/metacp")).settings(
  moduleName := "semanticdb-metacp",
  sharedJvmSettings,
  publishJvmSettings,
  fullCrossVersionSettings,
  mimaPreviousArtifacts := Set.empty,
  description := "Generates SemanticDB files for a classpath",
  mainClass := Some("scala.meta.cli.Metacp"),
).dependsOn(semanticdbScalacCore)

/* ============== CODEGEN FOR SCALA 3 QUASIQUOTES, TRANSVERSERS ============= */
lazy val scala3TreeLiftsMacro = project.in(file("scala3-tree-lifts/macro")).settings(
  jvmPlatformSettings,
  crossScalaVersions := List(LatestScala213),
  scalaVersion := LatestScala213,
  enableMacros,
  nonPublishableSettings,
).dependsOn(trees.jvm(EarliestScala213), common.jvm(EarliestScala213))

lazy val scala3TreeLiftsCodeGen = project.in(file("scala3-tree-lifts/impl")).settings(
  jvmPlatformSettings,
  crossScalaVersions := List(LatestScala213),
  scalaVersion := LatestScala213,
  libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0",
  nonPublishableSettings,
).dependsOn(scala3TreeLiftsMacro)

/* ======================== SCALAMETA ======================== */
lazy val common2 = projectMatrix.in(file("scalameta/common2")).settings(
  moduleName := "common2",
  sharedSettings,
  enableMacros,
  buildInfoPackage := "scala.meta.internal",
  buildInfoKeys := Seq[BuildInfoKey](version),
).crossAllPublished(EarliestScala2Versions).enablePlugins(BuildInfoPlugin)

lazy val common = projectMatrix.in(file("scalameta/common")).settings(
  moduleName := "common",
  sharedSettings,
  libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.4",
  description := "Bag of private and public helpers used in scalameta APIs and implementations",
  enableMacros,
).crossAllPublished(EarliestScalaVersions, EarliestScalaVersions).enablePlugins(BuildInfoPlugin)
  .dependsOn(common2)

lazy val io = projectMatrix.in(file("scalameta/io"))
  .settings(moduleName := "io", sharedSettings, description := "Scalameta IO abstractions")
  .crossAllPublished(EarliestScala2Versions)

lazy val trees2 = projectMatrix.in(file("scalameta/trees2")).settings(
  moduleName := "trees2",
  sharedSettings,
  // NOTE: uncomment this to update ast.md
  // scalacOptions += "-Xprint:typer",
  enableHardcoreMacros,
  mergedModule(projects2 = { base =>
    val scalameta = base / "scalameta"
    List("tokenizers2", "tokens2", "dialects2", "inputs2").map(scalameta / _)
  }),
  // no Scala 3 build of it exists, and a row forced to Scala 3 still needs the annotation
  libraryDependencies +=
    ("org.portable-scala" %% "portable-scala-reflect" % "1.1.3").cross(CrossVersion.for3Use2_13),
).crossAllPublished(EarliestScala2Versions).dependsOn(common2, io)

// trees is the only module carrying shaded code: its published jar relocates fastparse and geny
// under scala.meta.shaded.internal, while trees2, parsers and scalameta ship none.
lazy val trees = projectMatrix.in(file("scalameta/trees")).settings(
  moduleName := "trees",
  sharedSettings,
  description := "Scalameta abstract syntax trees",
  enableHardcoreMacros,
  libraryDependencies ++= {
    val fastparseVersion =
      if (VersionNumber(scalaVersion.value).matchesSemVer(SemanticSelector("<2.13.14"))) "3.1.0"
      else "3.1.1"
    List("com.lihaoyi" %% "fastparse" % fastparseVersion)
  },
  mergedModule(projects = { base =>
    val scalameta = base / "scalameta"
    List("tokenizers").map(scalameta / _)
  }),
) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty
  .crossAllPublished(EarliestScalaVersions, EarliestScalaVersions).shaded
  .dependsOn(common, io, trees2)

def parsersJsSettings = Def.settings(
  publishJsFor(EarliestScalaVersions),
  // has to agree with the "type" NpmPackage writes into package.json
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
  NpmPackage.settings(
    pkgName = "scalameta-parsers",
    pkgDescription = "Library to parse Scala programs",
    pkgRepository = "https://github.com/scalameta/scalameta",
    pkgAuthor = "scalameta",
    pkgLicense = "BSD-3-Clause",
    pkgKeywords = Seq("scala", "parser"),
    pkgHomepage = "https://scalameta.org/",
    pkgReadme = file("README.npm.md"),
  ),
)

lazy val parsers = projectMatrix.in(file("scalameta/parsers")).settings(
  moduleName := "parsers",
  sharedSettings,
  description := "Scalameta APIs for parsing and their baseline implementation",
  enableHardcoreMacros,
  mergedModule(
    base => List(base / "scalameta" / "quasiquotes", base / "scalameta" / "transversers"),
    base => List(base / "scalameta" / "transversers2"),
  ),
  Compile / sourceGenerators += Def.taskDyn {
    if (isScala3.value) {
      val args = Map(
        "treelifts" -> "TreeLifts.scala",
        "traversers" -> "Traversers.scala",
        "transformers" -> "Transformers.scala",
      )
      val outDir = (Compile / sourceManaged).value / "generated"
      val argsIter = args.toIterator ++ Iterator("dir" -> outDir.getAbsolutePath)
      val argsString = argsIter.map { case (k, v) => s" --$k=$v" }.mkString
      Def.task {
        (Compile / (scala3TreeLiftsCodeGen / run)).toTask(argsString).value
        args.values.map(outDir / _).toSeq
      }
    } else Def.task(Seq.empty[File])
  }.taskValue,
).crossJvm(EarliestScalaVersions, publishJvmFor(EarliestScalaVersions))
  .crossNative(EarliestScalaVersions, publishNativeFor(EarliestScalaVersions))
  .crossJs(EarliestScalaVersions, parsersJsSettings).dependsOn(trees)

def mergedModule(
    projects: File => List[File] = _ => Nil,
    projects2: File => List[File] = _ => Nil,
    projects3: File => List[File] = _ => Nil,
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
      res += project / platformAxis.value.id / "src" / "main" / "scala"
    }
    res.result()
  }
}

lazy val scalameta = projectMatrix.in(file("scalameta/scalameta")).settings(
  moduleName := "scalameta",
  sharedSettings,
  description := "Scalameta umbrella module that includes all public APIs",
  mergedModule(base => List(base / "scalameta" / "contrib")),
).crossAllPublished(EarliestScalaVersions, EarliestScalaVersions).dependsOn(parsers)

/* ======================== TESTS ======================== */
lazy val semanticdbIntegration = project.in(file("semanticdb/integration")).settings(
  description := "Sources to compile to build SemanticDB for tests.",
  sharedJvmSettings,
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
      "-Xsource-features:leading-infix",
    )
    else Nil
  },
  scalacOptions ++= {
    val pluginJar = fileConverter.value
      .toPath((semanticdbScalacPlugin / Compile / Keys.`package`).value).toAbsolutePath.toString
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
      "-P:semanticdb:synthetics:on",
    )
  },
  Compile / javaHome := {
    // force javac to fork by setting javaHome to workaround https://github.com/sbt/zinc/issues/520
    val home = file(sys.props("java.home"))
    val actualHome =
      if (System.getProperty("java.version").startsWith("1.8")) home.getParentFile else home
    Some(actualHome)
  },
  javacOptions += "-parameters",
).dependsOn(semanticdbIntegrationMacros, semanticdbScalacPlugin)

lazy val semanticdbIntegrationMacros = project.in(file("semanticdb/integration-macros")).settings(
  sharedJvmSettings,
  crossScalaVersions := AllScala2Versions,
  nonPublishableSettings,
  enableMacros,
)

lazy val testkit = projectMatrix.in(file("scalameta/testkit")).settings(
  moduleName := "testkit",
  sharedSettings,
  hasLargeIntegrationTests,
  description := "Testing utilities for scalameta APIs",
).dependsOn(scalameta, io).crossJvm(
  EarliestScalaVersions,
  publishJvmFor(EarliestScalaVersions),
  libraryDependencies += "org.rauschig" % "jarchivelib" % "1.2.0",
).crossJs(EarliestScalaVersions, publishJsFor(EarliestScalaVersions))
  .crossNative(EarliestScalaVersions, publishNativeFor(EarliestScalaVersions))

lazy val tests = projectMatrix.in(file("tests")).settings(
  testSettings,
  scalacOptions ++= {
    if (isScala3.value)
      List("-Wconf:msg=pattern binding uses refutable extractor:s", "-Xcheck-macros")
    else Nil
  },
).crossJvm(
  EarliestScalaVersions,
  // A JVM row is built at the earliest patch of its binary version, while munit's pom asks for a
  // newer one, and sbt 2 will not compile against a scala-library newer than scalaVersion. Only
  // here: the JS rows are built at the newest patch already, Scala Native pins its own, and the
  // JVM-only test projects depend on rows built at 2.13.15, so forcing a version on any of them
  // makes coursier's SameVersion rule reject the graph.
  dependencyOverrides ++=
    { if (isScala3.value) Nil else Seq("org.scala-lang" % "scala-library" % scalaVersion.value) },
  libraryDependencies ++=
    { if (!isScala3.value) List("org.scala-lang" % "scala-reflect" % scalaVersion.value) else Nil },
  dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.4.0",
  libraryDependencies ++= {
    if (isScala213.value) List(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0" % Test,
    )
    else Nil
  },
).crossJs(EarliestScalaVersions, scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) })
  .crossNative(
    EarliestScalaVersions,
    nativeConfig ~= { _.withMode(scalanative.build.Mode.debug).withLinkStubs(true) },
    // Scala Native's own dependencies require a newer scala-library than the earliest patch this
    // row compiles at, and unlike the JVM row it cannot be forced back down: coursier's SameVersion
    // rule refuses. sbt 1 compiled this combination without comment. Nothing here is published, so
    // the SIP-51 guarantee the check protects is not at stake.
    allowUnsafeScalaLibUpgrade := true,
  ).enablePlugins(BuildInfoPlugin).dependsOn(scalameta, testkit)

lazy val testsSemanticdb = project.in(file("tests-semanticdb")).settings(
  crossScalaVersions := AllScala2Versions,
  testSettings,
  jvmPlatformSettings,
  scalaVersion := LatestScala213,
  // only this project uses coursier; on a Scala.js row sbt 2's %% would ask for the Scala.js
  // build of it and drag a second suffix of fastparse, geny and sourcecode onto the classpath
  libraryDependencies += "io.get-coursier" %% "coursier" % "2.1.24" cross CrossVersion.for3Use2_13,
  // sets a system property as a side effect, so it must not be served from sbt 2's cache
  Test / fullClasspath := Def.uncached {
    val semanticdbScalacJar = fileConverter.value
      .toPath((semanticdbScalacPlugin / Compile / Keys.`package`).value).toAbsolutePath.toString
    sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar") = semanticdbScalacJar
    (Test / fullClasspath).value
  },
  // Needed because some tests rely on the --usejavacp option
  Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat,
).dependsOn(
  scalameta.jvm(EarliestScala213),
  testkit.jvm(EarliestScala213),
  semanticdbScalacPlugin,
  semanticdbMetac,
  semanticdbMetacp,
  semanticdbMetap,
  semanticdbIntegration,
).enablePlugins(BuildInfoPlugin)

lazy val sharedTestSettings = Def.settings(
  sharedSettings,
  nonPublishableSettings,
  testFrameworks := List(TestFrameworks.MUnit),
  libraryDependencies += "org.scalameta" %% "munit" % munit.sbtmunit.BuildInfo.munitVersion,
)

lazy val testSettings = Def.settings(
  sharedTestSettings,
  Test / unmanagedSourceDirectories ++= {
    val base = (Compile / baseDirectory).value
    List(base / "src" / "test" / ("scala-" + scalaVersion.value))
  },
  exposePaths("tests", Test),
  buildInfoKeys := Seq[BuildInfoKey](
    scalaVersion,
    scalaBinaryVersion,
    "latestScala212Version" -> LatestScala212,
    "latestScala213Version" -> LatestScala213,
    "databaseSourcepath" -> (ThisBuild / baseDirectory).value.getAbsolutePath,
    "resourcesDirectory" -> (Test / resourceDirectory).value.getAbsolutePath,
    "classDirectories" -> Seq(
      (common2.jvm(EarliestScala213) / Compile / classDirectory).value.getAbsolutePath,
      (common.jvm(EarliestScala213) / Compile / classDirectory).value.getAbsolutePath,
    ),
    "databaseClasspath" -> (semanticdbIntegration / Compile / classDirectory).value.getAbsolutePath,
    "integrationSourceDirectories" -> (semanticdbIntegration / Compile / sourceDirectories).value,
  ),
  buildInfoPackage := "scala.meta.tests",
)

lazy val communitytest = project.in(file("community-test")).settings(
  sharedTestSettings,
  jvmPlatformSettings,
  scalaVersion := LatestScala213,
  crossScalaVersions := LatestScala2Versions,
).dependsOn(scalameta.jvm(EarliestScala213))

/* ======================== BENCHES ======================== */
lazy val benchSemanticdb = project.in(file("bench/semanticdb")).enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin).settings(
    sharedJvmSettings,
    crossScalaVersions := LatestScala2Versions,
    nonPublishableSettings,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    buildInfoKeys := Seq[BuildInfoKey]("sourceroot" -> (ThisBuild / baseDirectory).value),
    buildInfoPackage := "scala.meta.internal.bench",
    Jmh / run := Def.inputTaskDyn {
      val args = spaceDelimited("<arg>").parsed
      val semanticdbScalacJar = fileConverter.value
        .toPath((semanticdbScalacPlugin / Compile / Keys.`package`).value).toAbsolutePath.toString
      val buf = List.newBuilder[String]
      buf += "org.openjdk.jmh.Main"
      buf ++= args
      buf += "-p"
      buf += s"semanticdbScalacJar=$semanticdbScalacJar"
      (Jmh / runMain).toTask(s"  ${buf.result().mkString(" ")}")
    }.evaluated,
  ).dependsOn(testsSemanticdb)

lazy val benchScalameta = project.in(file("bench/scalameta")).enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin).settings(
    sharedJvmSettings,
    crossScalaVersions := LatestScala2Versions,
    nonPublishableSettings,
    buildInfoKeys := Seq[BuildInfoKey]("sourceroot" -> (ThisBuild / baseDirectory).value),
    buildInfoPackage := "scala.meta.internal.bench",
    Jmh / resourceDirectory := (Compile / resourceDirectory).value,
    Jmh / fullClasspath ++= {
      // both Append instances match a bare Classpath, so name the type
      val cp: Classpath = (scalameta.jvm(EarliestScala213) / Compile / fullClasspath).value
      cp
    },
    Jmh / run := Def.inputTaskDyn {
      val buf = List.newBuilder[String]
      buf += "org.openjdk.jmh.Main"
      buf ++= spaceDelimited("<arg>").parsed
      (Jmh / runMain).toTask(s"  ${buf.result().mkString(" ")}")
    }.evaluated,
  ).dependsOn(scalameta.jvm(EarliestScala213))

// ==========================================
// Settings
// ==========================================

lazy val sharedJvmSettings = Def
  .settings(sharedSettings, jvmPlatformSettings, scalaVersion := LatestScala213)

lazy val sharedSettings = Def.settings(
  // version is set dynamically by sbt-dynver, but let's adjust it
  version := sys.props.get("scalameta.version").getOrElse {
    val curVersion = version.value
    def dynVer(out: sbtdynver.GitDescribeOutput): String = {
      def tagVersion = out.ref.dropPrefix
      if (out.isCleanAfterTag) tagVersion
      else if (System.getenv("CI") == null) s"$tagVersion-next-SNAPSHOT" // modified for local builds
      else if (out.commitSuffix.distance == 0) tagVersion
      else if (sys.props.contains("backport.release")) tagVersion
      else curVersion
    }
    dynverGitDescribeOutput.value.mkVersion(dynVer, curVersion)
  },
  isSnapshot := version.value.endsWith("-SNAPSHOT"), // overrides dynver setting
  organization := "org.scalameta",
  libraryDependencies ++= {
    if (isScala213or3.value) Nil
    else List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  },
  scalacOptions ++= { if (isScala213.value) List("-Ymacro-annotations") else Nil },
  scalacOptions ++= { if (isScala213or3.value) List("-Xfatal-warnings") else Nil },
  scalacOptions ++= { if (isScala213.value) List("-Wconf:cat=deprecation:is") else Nil },
  scalacOptions ++= {
    if (isScala3.value) List(
      "-Wconf:msg=.*no longer supported for vararg splices.*:silent",
      "-Wconf:msg=.*Implicit parameters should be provided.*:silent",
      "-Wconf:msg=.* deprecated.*:silent", // covers several
      "-Wconf:cat=deprecation:silent",
    )
    else Nil
  },
  scalacOptions ++= Seq("-feature", "-unchecked"),
  Compile / doc / scalacOptions ++=
    { if (!isScala3.value) Seq("-implicits", "-implicits-hide:.", "-groups") else Seq("-groups") },
  Test / parallelExecution := false, // hello, reflection sync!!
  logBuffered := false,
  updateOptions := updateOptions.value.withCachedResolution(true),
  ThisBuild / watchTriggeredMessage := Watch.clearScreenOnTrigger,
  evictionErrorLevel := sbt.util.Level.Warn,
  incOptions := Def.uncached(incOptions.value.withLogRecompileOnMacro(false)),
)

lazy val mergeSettings = Def.settings(
  sharedJvmSettings,
  // sbt-assembly's shade rules refuse to run against exported jars
  exportJars := false,
  assembly / test := TestResult.Passed,
  assembly / logLevel := Level.Error,
  assembly / assemblyJarName :=
    name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
  assembly / assemblyOption ~= { _.withIncludeScala(false) },
  // Relocate the plugin's internal implementation classes into a per-version
  // package, so two different semanticdb-scalac versions can coexist on one
  // -Xplugin classpath (e.g. scalafix's vs. sbt's) without their identically
  // named internal classes colliding at link time (see #4640). Only the entry
  // point named in scalac-plugin.xml stays put; shade rules still rewrite its
  // references (and InteractiveSemanticdb's) to the relocated classes.
  assembly / assemblyShadeRules := {
    val pkg = "scala.meta.internal.semanticdb.scalac"
    val ver = version.value.replaceAll("[^0-9A-Za-z]", "_")
    Seq(
      ShadeRule.rename(
        s"$pkg.SemanticdbPlugin" -> s"$pkg.SemanticdbPlugin",
        s"$pkg.SemanticdbPlugin$$" -> s"$pkg.SemanticdbPlugin$$",
        // Prevent double-shading: if classes are already in the shaded package, keep them as-is.
        s"$pkg.shaded_v$ver.**" -> s"$pkg.shaded_v$ver.@1",
        s"$pkg.**" -> s"$pkg.shaded_v$ver.@1",
      ).inAll,
    )
  },
  Compile / Keys.`package` := {
    val slimJar = (Compile / Keys.`package`).value
    val fatJar = crossTarget.value / (assembly / assemblyJarName).value
    val _ = assembly.value
    IO.copy(
      List(fatJar -> fileConverter.value.toPath(slimJar).toFile),
      CopyOptions().withOverwrite(true),
    )
    slimJar
  },
  Compile / packageBin / packagedArtifact := {
    val temp = (Compile / packageBin / packagedArtifact).value
    val (art, slimJar) = temp
    val fatJar = crossTarget.value / (assembly / assemblyJarName).value
    val _ = assembly.value
    IO.copy(
      List(fatJar -> fileConverter.value.toPath(slimJar).toFile),
      CopyOptions().withOverwrite(true),
    )
    (art, slimJar)
  },
  assembly / assemblyMergeStrategy := {
    case PathList("com", "sun", _*) => MergeStrategy.discard
    case PathList("sun", _*) => MergeStrategy.discard
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
  mimaCurrentClassfiles := fileConverter.value.toPath((Compile / Keys.`package`).value).toFile,
)

lazy val protobufSettings = Def.settings(
  // sbt 2 puts managed sources in the sources jar itself; adding them duplicates the entries
  Compile / PB.targets := Seq(protocbridge.Target(
    generator = PB.gens.plugin("scala"),
    outputPath = (Compile / sourceManaged).value / "protobuf",
    // what scalapb.gen(flatPackage = true) passes to protoc; spelled out so the meta-build needs
    // no scalapb compilerplugin, whose Scala 3 build wants a different protoc-bridge than sbt-protoc
    options = Seq("flat_package"),
  )),
  Compile / PB.protoSources := Seq(file("semanticdb/semanticdb/shared/src/main/proto")),
  PB.additionalDependencies := Nil,
  libraryDependencies ++= {
    val scalapbVersion =
      // for SIP-51, freeze version to the latest ScalaPB built against the earliest Scala 2.13.x version we support
      if (scalaVersion.value == "2.13.15") "0.11.17" else "0.11.20"
    Seq(
      "com.thesamet.scalapb" %% "scalapb-runtime" % scalapbVersion,
      "com.thesamet.scalapb" %% "scalapb-runtime" % scalapbVersion % "protobuf",
      ("com.thesamet.scalapb" % "protoc-gen-scala" % scalapbVersion % "protobuf").artifacts(
        if (scala.util.Properties.isWin)
          Artifact("protoc-gen-scala", PB.ProtocPlugin, "bat", "windows")
        else Artifact("protoc-gen-scala", PB.ProtocPlugin, "sh", "unix"),
      ),
    )
  },
)

def compatibilityPolicyViolation(ticket: String) = Seq(mimaPreviousArtifacts := Set.empty)

lazy val fullCrossVersionSettings = Seq(
  crossVersion := CrossVersion.full,
  crossScalaVersions := AllScala2Versions,
  Compile / unmanagedSourceDirectories += {
    // NOTE: sbt 0.13.8 provides cross-version support for Scala sources
    // (http://www.scala-sbt.org/0.13/docs/sbt-0.13-Tech-Previews.html#Cross-version+support+for+Scala+sources).
    // Unfortunately, it only includes directories like "scala_2.13", not "scala_2.13.18" that we need.
    // That's why we have to work around here.
    val base = (Compile / sourceDirectory).value
    val versionDir = scalaVersion.value.replaceAll("-.*", "")
    base / ("scala-" + versionDir)
  },
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
  // these set system properties as a side effect, so they must not be served from sbt 2's cache:
  // on a hit the body never runs and the tests that read the properties see null
  Seq(
    config / scalacOptions := Def.uncached {
      val defaultValue = (config / scalacOptions).value
      System.setProperty(prefix + "options", defaultValue.mkString(" "))
      defaultValue
    },
    config / sourceDirectory := Def.uncached {
      val defaultValue = (config / sourceDirectory).value
      System.setProperty(prefix + "sources", defaultValue.getAbsolutePath)
      defaultValue
    },
    config / resourceDirectory := Def.uncached {
      val defaultValue = (config / resourceDirectory).value
      System.setProperty(prefix + "resources", defaultValue.getAbsolutePath)
      defaultValue
    },
    config / fullClasspath := Def.uncached {
      val defaultValue = (config / fullClasspath).value
      val conv = fileConverter.value
      val classpath = defaultValue.map(x => conv.toPath(x.data).toAbsolutePath.toString)
      System.setProperty(prefix + "classes", classpath.mkString(java.io.File.pathSeparator))
      defaultValue
    },
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
  sharedJvmSettings,
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
    "SCALA_VERSION" -> scalaVersion.value,
  ),
  mdocOut := (ThisBuild / baseDirectory).value / "website" / "target" / "docs",
  mimaPreviousArtifacts := Set.empty,
).enablePlugins(BuildInfoPlugin, DocusaurusPlugin)

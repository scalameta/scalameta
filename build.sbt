import org.scalameta.build.Extensions._
import org.scalameta.build.Versions._
import org.scalameta.build._

import sbt.io.IO

import java.io._

import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}

import complete.DefaultParsers._

def isCI = System.getenv("CI") != null

def all(seq: Iterable[String]) = seq.mkString("; ", "; ", "")
/* A forced switch stays in the session, and the steps of one CI job share the sbt server.
 * reload gives each project its own version back; another ++ sets one for every project. */
def forVersions(task: String, ver: Seq[String]) = all(ver.map(v => s"++$v!; $task") :+ "reload")

/**
 * Generates the aliases CI calls for 2.12 and 2.13:
 *   - testsSemanticdb2_12 runs the published patch
 *   - tests2_12_other and testsSemanticdb2_12_other run the patches above it, lowest first
 */
def testAliasesFor(patches: Seq[String]*) = patches.flatten
  .groupBy(v => CrossVersion.binaryScalaVersion(v).replace('.', '_')).toSeq
  .flatMap { case (ver, vs) =>
    val (published, unpublished) = vs.partition(PublishedScalaVersions.contains)
    val res = Seq.newBuilder[Def.Setting[?]]
    val other = ver + "_other"

    // tests has a row per published patch, so a switch reaches the others
    res ++= addCommandAlias(s"tests" + other, forVersions(s"tests$ver/testFull", unpublished))

    // testsSemanticdb has a row per patch, so a step names the row and switches nothing
    def semanticdb(vs: Seq[String]) = forVersions("testsSemanticdb/testFull", vs)
    res ++= addCommandAlias(s"testsSemanticdb" + other, semanticdb(unpublished))
    if (published.nonEmpty) res ++= addCommandAlias("testsSemanticdb" + ver, semanticdb(published))

    res.result()
  }

/**
 * Generates two aliases per graph: one runs every Scala 3 row, the other runs the rows past the
 * pre-merge pair. A version added to Scala3Rows lands in both, so it changes no workflow file.
 */
def scala3Aliases(names: String*) = names.flatMap { name =>
  def testEach(rows: Iterable[(String, String)]) = all(rows.map { case (_, label) =>
    s"$name$label/testFull"
  })
  addCommandAlias(s"${name}3", testEach(Scala3Rows)) ++
    addCommandAlias(s"${name}3_other", testEach(Scala3PostMerge))
}

/* Runs mima for every row that builds this version and has a release to compare against. Only a
 * published JVM row gets a baseline, so the build knows the rows and no list here can go stale. */
def mimaPublished = Command.single("mimaPublished") { (state, version) =>
  val extracted = Project.extract(state)
  val rows = extracted.structure.allProjectRefs.filter(ref =>
    extracted.getOpt(ref / mimaPreviousArtifacts).exists(_.nonEmpty) &&
      extracted.getOpt(ref / scalaVersion).contains(version),
  )
  state.log.info(s"mima checks ${rows.map(_.project).mkString(", ")}")
  rows.map(ref => s"${ref.project}/mimaReportBinaryIssues").toList ::: state
}

def helloContributor(): Unit = println(
  """|Welcome to the Scalameta build! You probably don't want to run `sbt test` since
     |that will take a long time to complete.  More likely, you want to run `tests/test`.
     |For more productivity tips, please read CONTRIBUTING.md.
     |""".stripMargin,
)

// ==========================================
// Projects
// ==========================================

def rootSettings = Def.settings(
  sharedSettings,
  name := "scalametaRoot",
  nonPublishableSettings,
  crossScalaVersions := Nil,
  addCommandAlias("benchAll", benchAll.command),
  addCommandAlias("benchLSP", benchLSP.command),
  addCommandAlias("benchQuick", benchQuick.command),
  testAliasesFor(Scala213Versions, Scala212Versions),
  scala3Aliases("tests", "testsJS", "testsNative"),
  commands += mimaPublished,
  addCommandAlias("mima2_13", "mimaPublished " + PublishedScala213),
  addCommandAlias("mima2_12", "mimaPublished " + PublishedScala212),
  addCommandAlias("mima3_lts", "mimaPublished " + Scala3Published),
  commands += Command.command("releaseSemanticdb")(s =>
    List(
      "semanticdbShared2_13",
      "semanticdbScalacPlugin",
      "semanticdbMetac",
      "semanticdbMetap",
      "semanticdbMetacp",
      "semanticdbScalacCore",
    ).map(s => s + "/publishSigned") ::: s,
  ),
  commands += Command.command("mima")(s => "mimaReportBinaryIssues" :: s),
  commands += Command.command("download-scala-library") { s =>
    val out = file("target/scala-library")
    val arc = uri(s"https://github.com/scala/scala/archive/v$LatestScala213.zip").toURL
    val pat = s"scala-$LatestScala213/src/library/*"
    IO.unzipURL(arc, toDirectory = out, filter = pat, preserveLastModified = false)
    s
  },
  commands += Command.command("save-expect")(s =>
    LatestScala2.foldLeft(s) { case (s, version) =>
      s"++$version" :: "semanticdbScalacPlugin/compile" :: "semanticdbIntegration/clean" ::
        "semanticdbIntegration/compile" ::
        "testsSemanticdb/Test/runMain scala.meta.tests.semanticdb.SaveExpectTest" :: s
    },
  ),
  commands += Command.command("save-manifest")(s =>
    "tests/Test/runMain scala.meta.tests.semanticdb.SaveManifestTest" :: s,
  ),
  // can also be used to ensure sbt server has started
  commands += Command.command("whoami") { s =>
    s.log.info(s"Welcome to scalameta ${Project.extract(s).get(version)}")
    s
  },
  // `sbt test` at the root would take hours, so print advice instead of running it
  Test / test := {
    helloContributor()
    TestResult.Passed
  },
  Test / testOnly := {
    helloContributor()
    TestResult.Passed
  },
  Test / testOnly / aggregate := false,
  Test / test / aggregate := false,
  packagedArtifacts := Def.uncached(Map.empty),
  ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject,
  console := (scalameta.jvm(PublishedScala213) / Compile / console).value,
)

lazy val scalametaRoot = rootProject.withId("scalameta-root").autoAggregate
  .enablePlugins(ScalaUnidocPlugin).settings(rootSettings)

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
).dependsOn(semanticdbShared.jvm(PublishedScala213), io.jvm(PublishedScala213))
  .enablePlugins(BuildInfoPlugin)

def semanticdbSharedSettings = Def.settings(
  moduleName := "semanticdb-shared",
  sharedSettings,
  libraryDependencies += {
    val ver = if (isScala3.value) PublishedScala213 else scalaVersion.value
    "org.scala-lang" % "scalap" % ver
  },
  protobufSettings,
  description := "Library defining SemanticDB data structures",
)

lazy val semanticdbShared = projectMatrix.in(file("semanticdb/semanticdb"))
  .settings(semanticdbSharedSettings).dependsOn(scalameta)
  .crossAllPublished(TestedScalaVersions, PublishedScalaVersions)

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
).dependsOn(semanticdbShared.jvm(PublishedScala213))

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
).dependsOn(trees.jvm(PublishedScala213), common.jvm(PublishedScala213))

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
).crossAllPublished(PublishedScala2).enablePlugins(BuildInfoPlugin)

lazy val common = projectMatrix.in(file("scalameta/common")).settings(
  moduleName := "common",
  sharedSettings,
  libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.4.4",
  description := "Bag of private and public helpers used in scalameta APIs and implementations",
  enableMacros,
).crossAllPublished(TestedScalaVersions, PublishedScalaVersions).enablePlugins(BuildInfoPlugin)
  .dependsOn(common2)

lazy val io = projectMatrix.in(file("scalameta/io"))
  .settings(moduleName := "io", sharedSettings, description := "Scalameta IO abstractions")
  .crossAllPublished(PublishedScala2)

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
  libraryDependencies += "org.portable-scala" %% "portable-scala-reflect" % "1.1.3",
).crossAllPublished(PublishedScala2).dependsOn(common2, io)

lazy val trees = projectMatrix.in(file("scalameta/trees")).settings(
  moduleName := "trees",
  sharedSettings,
  description := "Scalameta abstract syntax trees",
  enableHardcoreMacros,
  libraryDependencies += "com.lihaoyi" %% "fastparse" % { if (isScala212.value) "3.1.0" else "3.1.1" },
  mergedModule(projects = { base =>
    val scalameta = base / "scalameta"
    List("tokenizers").map(scalameta / _)
  }),
) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty
  .crossAllPublished(TestedScalaVersions, PublishedScalaVersions).shaded
  .dependsOn(common, io, trees2)

def parsersJsSettings = Def.settings(
  publishJsFor(PublishedScalaVersions),
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
      val args = Seq(
        "treelifts" -> "TreeLifts.scala",
        "traversers" -> "Traversers.scala",
        "transformers" -> "Transformers.scala",
      )
      val outDir = (Compile / sourceManaged).value / "generated"
      val opts = s"--dir=${outDir.getAbsolutePath}" +: args.map { case (k, v) => s"--$k=$v" }
      val config = scala3TreeLiftsCodeGen / Compile
      Def.task {
        implicit val conv: xsbti.FileConverter = fileConverter.value
        val cp = (config / fullClasspath).value.files
        // runner waits for the process to finish, whereas Compile / runMain doesn't
        (config / runner).value.run("org.scalameta.adt.Main", cp, opts, streams.value.log).get
        args.map(outDir / _._2)
      }
    } else Def.task(Seq.empty[File])
  }.taskValue,
).crossJvm(TestedScalaVersions, publishJvmFor(PublishedScalaVersions))
  .crossNative(TestedScalaVersions, publishNativeFor(PublishedScalaVersions))
  .crossJs(TestedScalaVersions, parsersJsSettings).dependsOn(trees)

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
).crossAllPublished(TestedScalaVersions, PublishedScalaVersions).dependsOn(parsers)

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
  scalacOptions ++= Seq(
    s"-Xplugin:${semanticdbScalacPluginPackage.value}",
    "-Xplugin-require:semanticdb",
    if (isScala213.value) "-Wunused:imports" else "-Ywarn-unused-import",
    "-Yrangepos",
    "-P:semanticdb:text:on", // include text to print occurrences in expect suite
    "-P:semanticdb:failures:error", // fail fast during development.
    "-P:semanticdb:exclude:Exclude.scala",
    s"-P:semanticdb:sourceroot:${(ThisBuild / baseDirectory).value}",
    "-P:semanticdb:synthetics:on",
  ),
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
  TestedScalaVersions,
  publishJvmFor(PublishedScalaVersions),
  libraryDependencies += "org.rauschig" % "jarchivelib" % "1.2.0",
).crossJs(TestedScalaVersions, publishJsFor(PublishedScalaVersions))
  .crossNative(TestedScalaVersions, publishNativeFor(PublishedScalaVersions))

def testsSettings = Def.settings(
  testSettings,
  scalacOptions ++= {
    if (isScala3.value)
      List("-Wconf:msg=pattern binding uses refutable extractor:s", "-Xcheck-macros")
    else Nil
  },
)

def testsJvmSettings = Def.settings(
  /* munit's pom asks for a scala-library newer than this row's patch, and sbt 2 refuses that. Only
   * this row may pin it: a second pin makes coursier's SameVersion rule reject the graph. */
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
)

def testsJsSettings = Def.settings( // JS for tests
  scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
)

def testsNativeSettings = Def.settings( // Native for tests
  nativeConfig ~= { _.withMode(scalanative.build.Mode.debug).withLinkStubs(true) },
  allowUnsafeScalaLibUpgrade := true, // Scala Native needs a newer scala-library
)

lazy val tests = projectMatrix.in(file("tests")).settings(testsSettings)
  .crossJvm(TestedScalaVersions, testsJvmSettings).crossJs(TestedScalaVersions, testsJsSettings)
  .crossNative(TestedScalaVersions, testsNativeSettings).enablePlugins(BuildInfoPlugin)
  .dependsOn(scalameta, testkit)

def testsSemanticdbSettings = Def.settings(
  Test / exportJars := false,
  crossScalaVersions := AllScala2Versions,
  testSettings,
  jvmPlatformSettings,
  scalaVersion := LatestScala213,
  dependencyOverrides ++= // project switches to older scala library, so pin these artifacts
    Seq("scala-library", "scala-compiler", "scalap").map("org.scala-lang" % _ % scalaVersion.value),
  /* only this project uses coursier. On a Scala.js row sbt 2's %% asks for the Scala.js build of
   * coursier, which puts a second suffix of fastparse, geny and sourcecode on the classpath. */
  libraryDependencies += "io.get-coursier" %% "coursier" % "2.1.24" cross CrossVersion.for3Use2_13,
  Test / fullClasspath := Def.uncached {
    sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar") =
      semanticdbScalacPluginPackage.value
    (Test / fullClasspath).value
  },
  // Needed because some tests rely on the --usejavacp option
  Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat,
)

lazy val testsSemanticdb = project.in(file("tests-semanticdb")).dependsOn(
  scalameta.jvm(PublishedScala213),
  testkit.jvm(PublishedScala213),
  semanticdbScalacPlugin,
  semanticdbMetac,
  semanticdbMetacp,
  semanticdbMetap,
  semanticdbIntegration,
).settings(testsSemanticdbSettings).enablePlugins(BuildInfoPlugin)

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
      (common2.jvm(PublishedScala213) / Compile / classDirectory).value.getAbsolutePath,
      (common.jvm(PublishedScala213) / Compile / classDirectory).value.getAbsolutePath,
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
  crossScalaVersions := LatestScala2,
).dependsOn(scalameta.jvm(PublishedScala213))

/* ======================== BENCHES ======================== */
lazy val benchSemanticdb = project.in(file("bench/semanticdb")).enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin).settings(
    sharedJvmSettings,
    crossScalaVersions := LatestScala2,
    nonPublishableSettings,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    buildInfoKeys := Seq[BuildInfoKey]("sourceroot" -> (ThisBuild / baseDirectory).value),
    buildInfoPackage := "scala.meta.internal.bench",
    Jmh / run := Def.inputTaskDyn {
      val args = spaceDelimited("<arg>").parsed
      val buf = List.newBuilder[String]
      buf += "org.openjdk.jmh.Main"
      buf ++= args
      buf += "-p"
      buf += s"semanticdbScalacJar=${semanticdbScalacPluginPackage.value}"
      (Jmh / runMain).toTask(s"  ${buf.result().mkString(" ")}")
    }.evaluated,
  ).dependsOn(testsSemanticdb)

lazy val benchScalameta = project.in(file("bench/scalameta")).enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin).settings(
    sharedJvmSettings,
    crossScalaVersions := LatestScala2,
    nonPublishableSettings,
    buildInfoKeys := Seq[BuildInfoKey]("sourceroot" -> (ThisBuild / baseDirectory).value),
    buildInfoPackage := "scala.meta.internal.bench",
    Jmh / resourceDirectory := (Compile / resourceDirectory).value,
    // two Append instances match a bare Classpath, so name the type
    Jmh / fullClasspath ++=
      { (scalameta.jvm(PublishedScala213) / Compile / fullClasspath).value: Classpath },
    Jmh / run := Def.inputTaskDyn {
      val buf = List.newBuilder[String]
      buf += "org.openjdk.jmh.Main"
      buf ++= spaceDelimited("<arg>").parsed
      (Jmh / runMain).toTask(s"  ${buf.result().mkString(" ")}")
    }.evaluated,
  ).dependsOn(scalameta.jvm(PublishedScala213))

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
    if (!isScala212.value) Nil
    else List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  },
  scalacOptions ++= { if (isScala213.value) List("-Ymacro-annotations") else Nil },
  scalacOptions ++= { if (isScala212.value) Nil else List("-Xfatal-warnings") },
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

def copyAssemblyJar = Def.task {
  val fatJar = crossTarget.value / (assembly / assemblyJarName).value
  val _ = assembly.value
  fileOf.value.andThen(slimJar => IO.copy(List(fatJar -> slimJar), CopyOptions().withOverwrite(true)))
}

lazy val mergeSettings = Def.settings(
  sharedJvmSettings,
  // sbt-assembly's shade rules fail on an exported jar
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
  Compile / Keys.`package` := Def.uncached {
    val slimJar = (Compile / Keys.`package`).value
    copyAssemblyJar.value(slimJar)
    slimJar
  },
  Compile / packageBin / packagedArtifact := Def.uncached {
    val (art, slimJar) = (Compile / packageBin / packagedArtifact).value
    copyAssemblyJar.value(slimJar)
    (art, slimJar)
  },
  assembly / assemblyMergeStrategy := {
    case PathList("com", "sun", _*) => MergeStrategy.discard
    case PathList("sun", _*) => MergeStrategy.discard
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
  mimaCurrentClassfiles := fileOf.value((Compile / Keys.`package`).value),
)

// for SIP-51, the newest ScalaPB built against the earliest Scala 2.13.x we support
def scalapbVersion = Def.setting(if (scalaVersion.value == "2.13.15") "0.11.17" else "0.11.20")

lazy val protobufSettings = Def.settings(
  // sbt 2 puts managed sources in the sources jar already, and adding them duplicates the entries
  Compile / PB.targets := Seq {
    /* sbt-protoc loads the generator in a classloader of its own, so the meta-build needs no
     * scalapb compilerplugin, whose Scala 3 build wants another protoc-bridge than sbt-protoc */
    val artifact = protocbridge
      .Artifact("com.thesamet.scalapb", "compilerplugin_2.13", scalapbVersion.value)
    protocbridge.Target(
      generator = protocbridge.SandboxedJvmGenerator
        .forModule("scala", artifact, "scalapb.ScalaPbCodeGenerator$", Nil),
      outputPath = (Compile / sourceManaged).value / "protobuf",
      options = Seq("flat_package"), // what scalapb.gen(flatPackage = true) passes to protoc
    )
  },
  Compile / PB.protoSources := Seq(file("semanticdb/semanticdb/shared/src/main/proto")),
  PB.additionalDependencies := Nil,
  libraryDependencies ++= {
    val pbruntime = "com.thesamet.scalapb" %% "scalapb-runtime" % scalapbVersion.value
    Seq(pbruntime, pbruntime % "protobuf")
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

def exposePaths(projectName: String, config: Configuration) = Def.settings {
  val uncapitalizedName = {
    val chars = config.name.toCharArray
    if (chars.isEmpty) ""
    else {
      chars(0) = chars(0).toLower
      new String(chars)
    }
  }
  val prefix = Seq("sbt", "paths", projectName, uncapitalizedName).mkString("", ".", ".")
  // The tests run in the build's JVM, so a system property reaches them
  def setProp[A](label: String, value: A)(f: A => String): A = {
    System.setProperty(prefix + label, f(value))
    value
  }
  config / fullClasspath := Def.uncached {
    setProp("sources", (config / sourceDirectory).value)(_.getAbsolutePath)
    setProp("resources", (config / resourceDirectory).value)(_.getAbsolutePath)
    // resolvedScalacOptions expands ${CSR_CACHE} and the other roots to real paths
    setProp("options", (config / resolvedScalacOptions).value)(_.mkString(" "))
    val toFile = fileOf.value
    setProp("classes", (config / fullClasspath).value)(
      _.map(x => toFile(x.data).getAbsolutePath).mkString(java.io.File.pathSeparator),
    )
  }
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

def fileOf = Def.task(fileConverter.value.toPath(_: xsbti.HashedVirtualFileRef).toFile)

def semanticdbScalacPluginPackage = Def
  .task(fileOf.value((semanticdbScalacPlugin / Compile / Keys.`package`).value).getAbsolutePath)

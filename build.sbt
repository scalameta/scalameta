import java.io._
import scala.util.Try
import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import org.scalajs.sbtplugin.ScalaJSCrossVersion
import sbtcrossproject.{crossProject, CrossType}
import org.scalameta.build._
import org.scalameta.build.Versions._
import org.scalameta.os
import UnidocKeys._
import sbt.ScriptedPlugin._
import complete.DefaultParsers._
import scalapb.compiler.Version.scalapbVersion

lazy val LanguageVersions = Seq(LatestScala212, LatestScala211)
lazy val LanguageVersion = LanguageVersions.head

// ==========================================
// Projects
// ==========================================

sharedSettings
version.in(ThisBuild) ~= { old =>
  val suffix =
    if (sys.props.contains("scalameta.snapshot")) "-SNAPSHOT"
    else ""
  customVersion.getOrElse(old.replace('+', '-') + suffix)
}
name := {
  println(s"[info] Welcome to scalameta ${version.value}")
  val javaVersion = sys.props("java.specification.version")
  if (javaVersion != "1.8") sys.error(s"Obtained Java version $javaVersion. Expected 1.8")
  "scalametaRoot"
}
nonPublishableSettings
unidocSettings
addCommandAlias("benchAll", benchAll.command)
addCommandAlias("benchLSP", benchLSP.command)
addCommandAlias("benchQuick", benchQuick.command)
// ci-fast is not a CiCommand because `plz x.y.z test` is super slow,
// it runs `test` sequentially in every defined module.
commands += Command.command("ci-fast") { s =>
  s"wow $ciScalaVersion" ::
    ("tests" + ciPlatform + "/test") ::
    ci("doc") ::
    s
}
commands += Command.command("ci-native") { s =>
  "metapNative/nativeLink" ::
    "ci-fast" ::
    s
}
commands += CiCommand("ci-publish")(
  "publishSigned" :: Nil
)
commands += Command.command("mima") { s =>
  // MiMa is disabled until we have a 4.0.0-RC1 out.
  // s"very mimaReportBinaryIssues" ::
  s
}
commands += Command.command("ci-slow") { s =>
  val out = file("target/scala-library")
  if (!out.exists()) {
    IO.unzipURL(
      new URL(s"https://github.com/scala/scala/archive/v$LatestScala212.zip"),
      toDirectory = out,
      filter = s"scala-$LatestScala212/src/library/*"
    )
  }
  s"wow $ciScalaVersion" ::
    "testsJVM/test:runMain scala.meta.tests.semanticdb.MetacScalaLibrary" ::
    "testsJVM/slow:test" ::
    s
}
commands += Command.command("save-expect") { s =>
  "metapJVM/compile" ::
    "metacp/compile" ::
    "semanticdbScalacPlugin/compile" ::
    "semanticdbIntegration/clean" ::
    "semanticdbIntegration/compile" ::
    "testsJVM/test:runMain scala.meta.tests.semanticdb.SaveExpectTest" :: s
}
// NOTE: These tasks are aliased here in order to support running "tests/test"
// from a command. Running "test" inside a command will trigger the `test` task
// to run in all defined modules, including ones like inputs/io/dialects which
// have no tests.
test := {
  println(
    """Welcome to the scalameta build! This is a big project with lots of tests :)
      |Running "test" may take a really long time. Here are some other useful commands
      |that give a tighter edit/run/debug cycle.
      |- testsJVM/test      # Bread and butter tests
      |- testsJVM/slow:test # More thorough tests that take much longer to run
      |- testsJS/testQuick  # Ensure crosscompilability
      |- testkit/test       # Ensure additional reliability thanks to property tests
      |""".stripMargin
  )
}
packagedArtifacts := Map.empty
unidocProjectFilter.in(ScalaUnidoc, unidoc) := inAnyProject
console := console.in(scalametaJVM, Compile).value

/** ======================== SEMANTICDB ======================== **/
lazy val semanticdb3 = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("semanticdb/semanticdb3"))
  .settings(
    publishableSettings,
    protobufSettings,
    PB.protoSources.in(Compile) := Seq(file("semanticdb/semanticdb3"))
  )
  .nativeSettings(nativeSettings)
  .jvmSettings(
    crossScalaVersions := List(LatestScala211, LatestScala212)
  )
  .jsSettings(
    crossScalaVersions := List(LatestScala211, LatestScala212)
  )
lazy val semanticdb3JVM = semanticdb3.jvm
lazy val semanticdb3JS = semanticdb3.js
lazy val semanticdb3Native = semanticdb3.native

lazy val semanticdbScalacCore = project
  .in(file("semanticdb/scalac/library"))
  .settings(
    publishableSettings,
    fullCrossVersionSettings,
    moduleName := "semanticdb-scalac-core",
    description := "Library to generate SemanticDB from Scalac 2.x internal data structures",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  )
  .dependsOn(scalametaJVM, metacp)

lazy val semanticdbScalacPlugin = project
  .in(file("semanticdb/scalac/plugin"))
  .settings(
    moduleName := "semanticdb-scalac",
    description := "Scalac 2.x compiler plugin that generates SemanticDB on compile",
    publishableSettings,
    mergeSettings,
    fullCrossVersionSettings,
    pomPostProcess := { node =>
      new RuleTransformer(new RewriteRule {
        private def isAbsorbedDependency(node: XmlNode): Boolean = {
          def isArtifactId(node: XmlNode, fn: String => Boolean) =
            node.label == "artifactId" && fn(node.text)
          node.label == "dependency" && node.child.exists(child =>
            isArtifactId(child, _.startsWith("semanticdb-scalac-core")))
        }
        override def transform(node: XmlNode): XmlNodeSeq = node match {
          case e: Elem if isAbsorbedDependency(node) =>
            Comment("the dependency that was here has been absorbed via sbt-assembly")
          case _ => node
        }
      }).transform(node).head
    }
  )
  .dependsOn(semanticdbScalacCore)

lazy val cli = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("semanticdb/cli"))
  .settings(
    publishableSettings,
    description := "Shared CLI infrastructure for Scalameta tools"
  )
  .nativeSettings(nativeSettings)
lazy val cliJVM = cli.jvm
lazy val cliJS = cli.js
lazy val cliNative = cli.native

lazy val metac = project
  .in(file("semanticdb/metac"))
  .settings(
    publishableSettings,
    fullCrossVersionSettings,
    description := "Scalac 2.x launcher that generates SemanticDB on compile",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    mainClass := Some("scala.meta.cli.Metac")
  )
  // NOTE: workaround for https://github.com/sbt/sbt-core-next/issues/8
  .disablePlugins(BackgroundRunPlugin)
  .dependsOn(cliJVM, semanticdbScalacPlugin)

lazy val metacp = project
  .in(file("semanticdb/metacp"))
  .settings(
    publishableSettings,
    description := "Scala 2.x classpath to SemanticDB converter",
    libraryDependencies ++= List(
      "org.scala-lang" % "scalap" % scalaVersion.value,
      "io.github.soc" % "directories" % "5"
    ),
    mainClass := Some("scala.meta.cli.Metacp"),
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "scala.meta.internal.metacp"
  )
  .enablePlugins(BuildInfoPlugin)
  // NOTE: workaround for https://github.com/sbt/sbt-core-next/issues/8
  .disablePlugins(BackgroundRunPlugin)
  .dependsOn(semanticdb3JVM, cliJVM, ioJVM)

lazy val metap = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("semanticdb/metap"))
  .settings(
    publishableSettings,
    description := "SemanticDB decompiler",
    mainClass := Some("scala.meta.cli.Metap")
  )
  .nativeSettings(nativeSettings)
  // NOTE: workaround for https://github.com/sbt/sbt-core-next/issues/8
  .disablePlugins(BackgroundRunPlugin)
  .dependsOn(semanticdb3, cli)
lazy val metapJVM = metap.jvm
lazy val metapJS = metap.js
lazy val metapNative = metap.native

/** ======================== SCALAMETA ======================== **/
lazy val common = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/common"))
  .settings(
    publishableSettings,
    libraryDependencies += "com.lihaoyi" %%% "sourcecode" % "0.1.4",
    description := "Bag of private and public helpers used in scalameta APIs and implementations",
    enableMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(semanticdb3)
lazy val commonJVM = common.jvm
lazy val commonJS = common.js
lazy val commonNative = common.native

lazy val io = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/io"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for input/output"
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common)

lazy val ioJVM = io.jvm
lazy val ioJS = io.js
lazy val ioNative = io.native

lazy val dialects = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/dialects"))
  .settings(
    publishableSettings,
    description := "Scalameta dialects",
    enableMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common)
lazy val dialectsJVM = dialects.jvm
lazy val dialectsJS = dialects.js
lazy val dialectsNative = dialects.native

lazy val inputs = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/inputs"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for source code",
    enableMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common, io)
lazy val inputsJVM = inputs.jvm
lazy val inputsJS = inputs.js
lazy val inputsNative = inputs.native

lazy val interactive = project
  .in(file("scalameta/interactive"))
  .settings(
    publishableSettings,
    fullCrossVersionSettings,
    description := "Scalameta APIs for interactive building of SemanticDB",
    enableMacros
  )
  .dependsOn(semanticdbScalacCore)

lazy val parsers = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/parsers"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for parsing and their baseline implementation",
    scalaJSModuleKind := ModuleKind.CommonJSModule
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common, dialects, inputs, tokens, tokenizers, trees)
lazy val parsersJVM = parsers.jvm
lazy val parsersJS = parsers.js
lazy val parsersNative = parsers.native

lazy val quasiquotes = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/quasiquotes"))
  .settings(
    publishableSettings,
    description := "Scalameta quasiquotes for abstract syntax trees",
    enableHardcoreMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common, dialects, inputs, trees, parsers)
lazy val quasiquotesJVM = quasiquotes.jvm
lazy val quasiquotesJS = quasiquotes.js
lazy val quasiquotesNative = quasiquotes.native

lazy val tokenizers = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/tokenizers"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for tokenization and their baseline implementation",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0",
    enableMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common, dialects, inputs, tokens)
lazy val tokenizersJVM = tokenizers.jvm
lazy val tokenizersJS = tokenizers.js
lazy val tokenizersNative = tokenizers.native

lazy val tokens = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/tokens"))
  .settings(
    publishableSettings,
    description := "Scalameta tokens and token-based abstractions (inputs and positions)",
    enableMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common, dialects, inputs)
lazy val tokensJVM = tokens.jvm
lazy val tokensJS = tokens.js
lazy val tokensNative = tokens.native

lazy val transversers = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/transversers"))
  .settings(
    requiresMacrosSetting,
    publishableSettings,
    description := "Scalameta traversal and transformation infrastructure for abstract syntax trees",
    enableMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common, trees)
lazy val traversersJVM = transversers.jvm
lazy val traversersJS = transversers.js
lazy val traversersNative = transversers.native

lazy val trees = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/trees"))
  .settings(
    publishableSettings,
    description := "Scalameta abstract syntax trees",
    // NOTE: uncomment this to update ast.md
    // scalacOptions += "-Xprint:typer",
    enableMacros
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common, dialects, inputs, tokens, tokenizers) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty
lazy val treesJVM = trees.jvm
lazy val treesJS = trees.js
lazy val treesNative = trees.native

lazy val semanticdb = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/semanticdb"))
  .settings(
    publishableSettings,
    description := "Scalameta semantic database APIs"
  )
  .nativeSettings(nativeSettings)
  .dependsOn(common)
lazy val semanticdbJVM = semanticdb.jvm
lazy val semanticdbJS = semanticdb.js
lazy val semanticdbNative = semanticdb.native

lazy val scalameta = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/scalameta"))
  .settings(
    publishableSettings,
    description := "Scalameta umbrella module that includes all public APIs"
  )
  .nativeSettings(nativeSettings)
  .dependsOn(
    common,
    dialects,
    parsers,
    quasiquotes,
    tokenizers,
    transversers,
    trees,
    inputs,
    io,
    semanticdb
  )
lazy val scalametaJVM = scalameta.jvm
lazy val scalametaJS = scalameta.js
lazy val scalametaNative = scalameta.native

lazy val contrib = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("scalameta/contrib"))
  .settings(
    publishableSettings,
    description := "Incubator for Scalameta APIs"
  )
  .nativeSettings(nativeSettings)
  .dependsOn(scalameta)
lazy val contribJVM = contrib.jvm
lazy val contribJS = contrib.js
lazy val contribNative = contrib.native

/** ======================== TESTS ======================== **/
lazy val semanticdbIntegration = project
  .in(file("semanticdb/integration"))
  .settings(
    description := "Sources to compile to build SemanticDB for tests.",
    sharedSettings,
    nonPublishableSettings,
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions ++= {
      val pluginJar = Keys.`package`.in(semanticdbScalacPlugin, Compile).value.getAbsolutePath
      Seq(
        s"-Xplugin:$pluginJar",
        s"-Xplugin-require:semanticdb",
        s"-Ywarn-unused-import",
        s"-Yrangepos",
        s"-P:semanticdb:failures:error", // fail fast during development.
        s"-P:semanticdb:exclude:Exclude.scala",
        s"-P:semanticdb:sourceroot:${baseDirectory.in(ThisBuild).value}",
        s"-P:semanticdb:synthetics:on"
      )
    },
    javacOptions += "-parameters"
  )

lazy val testkit = project
  .in(file("scalameta/testkit"))
  .settings(
    publishableSettings,
    hasLargeIntegrationTests,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.0-SNAP10",
      "com.lihaoyi" %% "geny" % "0.1.1",
      // These are used to download and extract a corpus tar.gz
      "org.rauschig" % "jarchivelib" % "0.7.1",
      "commons-io" % "commons-io" % "2.5",
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
    ),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
    description := "Testing utilities for scalameta APIs"
  )
  .dependsOn(contribJVM)

lazy val tests = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("tests"))
  .configs(Slow)
  .settings(
    sharedSettings,
    nonPublishableSettings,
    description := "Tests for scalameta APIs",
    exposePaths("tests", Test),
    fullClasspath.in(Test) := {
      val semanticdbScalacJar =
        Keys.`package`.in(semanticdbScalacPlugin, Compile).value.getAbsolutePath
      sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar") = semanticdbScalacJar
      fullClasspath.in(Test).value
    },
    buildInfoKeys := Seq[BuildInfoKey](
      scalaVersion,
      "databaseSourcepath" -> baseDirectory.in(ThisBuild).value.getAbsolutePath,
      "databaseClasspath" -> classDirectory.in(semanticdbIntegration, Compile).value.getAbsolutePath
    ),
    buildInfoPackage := "scala.meta.tests",
    libraryDependencies += "com.lihaoyi" %%% "fansi" % "0.2.5" % "test",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.0-SNAP10" % "test",
    testOptions.in(Test) += Tests.Argument("-l", "org.scalatest.tags.Slow"),
    inConfig(Slow)(Defaults.testTasks),
    testOptions.in(Slow) -= Tests.Argument("-l", "org.scalatest.tags.Slow"),
    testOptions.in(Slow) += Tests.Argument("-n", "org.scalatest.tags.Slow")
  )
  .jvmSettings(
    // FIXME: https://github.com/scalatest/scalatest/issues/1112
    // Without adding scalacheck to library dependencies, we get the following error:
    // > testsJVM/test
    // [info] Compiling 79 Scala sources to /Users/eburmako/Projects/scalameta/tests/jvm/target/scala-2.12/test-classes...
    // [trace] Stack trace suppressed: run last testsJVM/test:executeTests for the full output.
    // [error] (testsJVM/test:executeTests) java.lang.NoClassDefFoundError: org/scalacheck/Test$TestCallback
    // [error] Total time: 19 s, completed Feb 1, 2018 3:12:34 PM
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.5",
    libraryDependencies ++= List(
      "io.get-coursier" %% "coursier" % coursier.util.Properties.version,
      "io.get-coursier" %% "coursier-cache" % coursier.util.Properties.version
    )
  )
  .jvmConfigure(_.dependsOn(testkit, interactive, metac, metacp, semanticdbIntegration))
  .nativeSettings(
    nativeSettings,
    // FIXME: https://github.com/scalatest/scalatest/issues/1112
    // discussion: https://github.com/scalameta/scalameta/pull/1243/files#r165529377
    // [error] cannot link: @java.lang.Thread::getStackTrace_scala.scalanative.runtime.ObjectArray
    // [error] unable to link
    nativeLinkStubs := true,
    nativeMode := "debug"
  )
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(scalameta, contrib, metap)
lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js
lazy val testsNative = tests.native

/** ======================== BENCHES ======================== **/
lazy val bench = project
  .in(file("bench/suite"))
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JmhPlugin)
  .settings(
    sharedSettings,
    nonPublishableSettings,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    buildInfoKeys := Seq[BuildInfoKey](
      "sourceroot" -> (baseDirectory in ThisBuild).value
    ),
    buildInfoPackage := "scala.meta.internal.bench",
    run.in(Jmh) := (Def.inputTaskDyn {
      val args = spaceDelimited("<arg>").parsed
      val semanticdbScalacJar =
        Keys.`package`.in(semanticdbScalacPlugin, Compile).value.getAbsolutePath
      val buf = List.newBuilder[String]
      buf += "org.openjdk.jmh.Main"
      buf ++= args
      buf += "-p"
      buf += s"semanticdbScalacJar=$semanticdbScalacJar"
      runMain.in(Jmh).toTask(s"  ${buf.result.mkString(" ")}")
    }).evaluated
  )
  .dependsOn(testsJVM)

// ==========================================
// Settings
// ==========================================

lazy val requiresMacrosSetting = Def.settings(
  scalacOptions += {
    val base = file("scalameta/common/shared/src/main/scala")
    val filesWithWhiteboxMacros =
      List(
        "org/scalameta" -> List(
          "adt/Adt.scala",
          "adt/Liftables.scala",
          "data/data.scala",
          "data/Macros.scala",
          "explore/Macros.scala",
          "internal/DebugFinder.scala",
          "internal/FreeLocalFinder.scala",
          "internal/ImplTransformers.scala",
          "internal/MacroHelpers.scala",
          "invariants/package.scala",
          "package.scala"
        ),
        "scala/meta/internal" -> List(
          "classifiers/Macros.scala",
          "prettyprinters/ShowMacros.scala",
          "tokens/root.scala",
          "tokens/token.scala",
          "transversers/transformer.scala",
          "transversers/transverser.scala",
          "transversers/traverser.scala",
          "trees/ast.scala",
          "trees/branch.scala",
          "trees/Liftables.scala",
          "trees/NamerMacros.scala",
          "trees/quasiquote.scala",
          "trees/registry.scala",
          "trees/root.scala",
          "trees/TyperMacros.scala"
        )
      )

    val flat = filesWithWhiteboxMacros.flatMap {
      case (k, vs) => vs.map(v => (base / k / v).lastModified)
    }

    "-J" + flat.hashCode
  }
)

lazy val sharedSettings = Def.settings(
  scalaVersion := LanguageVersion,
  crossScalaVersions := LanguageVersions,
  organization := "org.scalameta",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  scalacOptions.in(Compile, doc) ++= Seq("-skip-packages", ""),
  scalacOptions.in(Compile, doc) ++= Seq("-implicits", "-implicits-hide:."),
  scalacOptions.in(Compile, doc) ++= Seq("-groups"),
  scalacOptions ++= Seq("-Xfatal-warnings"),
  parallelExecution.in(Test) := false, // hello, reflection sync!!
  logBuffered := false,
  updateOptions := updateOptions.value.withCachedResolution(true),
  triggeredMessage.in(ThisBuild) := Watched.clearWhenTriggered,
  incOptions := incOptions.value.withLogRecompileOnMacro(false)
)

lazy val mergeSettings = Def.settings(
  sharedSettings,
  test.in(assembly) := {},
  logLevel.in(assembly) := Level.Error,
  assemblyJarName.in(assembly) :=
    name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
  assemblyOption.in(assembly) ~= { _.copy(includeScala = false) },
  Keys.`package`.in(Compile) := {
    val slimJar = Keys.`package`.in(Compile).value
    val fatJar =
      new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), overwrite = true)
    slimJar
  },
  packagedArtifact.in(Compile).in(packageBin) := {
    val temp = packagedArtifact.in(Compile).in(packageBin).value
    val (art, slimJar) = temp
    val fatJar =
      new File(crossTarget.value + "/" + assemblyJarName.in(assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), overwrite = true)
    (art, slimJar)
  },
  mimaCurrentClassfiles := {
    Keys.`package`.in(Compile).value
  }
)

lazy val protobufSettings = Def.settings(
  sharedSettings,
  PB.targets.in(Compile) := Seq(
    // scalapb.gen(
    //   flatPackage = true // Don't append filename to package
    // ) -> sourceManaged.in(Compile).value
  ),
  PB.runProtoc in Compile := {
    val isNixOS = sys.props.get("java.home").map(_.startsWith("/nix/store")).getOrElse(false)
    if (isNixOS) {
      // must have protoc installed
      // nix-env -i protobuf-3.3.0
      (args => Process("protoc", args) !)
    } else {
      (PB.runProtoc in Compile).value
    }
  },
  libraryDependencies += "com.thesamet.scalapb" %%% "scalapb-runtime" % scalapbVersion
)

lazy val adhocRepoUri = sys.props("scalameta.repository.uri")
lazy val adhocRepoCredentials = sys.props("scalameta.repository.credentials")
lazy val isCustomRepository = adhocRepoUri != null && adhocRepoCredentials != null

lazy val publishableSettings = Def.settings(
  SettingKey[Boolean]("ide-skip-project") :=
    platformDepsCrossVersion.value == ScalaNativeCrossVersion.binary,
  publishTo := Some {
    if (isCustomRepository) "adhoc" at adhocRepoUri
    // NOTE: isSnapshot.value does not work with sbt-dynver
    else if (version.value.endsWith("SNAPSHOT")) Opts.resolver.sonatypeSnapshots
    else Opts.resolver.sonatypeStaging
  },
  credentials ++= {
    val credentialsFile = if (adhocRepoCredentials != null) new File(adhocRepoCredentials) else null
    if (credentialsFile != null) List(new FileCredentials(credentialsFile))
    else Nil
  },
  sharedSettings,
  publishArtifact.in(Compile) := true,
  publishArtifact.in(Test) := false,
  publishMavenStyle := true,
  pomIncludeRepository := { x =>
    false
  },
  mimaPreviousArtifacts := {
    if (organization.value == "org.scalameta") {
      val rxVersion = """^(\d+)\.(\d+)\.(\d+)(?:-([0-9A-Za-z-]+))?$""".r
      val previousVersion = version.value match {
        case rxVersion(major, minor, patch, suffix) if suffix != null =>
          Some(s"$major.$minor.$patch")
        case rxVersion(major, "0", "0", null) =>
          None
        case rxVersion(major, minor, "0", null) =>
          Some(s"$major.${minor.toInt - 1}.0")
        case rxVersion(major, minor, patch, null) =>
          Some(s"$major.$minor.0")
        case _ =>
          sys.error(s"Invalid version number: ${version.value}")
      }
      val previousArtifact = {
        // NOTE: Here's what I'd like to do, but I can't because of deprecations:
        //   val isJVM = crossPlatform.value == JVMPlatform
        // Here's my second best guess, but it doesn't work due to some reason:
        //   val isJVM = platformDepsCrossVersion.value == CrossVersion.binary
        val isJVM = {
          val isJS = platformDepsCrossVersion.value == ScalaJSCrossVersion.binary
          val isNative = platformDepsCrossVersion.value == ScalaNativeCrossVersion.binary
          !isJS && !isNative
        }
        if (isJVM) {
          previousVersion.map { previousVersion =>
            organization.value % moduleName.value % previousVersion cross crossVersion.value
          }
        } else {
          None
        }
      }
      previousArtifact.toSet
    } else {
      Set()
    }
  },
  mimaBinaryIssueFilters += Mima.languageAgnosticCompatibilityPolicy,
  mimaBinaryIssueFilters += Mima.scalaSpecificCompatibilityPolicy,
  mimaBinaryIssueFilters ++= Mima.apiCompatibilityExceptions,
  licenses += "BSD" -> url("https://github.com/scalameta/scalameta/blob/master/LICENSE.md"),
  pomExtra := (
    <url>https://github.com/scalameta/scalameta</url>
    <inceptionYear>2014</inceptionYear>
    <scm>
      <url>git://github.com/scalameta/scalameta.git</url>
      <connection>scm:git:git://github.com/scalameta/scalameta.git</connection>
    </scm>
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
        <id>densh</id>
        <name>Denys Shabalin</name>
        <url>http://den.sh</url>
      </developer>
      <developer>
        <id>mutcianm</id>
        <name>Mikhail Mutcianko</name>
        <url>https://github.com/mutcianm</url>
      </developer>
      <developer>
        <id>olafurpg</id>
        <name>Ólafur Páll Geirsson</name>
        <url>https://geirsson.com/</url>
      </developer>
      <developer>
        <id>DavidDudson</id>
        <name>David Dudson</name>
        <url>https://daviddudson.github.io/</url>
      </developer>
      <developer>
        <id>gabro</id>
        <name>Gabriele Petronella</name>
        <url>http://buildo.io</url>
      </developer>
    </developers>
  )
)

lazy val nonPublishableSettings = Seq(
  mimaPreviousArtifacts := Set.empty,
  publishArtifact in (Compile, packageDoc) := false,
  publishArtifact in packageDoc := false,
  sources in (Compile, doc) := Seq.empty,
  publishArtifact := false,
  PgpKeys.publishSigned := {},
  publish := {}
)

def compatibilityPolicyViolation(ticket: String) = Seq(
  mimaPreviousArtifacts := Set.empty
)

lazy val fullCrossVersionSettings = Seq(
  crossVersion := CrossVersion.full,
  unmanagedSourceDirectories.in(Compile) += {
    // NOTE: sbt 0.13.8 provides cross-version support for Scala sources
    // (http://www.scala-sbt.org/0.13/docs/sbt-0.13-Tech-Previews.html#Cross-version+support+for+Scala+sources).
    // Unfortunately, it only includes directories like "scala_2.11" or "scala_2.12",
    // not "scala_2.11.8" or "scala_2.12.1" that we need.
    // That's why we have to work around here.
    val base = sourceDirectory.in(Compile).value
    base / ("scala-" + scalaVersion.value)
  }
)

lazy val hasLargeIntegrationTests = Seq(
  fork in (Test, run) := true,
  javaOptions in (Test, run) += "-Xss4m"
)

lazy val nativeSettings = Seq(
  SettingKey[Boolean]("ide-skip-project") := true,
  scalaVersion := LatestScala211,
  crossScalaVersions := List(LatestScala211),
  scalacOptions -= "-Xfatal-warnings",
  nativeGC := "immix",
  nativeMode := "release",
  nativeLinkStubs := false
)

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
    sourceDirectory.in(config) := {
      val defaultValue = sourceDirectory.in(config).value
      System.setProperty(prefix + "sources", defaultValue.getAbsolutePath)
      defaultValue
    },
    resourceDirectory.in(config) := {
      val defaultValue = resourceDirectory.in(config).value
      System.setProperty(prefix + "resources", defaultValue.getAbsolutePath)
      defaultValue
    },
    fullClasspath.in(config) := {
      val defaultValue = fullClasspath.in(config).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      System.setProperty(prefix + "classes", classpath.mkString(java.io.File.pathSeparator))
      defaultValue
    }
  )
}

lazy val enableMacros = macroDependencies(hardcore = false)

lazy val enableHardcoreMacros = macroDependencies(hardcore = true)

def macroDependencies(hardcore: Boolean) = libraryDependencies ++= {
  val scalaReflect =
    Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided")
  val scalaCompiler = {
    if (hardcore)
      Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided")
    else Nil
  }
  scalaReflect ++ scalaCompiler
}

lazy val isTagPush = sys.env.get("TRAVIS_TAG").exists(_.nonEmpty)
lazy val isCiPublish = sys.env.contains("CI_PUBLISH")
lazy val ciPlatform =
  if (sys.env.contains("CI_SCALA_JS")) "JS"
  else if (sys.env.contains("CI_SCALA_NATIVE")) "Native"
  else "JVM"
lazy val ciScalaVersion = sys.env("CI_SCALA_VERSION")
def CiCommand(name: String)(commands: List[String]): Command = Command.command(name) { initState =>
  commands.foldLeft(initState) {
    case (state, command) => ci(command) :: state
  }
}
def ci(command: String) = s"plz $ciScalaVersion $command"
def customVersion = sys.props.get("scalameta.version")

// Defining these here so it's only defined once and for all projects (including root)
inScope(Global)(
  Seq(
    credentials ++= (for {
      username <- sys.env.get("SONATYPE_USERNAME")
      password <- sys.env.get("SONATYPE_PASSWORD")
    } yield
      Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq,
    PgpKeys.pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toCharArray())
  )
)

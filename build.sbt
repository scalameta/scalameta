import java.io._
import scala.util.Try
import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import org.scalajs.sbtplugin.ScalaJSCrossVersion
import org.scalameta.os
import UnidocKeys._
import sbt.ScriptedPlugin._
import com.trueaccord.scalapb.compiler.Version.scalapbVersion
import Versions._

lazy val LanguageVersions = Seq(LatestScala212, LatestScala211)
lazy val LanguageVersion = LanguageVersions.head

// ==========================================
// Projects
// ==========================================

sharedSettings
name := {
  println(s"[info] Welcome to scalameta ${version.value}")
  "scalametaRoot"
}
nonPublishableSettings
unidocSettings
// ci-fast is not a CiCommand because `plz x.y.z test` is super slow,
// it runs `test` sequentially in every defined module.
commands += Command.command("ci-fast") { s =>
  s"wow $ciScalaVersion" ::
    ("testOnly" + ciPlatform) ::
    ci("doc") :: // skips 2.10 projects
    s
}
commands += CiCommand("ci-slow")(
  "testkit/test:runMain scala.meta.testkit.ScalametaParserPropertyTest" ::
  Nil
)
commands += CiCommand("ci-publish")(
  if (isCiPublish && isTagPush) s"publishSigned" :: Nil
  else if (isCiPublish) s"publish" :: Nil
  else Nil
)
// NOTE: These tasks are aliased here in order to support running "tests/test"
// from a command. Running "test" inside a command will trigger the `test` task
// to run in all defined modules, including ones like inputs/io/dialects which
// have no tests.
test := {
  println(
    """Welcome to the scalameta build! This is a big project with lots of tests :)
      |Running "test" may take a really long time. Here are some other useful commands
      |that give a tighter edit/run/debug cycle.
      |- scalametaJVM/testQuick # Parser/Pretty-printer/Trees/...
      |- contribJVM/testQuick   # Contrib
      |- semanticdbScalac/test  # Semanticdb implementation for Scalac
      |- testOnlyJVM
      |- testOnlyJS
      |""".stripMargin)
}
testAll := {
  testOnlyJVM.value
  testOnlyJS.value
}
// These tasks skip over modules with no tests, like dialects/inputs/io, speeding up
// edit/test cycles. You may prefer to run testJVM while iterating on a design
// because JVM tests link and run faster than JS tests.
testOnlyJVM := {
  val runScalametaTests = test.in(scalametaJVM, Test).value
  val runSemanticdbScalacTests = test.in(semanticdbScalac, Test).value
  val runBenchmarkTests = test.in(benchmarks, Test).value
  val runContribTests = test.in(contribJVM, Test).value
  val runTests = test.in(testsJVM, Test).value
  val propertyTests = compile.in(testkit, Test).value
  val runDocs = test.in(readme).value
}
testOnlyJS := {
  val runScalametaTests = test.in(scalametaJS, Test).value
  val runContribTests = test.in(contribJS, Test).value
  val runTests = test.in(testsJS, Test).value
  val runParsersTest = test.in(parsersJS, Test).value
}
packagedArtifacts := Map.empty
unidocProjectFilter.in(ScalaUnidoc, unidoc) := inAnyProject
console := console.in(scalametaJVM, Compile).value

/** ======================== LANGMETA ======================== **/

lazy val langmetaIo = crossProject
  .in(file("langmeta/io"))
  .settings(
    publishableSettings,
    moduleName := "langmeta-io",
    description := "Langmeta APIs for input/output"
  )

lazy val langmetaIoJVM = langmetaIo.jvm
lazy val langmetaIoJs = langmetaIo.js

lazy val langmetaInputs = crossProject
  .in(file("langmeta/inputs"))
  .settings(
    publishableSettings,
    moduleName := "langmeta-inputs",
    description := "Langmeta APIs for source code"
  )
  .dependsOn(langmetaIo)
lazy val langmetaInputsJVM = langmetaInputs.jvm
lazy val langmetaInputsJS = langmetaInputs.js

lazy val langmetaSemanticdb = crossProject
  .in(file("langmeta/semanticdb"))
  .settings(
    publishableSettings,
    moduleName := "langmeta-semanticdb",
    description := "Semantic database APIs",
    // Protobuf setup for binary serialization.
    PB.targets.in(Compile) := Seq(
      scalapb.gen(
        flatPackage = true // Don't append filename to package
      ) -> sourceManaged.in(Compile).value
    ),
    PB.protoSources.in(Compile) := Seq(file("langmeta/semanticdb/shared/src/main/protobuf")),
    libraryDependencies += "com.trueaccord.scalapb" %%% "scalapb-runtime" % scalapbVersion
  )
  .dependsOn(langmetaIo, langmetaInputs)
lazy val langmetaSemanticdbJVM = langmetaSemanticdb.jvm
lazy val langmetaSemanticdbJS = langmetaSemanticdb.js

lazy val langmeta = crossProject
  .in(file("langmeta/langmeta"))
  .settings(
    publishableSettings,
    description := "Langmeta umbrella module that includes all public APIs",
    exposePaths("langmeta", Test)
  )
  .dependsOn(
    langmetaInputs,
    langmetaIo,
    langmetaSemanticdb
  )
lazy val langmetaJVM = langmeta.jvm
lazy val langmetaJS = langmeta.js

/** ======================== SCALAMETA ======================== **/

lazy val common = crossProject
  .in(file("scalameta/common"))
  .settings(
    publishableSettings,
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3",
    description := "Bag of private and public helpers used in scalameta APIs and implementations",
    enableMacros
  )
lazy val commonJVM = common.jvm
lazy val commonJS = common.js

lazy val io = crossProject
  .in(file("scalameta/io"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for input/output"
  )
  .dependsOn(langmetaIo, common)

lazy val ioJVM = io.jvm
lazy val ioJS = io.js

lazy val dialects = crossProject
  .in(file("scalameta/dialects"))
  .settings(
    publishableSettings,
    description := "Scalameta dialects",
    enableMacros
  )
  .dependsOn(common)
lazy val dialectsJVM = dialects.jvm
lazy val dialectsJS = dialects.js

lazy val inputs = crossProject
  .in(file("scalameta/inputs"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for source code",
    enableMacros
  )
  .dependsOn(langmetaInputs, common, io)
lazy val inputsJVM = inputs.jvm
lazy val inputsJS = inputs.js

lazy val parsers = crossProject
  .in(file("scalameta/parsers"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for parsing and their baseline implementation",
    scalaJSModuleKind := ModuleKind.CommonJSModule
  )
  .dependsOn(common, dialects, inputs, tokens, tokenizers, trees)
lazy val parsersJVM = parsers.jvm
lazy val parsersJS = parsers.js

lazy val quasiquotes = crossProject
  .in(file("scalameta/quasiquotes"))
  .settings(
    publishableSettings,
    description := "Scalameta quasiquotes for abstract syntax trees",
    enableHardcoreMacros
  )
  .dependsOn(common, dialects, inputs, trees, parsers)
lazy val quasiquotesJVM = quasiquotes.jvm
lazy val quasiquotesJS = quasiquotes.js

lazy val tokenizers = crossProject
  .in(file("scalameta/tokenizers"))
  .settings(
    publishableSettings,
    description := "Scalameta APIs for tokenization and their baseline implementation",
    libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.4.3",
    enableMacros
  )
  .dependsOn(common, dialects, inputs, tokens)
lazy val tokenizersJVM = tokenizers.jvm
lazy val tokenizersJS = tokenizers.js

lazy val tokens = crossProject
  .in(file("scalameta/tokens"))
  .settings(
    publishableSettings,
    description := "Scalameta tokens and token-based abstractions (inputs and positions)",
    enableMacros
  )
  .dependsOn(common, dialects, inputs)
lazy val tokensJVM = tokens.jvm
lazy val tokensJS = tokens.js

lazy val transversers = crossProject
  .in(file("scalameta/transversers"))
  .settings(
    publishableSettings,
    description := "Scalameta traversal and transformation infrastructure for abstract syntax trees",
    enableMacros
  )
  .dependsOn(common, trees)
lazy val traversersJVM = transversers.jvm
lazy val traversersJS = transversers.js

lazy val trees = crossProject
  .in(file("scalameta/trees"))
  .settings(
    publishableSettings,
    description := "Scalameta abstract syntax trees",
    // NOTE: uncomment this to update ast.md
    // scalacOptions += "-Xprint:typer",
    enableMacros
  )
  .dependsOn(common, dialects, inputs, tokens, tokenizers) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty
lazy val treesJVM = trees.jvm
lazy val treesJS = trees.js

lazy val semanticdb = crossProject
  .in(file("scalameta/semanticdb"))
  .settings(
    publishableSettings,
    description := "Scalameta semantic database APIs"
  )
  .dependsOn(langmetaSemanticdb)
lazy val semanticdbJVM = semanticdb.jvm
lazy val semanticdbJS = semanticdb.js

lazy val scalameta = crossProject
  .in(file("scalameta/scalameta"))
  .settings(
    publishableSettings,
    description := "Scalameta umbrella module that includes all public APIs",
    exposePaths("scalameta", Test)
  )
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

lazy val semanticdbScalac = project
  .in(file("scalameta/semanticdb-scalac"))
  .settings(
    moduleName := "semanticdb-scalac",
    description := "Scala 2.x compiler plugin that generates semanticdb on compile",
    publishableSettings,
    mergeSettings,
    isFullCrossVersion,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    exposePaths("semanticdb-scalac", Test),
    pomPostProcess := { node =>
      new RuleTransformer(new RewriteRule {
        private def isAbsorbedDependency(node: XmlNode): Boolean = {
          def isArtifactId(node: XmlNode, fn: String => Boolean) =
            node.label == "artifactId" && fn(node.text)
          node.label == "dependency" && node.child.exists(child =>
            isArtifactId(child, _.startsWith("langmeta-")) ||
            isArtifactId(child, _.startsWith("scalameta_")))
        }
        override def transform(node: XmlNode): XmlNodeSeq = node match {
          case e: Elem if isAbsorbedDependency(node) =>
            Comment("this dependency has been absorbed via sbt-assembly")
          case _ => node
        }
      }).transform(node).head
    }
  )
  .dependsOn(scalametaJVM, testkit % Test)

lazy val semanticdbIntegration = project
  .in(file("scalameta/semanticdb-integration"))
  .settings(
    description := "Sources to compile to build a semanticdb for tests.",
    sharedSettings,
    nonPublishableSettings,
    scalacOptions ++= {
      val pluginJar = Keys.`package`.in(semanticdbScalac, Compile).value.getAbsolutePath
      Seq(
        s"-Xplugin:$pluginJar",
        "-Yrangepos",
        s"-P:semanticdb:sourceroot:${baseDirectory.in(ThisBuild).value}",
        s"-P:semanticdb:failures:error", // fail fast during development.
        "-Xplugin-require:semanticdb"
      )
    }
  )

lazy val testkit = Project(id = "testkit", base = file("scalameta/testkit"))
  .settings(
    publishableSettings,
    hasLargeIntegrationTests,
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.0.1",
      "com.lihaoyi" %% "geny" % "0.1.1",
      // These are used to download and extract a corpus tar.gz
      "org.rauschig" % "jarchivelib" % "0.7.1",
      "commons-io" % "commons-io" % "2.5",
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0"
    ),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
    description := "Testing utilities for scalameta APIs"
  )
  .dependsOn(scalametaJVM)

lazy val tests = crossProject
  .in(file("scalameta/tests"))
  .settings(
    sharedSettings,
    nonPublishableSettings,
    description := "Tests for scalameta APIs",
    compile.in(Test) := compile.in(Test).dependsOn(compile.in(semanticdbIntegration, Compile)).value,
    buildInfoKeys := Seq[BuildInfoKey](
      scalaVersion,
      "databaseSourcepath" -> baseDirectory.in(ThisBuild).value.getAbsolutePath,
      "databaseClasspath" -> classDirectory.in(semanticdbIntegration, Compile).value.getAbsolutePath
    ),
    buildInfoPackage := "scala.meta.tests"
  )
  .jvmConfigure(_.dependsOn(testkit))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(scalameta, contrib)
lazy val testsJVM = tests.jvm
lazy val testsJS = tests.js
lazy val testOnlyJVM = taskKey[Unit]("Run JVM tests")
lazy val testOnlyJS = taskKey[Unit]("Run Scala.js tests")
lazy val testAll = taskKey[Unit]("Run JVM and Scala.js tests")

lazy val contrib = crossProject
  .in(file("scalameta/contrib"))
  .settings(
    publishableSettings,
    description := "Incubator for scalameta APIs"
  )
  .jvmConfigure(_.dependsOn(testkit % Test))
  .dependsOn(scalameta)
lazy val contribJVM = contrib.jvm
lazy val contribJS = contrib.js

lazy val benchmarks =
  Project(id = "benchmarks", base = file("scalameta/benchmarks"))
    .settings(
      sharedSettings,
      nonPublishableSettings,
      resourceDirectory.in(Jmh) := resourceDirectory.in(Compile).value,
      javaOptions.in(run) ++= Seq(
        "-Djava.net.preferIPv4Stack=true",
        "-XX:+AggressiveOpts",
        "-XX:+UseParNewGC",
        "-XX:+UseConcMarkSweepGC",
        "-XX:+CMSParallelRemarkEnabled",
        "-XX:+CMSClassUnloadingEnabled",
        "-XX:ReservedCodeCacheSize=128m",
        "-XX:MaxPermSize=1024m",
        "-Xss8M",
        "-Xms512M",
        "-XX:SurvivorRatio=128",
        "-XX:MaxTenuringThreshold=0",
        "-Xss8M",
        "-Xms512M",
        "-Xmx2G",
        "-server"
      )
    )
    .dependsOn(scalametaJVM)
    .enablePlugins(JmhPlugin)

lazy val readme = scalatex
  .ScalatexReadme(
    projectId = "readme",
    wd = file(""),
    url = "https://github.com/scalameta/scalameta/tree/master",
    source = "Readme"
  )
  .settings(
    sharedSettings,
    nonPublishableSettings,
    buildInfoSettings,
    // only needed for scalatex 0.3.8-pre until next scalatex release
    exposePaths("readme", Runtime),
    crossScalaVersions := LanguageVersions, // No need to cross-build.
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    sources.in(Compile) ++= List("os.scala").map(f => baseDirectory.value / "../project" / f),
    watchSources ++= baseDirectory.value.listFiles.toList,
    test := run.in(Compile).toTask(" --validate").value,
    publish := (if (!isCiPublish) () else {
      // generate the scalatex readme into `website`
      val website =
        new File(target.value.getAbsolutePath + File.separator + "scalatex")
      if (website.exists) website.delete
      val _ = run.in(Compile).toTask(" --validate").value
      if (!website.exists) sys.error("failed to generate the scalatex website")

      // import the scalatex readme into `repo`
      val repo = new File(os.temp.mkdir.getAbsolutePath + File.separator + "scalameta.org")
      os.shell.call(s"git clone https://github.com/scalameta/scalameta.github.com ${repo.getAbsolutePath}")
      println(s"erasing everything in ${repo.getAbsolutePath}...")
      repo.listFiles.filter(f => f.getName != ".git").foreach(os.shutil.rmtree)
      println(s"importing website from ${website.getAbsolutePath} to ${repo.getAbsolutePath}...")
      new PrintWriter(new File(repo.getAbsolutePath + File.separator + "CNAME")) {
        write("scalameta.org"); close
      }
      website.listFiles.foreach(src =>
        os.shutil.copytree(src, new File(repo.getAbsolutePath + File.separator + src.getName)))

      // commit and push the changes if any
      os.shell.call(s"git add -A", cwd = repo.getAbsolutePath)
      val nothingToCommit = "nothing to commit, working directory clean"
      try {
        val url = "https://github.com/scalameta/scalameta/tree/" + os.git.currentSha()
        os.shell.call(s"git config user.email 'scalametabot@gmail.com'", cwd = repo.getAbsolutePath)
        os.shell.call(s"git config user.name 'Scalameta Bot'", cwd = repo.getAbsolutePath)
        os.shell.call(s"git commit -m $url", cwd = repo.getAbsolutePath)
        os.secret.obtain("github").foreach {
          case (username, password) =>
            val httpAuthentication = s"$username:$password@"
            val authUrl = s"https://${httpAuthentication}github.com/scalameta/scalameta.github.com"
            os.shell.call(s"git push $authUrl master", cwd = repo.getAbsolutePath)
        }
      } catch {
        case ex: Exception if ex.getMessage.contains(nothingToCommit) =>
          println(nothingToCommit)
      }
    }),
    publishLocal := {},
    publishM2 := {}
  )
  .dependsOn(scalametaJVM)
  .enablePlugins(BuildInfoPlugin)

// ==========================================
// Settings
// ==========================================

lazy val sharedSettings = Def.settings(
  scalaVersion := LanguageVersion,
  crossScalaVersions := LanguageVersions,
  crossVersion := {
    crossVersion.value match {
      case old @ ScalaJSCrossVersion.binary => old
      case _ => CrossVersion.binary
    }
  },
  version := customVersion.getOrElse(version.value.replace('+', '-')),
  organization := "org.scalameta",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.1" % "test",
  libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.13.5" % "test",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  scalacOptions.in(Compile, doc) ++= Seq("-skip-packages", ""),
  scalacOptions.in(Compile, doc) ++= Seq("-implicits", "-implicits-hide:."),
  scalacOptions.in(Compile, doc) ++= Seq("-groups"),
  scalacOptions ++= Seq("-Xfatal-warnings"),
  parallelExecution.in(Test) := false, // hello, reflection sync!!
  logBuffered := false,
  updateOptions := updateOptions.value.withCachedResolution(true),
  triggeredMessage.in(ThisBuild) := Watched.clearWhenTriggered
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
  }
)

lazy val adhocRepoUri = sys.props("scalameta.repository.uri")
lazy val adhocRepoCredentials = sys.props("scalameta.repository.credentials")
lazy val isCustomRepository = adhocRepoUri != null && adhocRepoCredentials != null

lazy val publishableSettings = Def.settings(
  publishTo := {
    if (isCustomRepository) Some("adhoc" at adhocRepoUri)
    else Some("Releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
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
        <id>olafurpg</id>
        <name>Ólafur Páll Geirsson</name>
        <url>https://geirsson.com/</url>
      </developer>
    </developers>
  )
)

lazy val nonPublishableSettings = Seq(
  publishArtifact in (Compile, packageDoc) := false,
  publishArtifact in packageDoc := false,
  sources in (Compile,doc) := Seq.empty,
  publishArtifact := false,
  PgpKeys.publishSigned := {},
  publish := {}
)

lazy val buildInfoSettings = Def.settings(
  buildInfoKeys := Seq[BuildInfoKey](
    version,
    "supportedScalaVersions" -> crossScalaVersions.in(scalametaJVM).value
  ),
  buildInfoPackage := "org.scalameta",
  buildInfoObject := "BuildInfo"
)

lazy val isFullCrossVersion = Seq(
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
      val scalaLibrary =
        classpath.map(_.toString).find(_.contains("scala-library")).get
      System.setProperty("sbt.paths.scalalibrary.classes", scalaLibrary)
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
  val backwardCompat210 = {
    if (scalaVersion.value.startsWith("2.10"))
      Seq("org.scalamacros" %% "quasiquotes" % "2.1.0")
    else Seq()
  }
  scalaReflect ++ scalaCompiler ++ backwardCompat210
}

lazy val isTagPush = sys.env.get("TRAVIS_TAG").exists(_.nonEmpty)
lazy val isCiPublish = sys.env.contains("CI_PUBLISH")
lazy val ciPlatform = if (sys.env.contains("CI_SCALA_JS")) "JS" else "JVM"
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
    } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq,
    PgpKeys.pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toCharArray())
  )
)

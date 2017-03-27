import java.io._
import scala.util.Try
import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import org.scalajs.sbtplugin.ScalaJSCrossVersion
import org.scalameta.os
import PgpKeys._
import UnidocKeys._
import sbt.ScriptedPlugin._
import com.trueaccord.scalapb.compiler.Version.scalapbVersion

lazy val ScalaVersion = sys.env.getOrElse("SCALA_VERSION", "2.11.8")
lazy val ScalaVersions = Seq("2.11.8", "2.12.1")
lazy val LibrarySeries = "1.7.0"
lazy val LibraryVersion =
  sys.props.getOrElse("scalameta.version", computePreReleaseVersion(LibrarySeries))

// ==========================================
// Projects
// ==========================================

name := "scalametaRoot"
sharedSettings
noPublish
unidocSettings
commands += Command.command("ci-fast") { state =>
  if (isScalaJS) "scalametaJS/test" :: state
  else "test" :: "doc" :: state
}
commands += Command.command("ci-slow") { state =>
  "scalahost/test:runMain scala.meta.tests.scalahost.converters.LotsOfProjects" ::
    "testkit/test:runMain scala.meta.testkit.ScalametaParserPropertyTest" ::
    "scalahostSbt/test" ::
    state
}
commands += Command.command("ci-publish") { state =>
  if (isPublish) "very publish" :: state
  else state
}
packagedArtifacts := Map.empty
unidocProjectFilter.in(ScalaUnidoc, unidoc) := inAnyProject
aggregate.in(test) := false
test := {
  // `test` by default only runs on the JVM
  val runScalametaTests = test.in(scalametaJVM, Test).value
  val runScalahostTests = test.in(scalahost, Test).value
  val runBenchmarkTests = test.in(benchmarks, Test).value
  val runContribTests = test.in(contrib, Test).value
  val runDocs = run.in(readme, Compile).toTask(" --validate").value
}
console := console.in(scalametaJVM, Compile).value

lazy val common = crossProject
  .in(file("scalameta/common"))
  .settings(
    publishableSettings,
    libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3",
    description := "Bag of private and public helpers used in scala.meta's APIs and implementations",
    enableMacros
  )
lazy val commonJVM = common.jvm
lazy val commonJS = common.js

lazy val io = crossProject
  .in(file("scalameta/io"))
  .settings(
    publishableSettings,
    description := "Scala.meta's API for JVM/JS agnostic IO."
  )
lazy val ioJVM = io.jvm
lazy val ioJS = io.js

lazy val dialects = crossProject
  .in(file("scalameta/dialects"))
  .settings(
    publishableSettings,
    description := "Scala.meta's dialects",
    enableMacros
  )
  .dependsOn(common)
lazy val dialectsJVM = dialects.jvm
lazy val dialectsJS = dialects.js

lazy val inline = crossProject
  .in(file("scalameta/inline"))
  .settings(
    publishableSettings,
    description := "Scala.meta's APIs for new-style (\"inline\") macros"
  )
  .dependsOn(inputs)
lazy val inlineJVM = inline.jvm
lazy val inlineJS = inline.js

lazy val inputs = crossProject
  .in(file("scalameta/inputs"))
  .settings(
    publishableSettings,
    description := "Scala.meta's APIs for source code in textual format",
    enableMacros
  )
  .dependsOn(common, io)
lazy val inputsJVM = inputs.jvm
lazy val inputsJS = inputs.js

lazy val parsers = crossProject
  .in(file("scalameta/parsers"))
  .settings(
    publishableSettings,
    description := "Scala.meta's API for parsing and its baseline implementation"
  )
  .dependsOn(common, dialects, inputs, tokens, tokenizers, trees)
lazy val parsersJVM = parsers.jvm
lazy val parsersJS = parsers.js

lazy val quasiquotes = crossProject
  .in(file("scalameta/quasiquotes"))
  .settings(
    publishableSettings,
    description := "Scala.meta's quasiquotes for abstract syntax trees",
    enableHardcoreMacros
  )
  .dependsOn(common, dialects, inputs, trees, parsers)
lazy val quasiquotesJVM = quasiquotes.jvm
lazy val quasiquotesJS = quasiquotes.js

lazy val tokenizers = crossProject
  .in(file("scalameta/tokenizers"))
  .settings(
    publishableSettings,
    description := "Scala.meta's APIs for tokenization and its baseline implementation",
    libraryDependencies += "com.lihaoyi" %%% "scalaparse" % "0.4.2",
    enableMacros
  )
  .dependsOn(common, dialects, inputs, tokens)
lazy val tokenizersJVM = tokenizers.jvm
lazy val tokenizersJS = tokenizers.js

lazy val tokens = crossProject
  .in(file("scalameta/tokens"))
  .settings(
    publishableSettings,
    description := "Scala.meta's tokens and token-based abstractions (inputs and positions)",
    enableMacros
  )
  .dependsOn(common, dialects, inputs)
lazy val tokensJVM = tokens.jvm
lazy val tokensJS = tokens.js

lazy val transversers = crossProject
  .in(file("scalameta/transversers"))
  .settings(
    publishableSettings,
    description := "Scala.meta's traversal and transformation infrastructure for abstract syntax trees",
    enableMacros
  )
  .dependsOn(common, trees)
lazy val traversersJVM = transversers.jvm
lazy val traversersJS = transversers.js

lazy val trees = crossProject
  .in(file("scalameta/trees"))
  .settings(
    publishableSettings,
    description := "Scala.meta's abstract syntax trees",
    // NOTE: uncomment this to update ast.md
    // scalacOptions += "-Xprint:typer",
    enableMacros
  )
  .dependsOn(common, dialects, inputs, tokens, tokenizers) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty
lazy val treesJVM = trees.jvm
lazy val treesJS = trees.js

lazy val semantic = crossProject
  .in(file("scalameta/semantic"))
  .settings(
    publishableSettings,
    description := "Scala.meta's semantic APIs",
    // Protobuf setup for binary serialization.
    PB.targets.in(Compile) := Seq(
      scalapb.gen(
        flatPackage = true // Don't append filename to package
      ) -> sourceManaged.in(Compile).value
    ),
    PB.protoSources.in(Compile) := Seq(file("scalameta/semantic/shared/src/main/protobuf")),
    libraryDependencies += "com.trueaccord.scalapb" %%% "scalapb-runtime" % scalapbVersion
  )
  .dependsOn(common, trees)
lazy val semanticJVM = semantic.jvm
lazy val semanticJS = semantic.js

lazy val scalameta = crossProject
  .in(file("scalameta/scalameta"))
  .settings(
    publishableSettings,
    description := "Scala.meta's metaprogramming APIs",
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
    inline,
    semantic
  )
lazy val scalametaJVM = scalameta.jvm
lazy val scalametaJS = scalameta.js

lazy val scalahost = Project(id = "scalahost", base = file("scalahost"))
  .settings(
    publishableSettings,
    hasLargeIntegrationTests,
    publishArtifact.in(Compile, packageSrc) := {
      // TODO: addCompilerPlugin for ivy repos is kinda broken.
      // If sbt finds a sources jar for a compiler plugin, it tries to add it to -Xplugin,
      // leading to nonsensical scalac invocations like `-Xplugin:...-sources.jar -Xplugin:...jar`.
      // Until this bug is fixed, we work around.
      if (shouldPublishToBintray) false
      else if (shouldPublishToSonatype) true
      else (publishArtifact.in(Compile, packageSrc)).value
    },
    mergeSettings,
    description := "Scala.meta's connector to the Scala compiler",
    crossVersion := CrossVersion.full,
    unmanagedSourceDirectories.in(Compile) += {
      // NOTE: sbt 0.13.8 provides cross-version support for Scala sources
      // (http://www.scala-sbt.org/0.13/docs/sbt-0.13-Tech-Previews.html#Cross-version+support+for+Scala+sources).
      // Unfortunately, it only includes directories like "scala_2.11" or "scala_2.12",
      // not "scala_2.11.8" or "scala_2.12.1" that we need.
      // That's why we have to work around here.
      val base = sourceDirectory.in(Compile).value
      base / ("scala-" + scalaVersion.value)
    },
    exposePaths("scalahost", Test),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    pomPostProcess := { node =>
      new RuleTransformer(new RewriteRule {
        private def isScalametaDependency(node: XmlNode): Boolean = {
          def isArtifactId(node: XmlNode, fn: String => Boolean) =
            node.label == "artifactId" && fn(node.text)
          node.label == "dependency" && node.child.exists(child =>
            isArtifactId(child, _.startsWith("scalameta_")))
        }
        override def transform(node: XmlNode): XmlNodeSeq = node match {
          case e: Elem if isScalametaDependency(node) =>
            Comment("scalameta dependency has been merged into scalahost via sbt-assembly")
          case _ => node
        }
      }).transform(node).head
    }
  )
  .dependsOn(scalametaJVM, testkit % Test)

lazy val scalahostSbt =
  Project(id = "scalahostSbt", base = file("scalahost-sbt"))
    .settings(
      publishableSettings,
      buildInfoSettings,
      sbt.ScriptedPlugin.scriptedSettings,
      sbtPlugin := true,
      bintrayRepository := "maven", // sbtPlugin overrides this to sbt-plugins
      testQuick := {
        // runs tests for 2.11 only, avoiding the need to publish for 2.12
        RunSbtCommand("; plz 2.11.8 publishLocal " +
          "; such scalahostSbt/scripted sbt-scalahost/simple211")(state.value)
      },
      test := {
        RunSbtCommand("; such publishLocal " +
          "; such scalahostSbt/scripted")(state.value)
      },
      description := "sbt plugin to enable the scalahost compiler plugin",
      moduleName := "sbt-scalahost", // sbt convention is that plugin names start with sbt-
      scalaVersion := "2.10.5",
      crossScalaVersions := Seq("2.10.5"), // for some reason, scalaVersion.value does not work.
      scriptedLaunchOpts ++= Seq(
        "-Dplugin.version=" + version.value,
        // .jvmopts is ignored, simulate here
        "-XX:MaxPermSize=256m",
        "-Xmx2g",
        "-Xss2m"
      ) ++ {
        // pass along custom boot properties if specified
        val bootProps = "sbt.boot.properties"
        sys.props.get(bootProps).map(x => s"-D$bootProps=$x").toList
      },
      scriptedBufferLog := false
    )
    .enablePlugins(BuildInfoPlugin)

lazy val testkit = Project(id = "testkit", base = file("scalameta/testkit"))
  .settings(
    publishableSettings,
    hasLargeIntegrationTests,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "geny" % "0.1.1",
      // These are used to download and extract a corpus tar.gz
      "org.rauschig" % "jarchivelib" % "0.7.1",
      "commons-io" % "commons-io" % "2.5"
    ),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
    description := "Testing utilities for scala.meta's metaprogramming APIs"
  )
  .dependsOn(scalametaJVM)

lazy val contrib = Project(id = "contrib", base = file("scalameta/contrib"))
  .settings(
    publishableSettings,
    description := "Utilities for scala.meta"
  )
  .dependsOn(scalametaJVM, testkit % Test)

lazy val benchmarks =
  Project(id = "benchmarks", base = file("scalameta/benchmarks"))
    .settings(
      sharedSettings,
      noPublish,
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
    noPublish,
    exposePaths("readme", Runtime),
    scalaVersion := scalaVersion.in(scalametaJVM).value,
    crossScalaVersions := ScalaVersions,
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    sources.in(Compile) ++= List("os.scala").map(f => baseDirectory.value / "../project" / f),
    watchSources ++= baseDirectory.value.listFiles.toList,
    test := run.in(Compile).toTask(" --validate").value,
    publish := {
      if (sys.props("sbt.prohibit.publish") != null)
        sys.error("Undefined publishing strategy")

      // generate the scalatex readme into `website`
      val website =
        new File(target.value.getAbsolutePath + File.separator + "scalatex")
      if (website.exists) website.delete
      val _ = run.in(Compile).toTask(" --validate").value
      if (!website.exists) sys.error("failed to generate the scalatex website")

      // import the scalatex readme into `repo`
      val repo = new File(os.temp.mkdir.getAbsolutePath + File.separator + "scalameta.org")
      os.shell.call(
        s"git clone https://github.com/scalameta/scalameta.github.com ${repo.getAbsolutePath}")
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
        val currentUrl = s"https://github.com/scalameta/scalameta/tree/" + os.git
          .currentSha()
        os.shell.call(s"git config user.email 'scalametabot@gmail.com'",
                      cwd = repo.getAbsolutePath)
        os.shell.call(s"git config user.name 'Scalameta Bot'", cwd = repo.getAbsolutePath)
        os.shell.call(s"git commit -m $currentUrl", cwd = repo.getAbsolutePath)
        val httpAuthentication = os.secret
          .obtain("github")
          .map { case (username, password) => s"$username:$password@" }
          .getOrElse("")
        val authenticatedUrl =
          s"https://${httpAuthentication}github.com/scalameta/scalameta.github.com"
        os.shell.call(s"git push $authenticatedUrl master", cwd = repo.getAbsolutePath)
      } catch {
        case ex: Exception if ex.getMessage.contains(nothingToCommit) =>
          println(nothingToCommit)
      }
    },
    // TODO: doesn't work at the moment, see https://github.com/sbt/sbt-pgp/issues/42
    publishSigned := publish.value,
    publishLocal := {},
    publishLocalSigned := {},
    publishM2 := {}
  )
  .dependsOn(scalametaJVM)

// ==========================================
// Settings
// ==========================================

lazy val sharedSettings = Def.settings(
  scalaVersion := ScalaVersion,
  crossScalaVersions := ScalaVersions,
  crossVersion := {
    crossVersion.value match {
      case old @ ScalaJSCrossVersion.binary => old
      case _ => CrossVersion.binary
    }
  },
  version := LibraryVersion,
  organization := "org.scalameta",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
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

def computePreReleaseVersion(LibrarySeries: String): String = {
  val preReleaseSuffix = {
    val gitDescribeSuffix = {
      val distance = os.git.distance("v1.0.0", "HEAD")
      val currentSha = os.git.currentSha().substring(0, 8)
      s"$distance-$currentSha"
    }
    if (os.git.isStable()) gitDescribeSuffix
    else gitDescribeSuffix + "." + os.time.stamp
  }
  LibrarySeries + "-" + preReleaseSuffix
}

def isScalaJS = sys.env.get("SCALA_JS") == Some("true")
def isPublish = sys.env.get("PUBLISH") == Some("true")

// Pre-release versions go to bintray and should be published via `publish`.
// This is the default behavior that you get without modifying the build.
// The only exception is that we take extra care to not publish on pull request validation jobs in Drone.
def shouldPublishToBintray: Boolean = {
  if (!new File(sys.props("user.home") + "/.bintray/.credentials").exists)
    return false
  if (sys.props("sbt.prohibit.publish") != null) return false
  if (sys.env.contains("CI_PULL_REQUEST")) return false
  LibraryVersion.contains("-")
}

// Release versions go to sonatype and then get synced to maven central.
// They should be published via `publish-signed` and signed by someone from <developers>.
// In order to publish a release version, change LibraryVersion to be equal to LibrarySeries.
def shouldPublishToSonatype: Boolean = {
  if (!os.secret.obtain("sonatype").isDefined) return false
  if (sys.props("sbt.prohibit.publish") != null) return false
  !LibraryVersion.contains("-")
}

lazy val publishableSettings = Def.settings(
  sharedSettings,
  bintrayOrganization := Some("scalameta"),
  publishArtifact.in(Compile) := true,
  publishArtifact.in(Test) := false, {
    val publishingStatus = {
      if (shouldPublishToBintray) "publishing to Bintray"
      else if (shouldPublishToSonatype) "publishing to Sonatype"
      else "publishing disabled"
    }
    println(s"[info] Welcome to scala.meta $LibraryVersion ($publishingStatus)")
    publish.in(Compile) := compilePublish.value
  },
  publishSigned.in(Compile) := compilePublishSigned.value,
  publishTo := {
    if (shouldPublishToBintray) {
      publishTo.in(bintray).value
    } else if (shouldPublishToSonatype)
      Some("releases".at("https://oss.sonatype.org/" + "service/local/staging/deploy/maven2"))
    else publishTo.value
  },
  credentials ++= {
    if (shouldPublishToBintray) {
      // NOTE: Bintray credentials are automatically loaded by the sbt-bintray plugin
      Nil
    } else if (shouldPublishToSonatype) {
      os.secret
        .obtain("sonatype")
        .map {
          case (username, password) =>
            Credentials("Sonatype Nexus Repository Manager",
                        "oss.sonatype.org",
                        username,
                        password)
        }
        .toList
    } else {
      Nil
    }
  },
  publishMavenStyle := {
    if (shouldPublishToBintray) false
    else if (shouldPublishToSonatype) true
    else publishMavenStyle.value
  },
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

lazy val buildInfoSettings = Def.settings(
  buildInfoKeys := Seq[BuildInfoKey](
    version,
    "supportedScalaVersions" -> crossScalaVersions.in(scalametaJVM).value
  ),
  buildInfoPackage := "org.scalameta",
  buildInfoObject := "BuildInfo"
)

lazy val noPublish = Seq(
  publishArtifact := false,
  publish := {},
  publishSigned := {},
  publishLocal := {}
)

lazy val hasLargeIntegrationTests = Seq(
  fork in (Test, run) := true,
  javaOptions in (Test, run) += "-Xss4m"
)

def exposePaths(projectName: String, config: Configuration) = {
  def uncapitalize(s: String) =
    if (s.length == 0) ""
    else {
      val chars = s.toCharArray; chars(0) = chars(0).toLower; new String(chars)
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

lazy val compilePublish: Def.Initialize[Task[Unit]] = Def.taskDyn {
  if (shouldPublishToBintray) {
    Def.task {
      publish.value
    }
  } else if (shouldPublishToSonatype) {
    Def.task {
      sys.error("Use publish-signed to publish release versions");
      ()
    }
  } else {
    Def.task {
      sys.error("Undefined publishing strategy"); ()
    }
  }
}

lazy val compilePublishSigned: Def.Initialize[Task[Unit]] = Def.taskDyn {
  if (shouldPublishToBintray) {
    Def.task {
      sys.error("Use publish to publish pre-release versions"); ()
    }
  } else if (shouldPublishToSonatype) Def.task {
    publishSigned.value
  } else {
    Def.task {
      sys.error("Undefined publishing strategy"); ()
    }
  }
}

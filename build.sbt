import java.io._
import scala.util.Try
import scala.xml.{Node => XmlNode, NodeSeq => XmlNodeSeq, _}
import scala.xml.transform.{RewriteRule, RuleTransformer}
import org.scalameta.os
import PgpKeys._
import UnidocKeys._

lazy val ScalaVersion = "2.11.8"
lazy val ScalaVersions = Seq("2.11.8", "2.12.1")
lazy val LibrarySeries = "1.6.0"
lazy val LibraryVersion = computePreReleaseVersion(LibrarySeries)

// ==========================================
// Projects
// ==========================================

lazy val scalametaRoot = Project(
  id = "scalametaRoot",
  base = file(".")
) settings (
  sharedSettings,
  unidocSettings,
  packagedArtifacts := Map.empty,
  unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject,
  aggregate in test := false,
  test := {
    val runScalametaTests = (test in scalameta in Test).value
    val runScalahostTests = (test in scalahost in Test).value
    val runDocs = (run in readme in Compile).toTask(" --validate").value
  },
  publish := {},
  publishSigned := {},
  console := (console in scalameta in Compile).value
) aggregate (
  common,
  contrib,
  dialects,
  inline,
  inputs,
  parsers,
  quasiquotes,
  scalahost,
  scalahostSbt,
  scalameta,
  semantic,
  testkit,
  tokenizers,
  tokens,
  transversers,
  trees
)

lazy val common = Project(
  id   = "common",
  base = file("scalameta/common")
) settings (
  publishableSettingsWithCrossBuild,
  libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.1.3",
  description := "Bag of private and public helpers used in scala.meta's APIs and implementations",
  enableMacros
)

lazy val dialects = Project(
  id   = "dialects",
  base = file("scalameta/dialects")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's dialects",
  enableMacros
) dependsOn (common)

lazy val inline = Project(
  id   = "inline",
  base = file("scalameta/inline")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's APIs for new-style (\"inline\") macros"
) dependsOn (inputs)

lazy val inputs = Project(
  id   = "inputs",
  base = file("scalameta/inputs")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's APIs for source code in textual format",
  enableMacros
) dependsOn (common)

lazy val parsers = Project(
  id   = "parsers",
  base = file("scalameta/parsers")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's API for parsing and its baseline implementation"
) dependsOn (common, dialects, inputs, tokens, tokenizers, trees)

lazy val quasiquotes = Project(
  id   = "quasiquotes",
  base = file("scalameta/quasiquotes")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's quasiquotes for abstract syntax trees",
  enableHardcoreMacros
) dependsOn (common, dialects, inputs, trees, parsers)

lazy val tokenizers = Project(
  id   = "tokenizers",
  base = file("scalameta/tokenizers")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's APIs for tokenization and its baseline implementation",
  libraryDependencies += "com.lihaoyi" %% "scalaparse" % "0.4.2",
  enableMacros
) dependsOn (common, dialects, inputs, tokens)

lazy val tokens = Project(
  id   = "tokens",
  base = file("scalameta/tokens")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's tokens and token-based abstractions (inputs and positions)",
  enableMacros
) dependsOn (common, dialects, inputs)

lazy val transversers = Project(
  id   = "transversers",
  base = file("scalameta/transversers")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's traversal and transformation infrastructure for abstract syntax trees",
  enableMacros
) dependsOn (common, trees)

lazy val trees = Project(
  id   = "trees",
  base = file("scalameta/trees")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's abstract syntax trees",
  // NOTE: uncomment this to update ast.md
  // scalacOptions += "-Xprint:typer",
  enableMacros
) dependsOn (common, dialects, inputs, tokens, tokenizers) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty

lazy val semantic = Project(
  id   = "semantic",
  base = file("scalameta/semantic")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's semantic APIs"
) dependsOn (common, trees)

lazy val scalameta = Project(
  id   = "scalameta",
  base = file("scalameta/scalameta")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Scala.meta's metaprogramming APIs",
  exposePaths("scalameta", Test)
) dependsOn (common, dialects, parsers, quasiquotes, tokenizers, transversers, trees, inline, semantic)

lazy val scalahost = Project(
  id   = "scalahost",
  base = file("scalahost")
) settings (
  publishableSettingsWithCrossBuild,
  publishArtifact in (Compile, packageSrc) := {
    // TODO: addCompilerPlugin for ivy repos is kinda broken.
    // If sbt finds a sources jar for a compiler plugin, it tries to add it to -Xplugin,
    // leading to nonsensical scalac invocations like `-Xplugin:...-sources.jar -Xplugin:...jar`.
    // Until this bug is fixed, we work around.
    if (shouldPublishToBintray) false
    else if (shouldPublishToSonatype) true
    else (publishArtifact in (Compile, packageSrc)).value
  },
  mergeSettings,
  description := "Scala.meta's connector to the Scala compiler",
  crossVersion := CrossVersion.full,
  unmanagedSourceDirectories in Compile += {
    // NOTE: sbt 0.13.8 provides cross-version support for Scala sources
    // (http://www.scala-sbt.org/0.13/docs/sbt-0.13-Tech-Previews.html#Cross-version+support+for+Scala+sources).
    // Unfortunately, it only includes directories like "scala_2.11" or "scala_2.12",
    // not "scala_2.11.8" or "scala_2.12.1" that we need.
    // That's why we have to work around here.
    val base = (sourceDirectory in Compile).value
    base / ("scala-" + scalaVersion.value)
  },
  exposePaths("scalahost", Test),
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  pomPostProcess := { node =>
    new RuleTransformer(new RewriteRule {
      private def isScalametaDependency(node: XmlNode): Boolean = {
        def isArtifactId(node: XmlNode, fn: String => Boolean) = node.label == "artifactId" && fn(node.text)
        node.label == "dependency" && node.child.exists(child => isArtifactId(child, _.startsWith("scalameta_")))
      }
      override def transform(node: XmlNode): XmlNodeSeq = node match {
        case e: Elem if isScalametaDependency(node) => Comment("scalameta dependency has been merged into scalahost via sbt-assembly")
        case _ => node
      }
    }).transform(node).head
  }
) dependsOn (scalameta, testkit % Test)

lazy val scalahostSbt = Project(
  id   = "scalahostSbt",
  base = file("sbt-plugins/scalahost")
) settings (
  publishableSettings,
  buildInfoSettings,
  description := "sbt plugin to enable the scalahost compiler plugin",
  moduleName := "sbt-scalahost",  // sbt convention is that plugin names start with sbt-
  sbtPlugin := true,
  scalaVersion := "2.10.5",
  crossScalaVersions := Seq(scalaVersion.value)
) enablePlugins (BuildInfoPlugin)

lazy val testkit = Project(
  id   = "testkit",
  base = file("scalameta/testkit")
) settings (
  publishableSettingsWithCrossBuild,
  libraryDependencies ++= Seq(
    "com.lihaoyi" %% "geny" % "0.1.1",
    // These are used to download and extract a corpus tar.gz
    "org.rauschig" % "jarchivelib" % "0.7.1",
    "commons-io" % "commons-io"  % "2.5"
  ),
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
  description := "Testing utilities for scala.meta's metaprogramming APIs"
) dependsOn (scalameta)

lazy val contrib = Project(
  id   = "contrib",
  base = file("scalameta/contrib")
) settings (
  publishableSettingsWithCrossBuild,
  description := "Utilities for scala.meta"
) dependsOn (scalameta, testkit % Test)

lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/scalameta/scalameta/tree/master",
  source = "Readme"
) settings (
  exposePaths("readme", Runtime),
  scalaVersion := (scalaVersion in scalameta).value,
  crossScalaVersions := ScalaVersions,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  sources in Compile ++= List("os.scala").map(f => baseDirectory.value / "../project" / f),
  watchSources ++= baseDirectory.value.listFiles.toList,
  publish := {
    if (sys.props("sbt.prohibit.publish") != null) sys.error("Undefined publishing strategy")

    // generate the scalatex readme into `website`
    val website = new File(target.value.getAbsolutePath + File.separator + "scalatex")
    if (website.exists) website.delete
    val _ = (run in Compile).toTask(" --validate").value
    if (!website.exists) sys.error("failed to generate the scalatex website")

    // import the scalatex readme into `repo`
    val repo = new File(os.temp.mkdir.getAbsolutePath + File.separator + "scalameta.org")
    os.shell.call(s"git clone https://github.com/scalameta/scalameta.github.com ${repo.getAbsolutePath}")
    println(s"erasing everything in ${repo.getAbsolutePath}...")
    repo.listFiles.filter(f => f.getName != ".git").foreach(os.shutil.rmtree)
    println(s"importing website from ${website.getAbsolutePath} to ${repo.getAbsolutePath}...")
    new PrintWriter(new File(repo.getAbsolutePath + File.separator + "CNAME")) { write("scalameta.org"); close }
    website.listFiles.foreach(src => os.shutil.copytree(src, new File(repo.getAbsolutePath + File.separator + src.getName)))

    // commit and push the changes if any
    os.shell.call(s"git add -A", cwd = repo.getAbsolutePath)
    val nothingToCommit = "nothing to commit, working directory clean"
    try {
      val currentUrl = s"https://github.com/scalameta/scalameta/tree/" + os.git.stableSha()
      os.shell.call(s"git config user.email 'scalametabot@gmail.com'", cwd = repo.getAbsolutePath)
      os.shell.call(s"git config user.name 'Scalameta Bot'", cwd = repo.getAbsolutePath)
      os.shell.call(s"git commit -m $currentUrl", cwd = repo.getAbsolutePath)
      val httpAuthentication = os.secret.obtain("github").map{ case (username, password) => s"$username:$password@" }.getOrElse("")
      val authenticatedUrl = s"https://${httpAuthentication}github.com/scalameta/scalameta.github.com"
      os.shell.call(s"git push $authenticatedUrl master", cwd = repo.getAbsolutePath)
    } catch {
      case ex: Exception if ex.getMessage.contains(nothingToCommit) => println(nothingToCommit)
    }
  },
  // TODO: doesn't work at the moment, see https://github.com/sbt/sbt-pgp/issues/42
  publishSigned := publish.value,
  publishLocal := {},
  publishLocalSigned := {},
  publishM2 := {}
) dependsOn (scalameta)

// ==========================================
// Settings
// ==========================================

lazy val sharedSettings = Def.settings(
  scalaVersion := ScalaVersion,
  version := LibraryVersion,
  organization := "org.scalameta",
  resolvers += Resolver.sonatypeRepo("snapshots"),
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  scalacOptions in (Compile, doc) ++= Seq("-skip-packages", ""),
  scalacOptions in (Compile, doc) ++= Seq("-implicits", "-implicits-hide:."),
  scalacOptions in (Compile, doc) ++= Seq("-groups"),
  scalacOptions ++= Seq("-Xfatal-warnings"),
  parallelExecution in Test := false, // hello, reflection sync!!
  logBuffered := false,
  triggeredMessage in ThisBuild := Watched.clearWhenTriggered
)

lazy val mergeSettings = Def.settings(
  sharedSettings,
  test in assembly := {},
  logLevel in assembly := Level.Error,
  assemblyJarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
  assemblyOption in assembly ~= { _.copy(includeScala = false) },
  Keys.`package` in Compile := {
    val slimJar = (Keys.`package` in Compile).value
    val fatJar  = new File(crossTarget.value + "/" + (assemblyJarName in assembly).value)
    val _       = assembly.value
    IO.copy(List(fatJar -> slimJar), overwrite = true)
    slimJar
  },
  packagedArtifact in Compile in packageBin := {
    val temp           = (packagedArtifact in Compile in packageBin).value
    val (art, slimJar) = temp
    val fatJar         = new File(crossTarget.value + "/" + (assemblyJarName in assembly).value)
    val _              = assembly.value
    IO.copy(List(fatJar -> slimJar), overwrite = true)
    (art, slimJar)
  }
)

def computePreReleaseVersion(LibrarySeries: String): String = {
  val preReleaseSuffix = {
    import sys.process._
    val stableSha = Try(os.git.stableSha()).toOption
    val commitSubjects = Try(augmentString(os.shell.check_output("git log -10 --pretty=%s", cwd = ".")).lines.toList).getOrElse(Nil)
    val prNumbers = commitSubjects.map(commitSubject => {
      val Merge = "Merge pull request #(\\d+).*".r
      val Squash = ".*\\(#(\\d+)\\)".r
      commitSubject match {
        case Merge(prNumber) => Some(prNumber)
        case Squash(prNumber) => Some(prNumber)
        case _ => None
      }
    })
    val mostRecentPrNumber = prNumbers.flatMap(_.toList).headOption
    (stableSha, prNumbers, mostRecentPrNumber) match {
      case (Some(_), Some(prNumber) +: _, _) => prNumber
      case (_, _, Some(prNumber)) => prNumber + "." + System.currentTimeMillis()
      case _ => "unknown" + "." + System.currentTimeMillis()
    }
  }
  LibrarySeries + "-" + preReleaseSuffix
}

// Pre-release versions go to bintray and should be published via `publish`.
// This is the default behavior that you get without modifying the build.
// The only exception is that we take extra care to not publish on pull request validation jobs in Drone.
def shouldPublishToBintray: Boolean = {
  if (!new File(sys.props("user.home") + "/.bintray/.credentials").exists) return false
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

lazy val crossBuildSettings = Def.settings(
  crossScalaVersions := ScalaVersions,
  crossVersion := CrossVersion.binary
)

lazy val publishableSettingsWithCrossBuild = Def.settings(
  crossBuildSettings,
  publishableSettings
)

lazy val publishableSettings = Def.settings(
  sharedSettings,
  bintrayOrganization := Some("scalameta"),
  publishArtifact in Compile := true,
  publishArtifact in Test := false,
  {
    val publishingStatus = {
      if (shouldPublishToBintray) "publishing to Bintray"
      else if (shouldPublishToSonatype) "publishing to Sonatype"
      else "publishing disabled"
    }
    println(s"[info] Welcome to scala.meta $LibraryVersion ($publishingStatus)")
    publish in Compile := (Def.taskDyn {
      if (shouldPublishToBintray) Def.task { publish.value }
      else if (shouldPublishToSonatype) Def.task { sys.error("Use publish-signed to publish release versions"); () }
      else Def.task { sys.error("Undefined publishing strategy"); () }
    }).value
  },
  publishSigned in Compile := (Def.taskDyn {
    if (shouldPublishToBintray) Def.task { sys.error("Use publish to publish pre-release versions"); () }
    else if (shouldPublishToSonatype) Def.task { publishSigned.value }
    else Def.task { sys.error("Undefined publishing strategy"); () }
  }).value,
  publishTo := {
    if (shouldPublishToBintray) (publishTo in bintray).value
    else if (shouldPublishToSonatype) Some("releases" at "https://oss.sonatype.org/" + "service/local/staging/deploy/maven2")
    else publishTo.value
  },
  credentials ++= {
    if (shouldPublishToBintray) {
      // NOTE: Bintray credentials are automatically loaded by the sbt-bintray plugin
      Nil
    } else if (shouldPublishToSonatype) {
      os.secret.obtain("sonatype").map({
        case (username, password) => Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
      }).toList
    } else Nil
  },
  publishMavenStyle := {
    if (shouldPublishToBintray) false
    else if (shouldPublishToSonatype) true
    else publishMavenStyle.value
  },
  pomIncludeRepository := { x => false },
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
    version
  ),
  buildInfoPackage := "org.scalameta",
  buildInfoObject := "BuildInfo"
)

def exposePaths(projectName: String, config: Configuration) = {
  def uncapitalize(s: String) = if (s.length == 0) "" else { val chars = s.toCharArray; chars(0) = chars(0).toLower; new String(chars) }
  val prefix = "sbt.paths." + projectName + "." + uncapitalize(config.name) + "."
  Seq(
    sourceDirectory in config := {
      val defaultValue = (sourceDirectory in config).value
      System.setProperty(prefix + "sources", defaultValue.getAbsolutePath)
      defaultValue
    },
    resourceDirectory in config := {
      val defaultValue = (resourceDirectory in config).value
      System.setProperty(prefix + "resources", defaultValue.getAbsolutePath)
      defaultValue
    },
    fullClasspath in config := {
      val defaultValue = (fullClasspath in config).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      val scalaLibrary = classpath.map(_.toString).find(_.contains("scala-library")).get
      System.setProperty("sbt.paths.scalalibrary.classes", scalaLibrary)
      System.setProperty(prefix + "classes", classpath.mkString(java.io.File.pathSeparator))
      defaultValue
    }
  )
}

lazy val enableMacros = macroDependencies(hardcore = false)

lazy val enableHardcoreMacros = macroDependencies(hardcore = true)

def macroDependencies(hardcore: Boolean) = libraryDependencies ++= {
  val scalaReflect = Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided")
  val scalaCompiler = {
    if (hardcore) Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided")
    else Nil
  }
  val backwardCompat210 = {
    if (scalaVersion.value.startsWith("2.10")) Seq("org.scalamacros" %% "quasiquotes" % "2.1.0")
    else Seq()
  }
  scalaReflect ++ scalaCompiler ++ backwardCompat210
}

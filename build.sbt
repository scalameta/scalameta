import java.io._
import scala.util.Try
import org.scalameta.os._
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
    val runTests = (test in scalameta in Test).value
    val runDocs = (run in readme in Compile).toTask(" --validate").value
  },
  publish := {},
  publishSigned := {},
  console := (console in scalameta in Compile).value
) aggregate (
  common,
  dialects,
  inline,
  inputs,
  parsers,
  quasiquotes,
  scalameta,
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
  publishableSettings,
  description := "Bag of private and public helpers used in scala.meta's APIs and implementations",
  enableMacros
)

lazy val dialects = Project(
  id   = "dialects",
  base = file("scalameta/dialects")
) settings (
  publishableSettings,
  description := "Scala.meta's dialects",
  enableMacros
) dependsOn (common)

lazy val inline = Project(
  id   = "inline",
  base = file("scalameta/inline")
) settings (
  publishableSettings,
  description := "Scala.meta's APIs for new-style (\"inline\") macros"
) dependsOn (inputs)

lazy val inputs = Project(
  id   = "inputs",
  base = file("scalameta/inputs")
) settings (
  publishableSettings,
  description := "Scala.meta's APIs for source code in textual format"
) dependsOn (common)

lazy val parsers = Project(
  id   = "parsers",
  base = file("scalameta/parsers")
) settings (
  publishableSettings,
  description := "Scala.meta's API for parsing and its baseline implementation"
) dependsOn (common, dialects, inputs, tokens, tokenizers, trees)

lazy val quasiquotes = Project(
  id   = "quasiquotes",
  base = file("scalameta/quasiquotes")
) settings (
  publishableSettings,
  description := "Scala.meta's quasiquotes for abstract syntax trees",
  enableHardcoreMacros
) dependsOn (common, dialects, inputs, trees, parsers)

lazy val tokenizers = Project(
  id   = "tokenizers",
  base = file("scalameta/tokenizers")
) settings (
  publishableSettings,
  description := "Scala.meta's APIs for tokenization and its baseline implementation",
  libraryDependencies += "com.lihaoyi" %% "scalaparse" % "0.4.2",
  enableMacros
) dependsOn (common, dialects, inputs, tokens)

lazy val tokens = Project(
  id   = "tokens",
  base = file("scalameta/tokens")
) settings (
  publishableSettings,
  description := "Scala.meta's tokens and token-based abstractions (inputs and positions)",
  enableMacros
) dependsOn (common, dialects, inputs)

lazy val transversers = Project(
  id   = "transversers",
  base = file("scalameta/transversers")
) settings (
  publishableSettings,
  description := "Scala.meta's traversal and transformation infrastructure for abstract syntax trees",
  enableMacros
) dependsOn (common, trees)

lazy val trees = Project(
  id   = "trees",
  base = file("scalameta/trees")
) settings (
  publishableSettings,
  description := "Scala.meta's abstract syntax trees",
  // NOTE: uncomment this to update ast.md
  // scalacOptions += "-Xprint:typer",
  enableMacros
) dependsOn (common, dialects, inputs, tokens, tokenizers) // NOTE: tokenizers needed for Tree.tokens when Tree.pos.isEmpty

lazy val scalameta = Project(
  id   = "scalameta",
  base = file("scalameta/scalameta")
) settings (
  publishableSettings,
  description := "Scala.meta's metaprogramming APIs",
  exposePaths("scalameta", Test)
) dependsOn (common, dialects, parsers, quasiquotes, tokenizers, transversers, trees, inline)

lazy val testkit = Project(
  id   = "testkit",
  base = file("scalameta/testkit")
) settings (
  publishableSettings,
  libraryDependencies += "com.lihaoyi" %% "geny" % "0.1.1",
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Test,
  description := "Testing utilities for scala.meta's metaprogramming APIs"
) dependsOn (scalameta)

lazy val readme = scalatex.ScalatexReadme(
  projectId = "readme",
  wd = file(""),
  url = "https://github.com/scalameta/scalameta/tree/master",
  source = "Readme"
) settings (
  exposePaths("readme", Runtime),
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
    val repo = new File(temp.mkdir.getAbsolutePath + File.separator + "scalameta.org")
    shell.call(s"git clone https://github.com/scalameta/scalameta.github.com ${repo.getAbsolutePath}")
    println(s"erasing everything in ${repo.getAbsolutePath}...")
    repo.listFiles.filter(f => f.getName != ".git").foreach(shutil.rmtree)
    println(s"importing website from ${website.getAbsolutePath} to ${repo.getAbsolutePath}...")
    new PrintWriter(new File(repo.getAbsolutePath + File.separator + "CNAME")) { write("scalameta.org"); close }
    website.listFiles.foreach(src => shutil.copytree(src, new File(repo.getAbsolutePath + File.separator + src.getName)))

    // commit and push the changes if any
    shell.call(s"git add -A", cwd = repo.getAbsolutePath)
    val nothingToCommit = "nothing to commit, working directory clean"
    try {
      val currentUrl = s"https://github.com/scalameta/scalameta/tree/" + git.stableSha()
      shell.call(s"git config user.email 'scalametabot@gmail.com'", cwd = repo.getAbsolutePath)
      shell.call(s"git config user.name 'Scalameta Bot'", cwd = repo.getAbsolutePath)
      shell.call(s"git commit -m $currentUrl", cwd = repo.getAbsolutePath)
      val httpAuthentication = secret.obtain("github").map{ case (username, password) => s"$username:$password@" }.getOrElse("")
      val authenticatedUrl = s"https://${httpAuthentication}github.com/scalameta/scalameta.github.com"
      shell.call(s"git push $authenticatedUrl master", cwd = repo.getAbsolutePath)
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
  crossScalaVersions := ScalaVersions,
  crossVersion := CrossVersion.binary,
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

def computePreReleaseVersion(LibrarySeries: String): String = {
  val preReleaseSuffix = {
    import sys.process._
    val stableSha = Try(git.stableSha()).toOption
    val commitSubjects = Try(augmentString(shell.check_output("git log -10 --pretty=%s", cwd = ".")).lines.toList).getOrElse(Nil)
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
  if (!secret.obtain("sonatype").isDefined) return false
  if (sys.props("sbt.prohibit.publish") != null) return false
  !LibraryVersion.contains("-")
}

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
      secret.obtain("sonatype").map({
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
    </developers>
  )
)

lazy val mergeSettings: Seq[sbt.Def.Setting[_]] = Def.settings(
  test in assembly := {},
  logLevel in assembly := Level.Error,
  assemblyJarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
  assemblyOption in assembly ~= { _.copy(includeScala = false) },
  Keys.`package` in Compile := {
    val slimJar = (Keys.`package` in Compile).value
    val fatJar = new File(crossTarget.value + "/" + (assemblyJarName in assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), overwrite = true)
    slimJar
  },
  packagedArtifact in Compile in packageBin := {
    val temp = (packagedArtifact in Compile in packageBin).value
    val (art, slimJar) = temp
    val fatJar = new File(crossTarget.value + "/" + (assemblyJarName in assembly).value)
    val _ = assembly.value
    IO.copy(List(fatJar -> slimJar), overwrite = true)
    (art, slimJar)
  }
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

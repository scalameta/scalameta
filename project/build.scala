import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import sbtunidoc.Plugin._
import UnidocKeys._

object build extends Build {
  lazy val ScalaVersions = Seq("2.11.8")
  lazy val LibraryVersion = "0.1.0-SNAPSHOT"

  lazy val root = Project(
    id = "root",
    base = file("root")
  ) settings (
    sharedSettings : _*
  ) settings (
    unidocSettings : _*
  ) settings (
    packagedArtifacts := Map.empty,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject,
    aggregate in test := false,
    test := (test in scalameta in Test).value
  ) aggregate (
    common,
    dialects,
    inputs,
    parsers,
    quasiquotes,
    scalameta,
    tokenizers,
    tokens,
    transversers,
    trees
  )

  lazy val common = Project(
    id   = "common",
    base = file("scalameta/common")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Bag of private and public helpers used in scala.meta's APIs and implementations",
    enableMacros
  )

  lazy val dialects = Project(
    id   = "dialects",
    base = file("scalameta/dialects")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's dialects",
    enableMacros
  ) dependsOn (common)

  lazy val inputs = Project(
    id   = "inputs",
    base = file("scalameta/inputs")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's APIs for source code in textual format"
  ) dependsOn (common)

  lazy val parsers = Project(
    id   = "parsers",
    base = file("scalameta/parsers")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's API for parsing and its baseline implementation"
  ) dependsOn (common, dialects, inputs, trees)

  lazy val quasiquotes = Project(
    id   = "quasiquotes",
    base = file("scalameta/quasiquotes")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's quasiquotes for abstract syntax trees",
    enableHardcoreMacros
  ) dependsOn (common, trees, parsers)

  lazy val tokenizers = Project(
    id   = "tokenizers",
    base = file("scalameta/tokenizers")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's APIs for tokenization and its baseline implementation",
    libraryDependencies += "com.lihaoyi" %% "scalaparse" % "0.3.7",
    enableMacros
  ) dependsOn (common, dialects, inputs, tokens)

  lazy val tokens = Project(
    id   = "tokens",
    base = file("scalameta/tokens")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's tokens and token-based abstractions (inputs and positions)",
    enableMacros
  ) dependsOn (common, dialects, inputs)

  lazy val transversers = Project(
    id   = "transversers",
    base = file("scalameta/transversers")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's traversal and transformation infrastructure for abstract syntax trees",
    enableMacros
  ) dependsOn (common, trees)

  lazy val trees = Project(
    id   = "trees",
    base = file("scalameta/trees")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's abstract syntax trees",
    // NOTE: uncomment this to update ast.md
    // scalacOptions += "-Xprint:typer",
    enableMacros
  ) dependsOn (common, dialects, tokens, tokenizers) // TODO: get rid of tokenizers

  lazy val scalameta = Project(
    id   = "scalameta",
    base = file("scalameta/scalameta")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's metaprogramming APIs"
  ) settings (
    exposePaths("scalameta", Test): _*
  ) dependsOn (common, dialects, parsers, quasiquotes, tokenizers, transversers, trees)

  lazy val sharedSettings = Defaults.defaultSettings ++ crossVersionSharedSources ++ Seq(
    scalaVersion := ScalaVersions.max,
    crossScalaVersions := ScalaVersions,
    crossVersion := CrossVersion.binary,
    version := LibraryVersion,
    organization := "org.scalameta",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    scalacOptions in (Compile, doc) ++= Seq("-skip-packages", ""),
    scalacOptions in (Compile, doc) ++= Seq("-implicits", "-implicits-hide:."),
    scalacOptions in (Compile, doc) ++= Seq("-groups"),
    scalacOptions in Test ++= Seq("-Xfatal-warnings"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("core.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    },
    publishMavenStyle := true,
    publishOnlyWhenOnMaster := publishOnlyWhenOnMasterImpl.value,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/scalameta/scalameta</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>https://github.com/scalameta/scalameta/blob/master/LICENSE.md</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
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

  lazy val publishableSettings = sharedSettings ++ Seq(
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    credentials ++= {
      val mavenSettingsFile = System.getProperty("maven.settings.file")
      if (mavenSettingsFile != null) {
        println("Loading Sonatype credentials from " + mavenSettingsFile)
        try {
          import scala.xml._
          val settings = XML.loadFile(mavenSettingsFile)
          def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
          Some(Credentials(
            "Sonatype Nexus Repository Manager",
            "oss.sonatype.org",
            readServerConfig("username"),
            readServerConfig("password")
          ))
        } catch {
          case ex: Exception =>
            println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
            None
        }
      } else {
        for {
          realm <- sys.env.get("SCALAMETA_MAVEN_REALM")
          domain <- sys.env.get("SCALAMETA_MAVEN_DOMAIN")
          user <- sys.env.get("SCALAMETA_MAVEN_USER")
          password <- sys.env.get("SCALAMETA_MAVEN_PASSWORD")
        } yield {
          println("Loading Sonatype credentials from environment variables")
          Credentials(realm, domain, user, password)
        }
      }
    }.toList
  )

  // http://stackoverflow.com/questions/20665007/how-to-publish-only-when-on-master-branch-under-travis-and-sbt-0-13
  lazy val publishOnlyWhenOnMaster = taskKey[Unit]("publish task for Travis (don't publish when building pull requests, only publish when the build is triggered by merge into master)")
  def publishOnlyWhenOnMasterImpl = Def.taskDyn {
    import scala.util.Try
    val travis   = Try(sys.env("TRAVIS")).getOrElse("false") == "true"
    val pr       = Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false"
    val branch   = Try(sys.env("TRAVIS_BRANCH")).getOrElse("??")
    val snapshot = version.value.trim.endsWith("SNAPSHOT")
    (travis, pr, branch, snapshot) match {
      case (true, false, "master", true) => publish
      case _                             => Def.task ()
    }
  }

  lazy val mergeSettings: Seq[sbt.Def.Setting[_]] = assemblySettings ++ Seq(
    test in assembly := {},
    logLevel in assembly := Level.Error,
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false) },
    Keys.`package` in Compile := {
      val slimJar = (Keys.`package` in Compile).value
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      slimJar
    },
    packagedArtifact in Compile in packageBin := {
      val temp = (packagedArtifact in Compile in packageBin).value
      val (art, slimJar) = temp
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
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

  lazy val enableMacros = macroDependencies(hardcore = false)
  lazy val enableHardcoreMacros = macroDependencies(hardcore = true)

  lazy val crossVersionSharedSources: Seq[Setting[_]] =
    Seq(Compile, Test).map { sc =>
      (unmanagedSourceDirectories in sc) ++= {
        (unmanagedSourceDirectories in sc).value.map { dir =>
          CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((2, y)) if y == 10 => new File(dir.getPath + "_2.10")
            case Some((2, y)) if y == 11 => new File(dir.getPath + "_2.11")
            case other => sys.error("unsupported Scala version " + other)
          }
        }
      }
    }
}

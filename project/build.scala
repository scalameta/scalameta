import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import sbtunidoc.Plugin._
import UnidocKeys._

object build extends Build {
  lazy val ScalaVersion = "2.11.7"
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
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(foundation)
  ) aggregate (
    foundation,
    dialects,
    exceptions,
    inputs,
    parsers,
    prettyprinters,
    quasiquotes,
    scalameta,
    tokenizers,
    tokenquasiquotes,
    tokens,
    tql,
    trees,
    scalahost
  )

  lazy val foundation = Project(
    id   = "foundation",
    base = file("foundation")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's fundamental helpers and utilities",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")
  )

  lazy val dialects = Project(
    id   = "dialects",
    base = file("scalameta/dialects")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's dialects",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")
  ) dependsOn (foundation, exceptions)

  lazy val exceptions = Project(
    id   = "exceptions",
    base = file("scalameta/exceptions")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's baseline exceptions"
  ) dependsOn (foundation)

  lazy val inputs = Project(
    id   = "inputs",
    base = file("scalameta/inputs")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's APIs for source code in textual format"
  ) dependsOn (foundation, exceptions)

  lazy val parsers = Project(
    id   = "parsers",
    base = file("scalameta/parsers")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's API for parsing and its baseline implementation"
  ) dependsOn (foundation, exceptions, trees, tokens, tokenizers % "test", tql % "test")

  lazy val prettyprinters = Project(
    id   = "prettyprinters",
    base = file("scalameta/prettyprinters")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's baseline prettyprinters"
  ) dependsOn (foundation, exceptions)

  lazy val quasiquotes = Project(
    id   = "quasiquotes",
    base = file("scalameta/quasiquotes")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's quasiquotes for abstract syntax trees"
  ) dependsOn (foundation, exceptions, tokens, trees, parsers)

  lazy val tokenizers = Project(
    id   = "tokenizers",
    base = file("scalameta/tokenizers")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's APIs for tokenization and its baseline implementation",
    // TODO: This is a major embarassment: we need scalac's parser to parse xml literals,
    // because it was too hard to implement the xml literal parser from scratch.
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
  ) dependsOn (foundation, exceptions, tokens)

  lazy val tokenquasiquotes = Project(
    id   = "tokenquasiquotes",
    base = file("scalameta/tokenquasiquotes")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's quasiquotes for tokens",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")
  ) dependsOn (foundation, exceptions, tokens, tokenizers)

  lazy val tokens = Project(
    id   = "tokens",
    base = file("scalameta/tokens")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's tokens and token-based abstractions (inputs and positions)",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")
  ) dependsOn (foundation, exceptions, prettyprinters, dialects, inputs)

  lazy val tql = Project(
    id   = "tql",
    base = file("scalameta/tql")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's tree query language (basic and extended APIs)",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "provided")
  ) dependsOn (foundation, exceptions, trees)

  lazy val trees = Project(
    id   = "trees",
    base = file("scalameta/trees")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's abstract syntax trees",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided")
  ) dependsOn (foundation, exceptions, prettyprinters, inputs, tokens, tokenquasiquotes)

  lazy val scalameta = Project(
    id   = "scalameta",
    base = file("scalameta/scalameta")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Scala.meta's metaprogramming and hosting APIs"
  ) dependsOn (foundation, exceptions, dialects, parsers, prettyprinters, quasiquotes, tokenizers, tokenquasiquotes, tql, trees)

  lazy val scalahost = Project(
    id   = "scalahost",
    base = file("scalahost")
  ) settings (
    publishableSettings: _*
  ) settings (
    mergeSettings: _*
  ) settings (
    crossVersion := CrossVersion.full,
    description := "Scalac-based host that implements scala.meta's hosting APIs",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    scalacOptions in Test := {
      val defaultValue = (scalacOptions in Test).value
      val scalahostJar = (Keys.`package` in Compile).value
      System.setProperty("sbt.paths.scalahost.compile.jar", scalahostJar.getAbsolutePath)
      val addPlugin = "-Xplugin:" + scalahostJar.getAbsolutePath
      defaultValue ++ Seq("-Jdummy=" + scalahostJar.lastModified)
    }
  ) settings (
    exposePaths("scalahost", Test): _*
  ) dependsOn (scalameta)

  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := ScalaVersion,
    crossVersion := CrossVersion.binary,
    version := LibraryVersion,
    organization := "org.scalameta",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
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
}

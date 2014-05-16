import sbt._
import Keys._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.0",
    crossVersion := CrossVersion.binary,
    version := "0.1.0-SNAPSHOT",
    organization := "org.scalareflect",
    description := "Reflection core of Project Palladium",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalaSource in Compile <<= (baseDirectory in Compile)(base => base / "src"),
    scalaSource in Test <<= (baseDirectory in Test)(base => base / "test"),
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature", "-optimise"),
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
      <url>https://github.com/scalareflect/core</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/scalareflect/core.git</url>
        <connection>scm:git:git://github.com/scalareflect/core.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalareflect/core/issues</url>
      </issueManagement>
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M1" cross CrossVersion.full)
  )

  // http://stackoverflow.com/questions/20665007/how-to-publish-only-when-on-master-branch-under-travis-and-sbt-0-13
  val publishOnlyWhenOnMaster = taskKey[Unit]("publish task for Travis (don't publish when building pull requests, only publish when the build is triggered by merge into master)")
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
          realm <- sys.env.get("SCALAREFLECT_MAVEN_REALM")
          domain <- sys.env.get("SCALAREFLECT_MAVEN_DOMAIN")
          user <- sys.env.get("SCALAREFLECT_MAVEN_USER")
          password <- sys.env.get("SCALAREFLECT_MAVEN_PASSWORD")
        } yield {
          println("Loading Sonatype credentials from environment variables")
          Credentials(realm, domain, user, password)
        }
      }
    }.toList
  )

  lazy val root = Project(
    id = "root",
    base = file("root")
  ) settings (
    sharedSettings : _*
  ) settings (
    test in Test := (test in tests in Test).value,
    packagedArtifacts := Map.empty
  ) aggregate (reflection, tests)

  lazy val foundation = Project(
    id   = "foundation",
    base = file("foundation")
  ) settings (
    publishableSettings: _*
  ) settings (
    scalaSource in Compile <<= (baseDirectory in Compile)(base => base),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
    scalacOptions ++= Seq()
  )

  lazy val reflection = Project(
    id   = "core",
    base = file("reflection")
  ) settings (
    publishableSettings: _*
  ) settings (
    scalaSource in Compile <<= (baseDirectory in Compile)(base => base),
    // scalacOptions ++= Seq("-Xprint:typer")
    scalacOptions ++= Seq()
  ) dependsOn (foundation)

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    sharedSettings: _*
  ) settings (
    scalaSource in Compile <<= (baseDirectory in Compile)(base => base),
    scalacOptions ++= Seq()
  ) dependsOn (reflection)

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings: _*
  ) settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    packagedArtifacts := Map.empty,
    scalacOptions ++= Seq()
  ) dependsOn (reflection)
}

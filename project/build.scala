import sbt._
import Keys._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.0-RC1",
    crossVersion := CrossVersion.full,
    version := "0.1.0-SNAPSHOT",
    organization := "org.scalareflect",
    description := "Reflection core of Project Palladium",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
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
    }
    // TODO: how to I make this recursion work?
    // run <<= run in Compile in sandbox,
    // test <<= test in Test in tests
  )

  lazy val publishableSettings = sharedSettings ++ Seq(
    publishMavenStyle := true,
    publishArtifact in Compile := true,
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
    credentials ++= loadCredentials()
  )

  def loadCredentials(): List[Credentials] = {
    val mavenSettingsFile = System.getProperty("maven.settings.file")
    if (mavenSettingsFile != null) {
      println("Loading Sonatype credentials from " + mavenSettingsFile)
      try {
        import scala.xml._
        val settings = XML.loadFile(mavenSettingsFile)
        def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
        List(Credentials(
          "Sonatype Nexus Repository Manager",
          "oss.sonatype.org",
          readServerConfig("username"),
          readServerConfig("password")
        ))
      } catch {
        case ex: Exception =>
          println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
          Nil
      }
    } else {
      // println("Sonatype credentials cannot be loaded: -Dmaven.settings.file is not specified.")
      Nil
    }
  }

  lazy val core = Project(
    id   = "core",
    base = file("core")
  ) settings (
    publishableSettings: _*
  ) settings (
    scalacOptions ++= Seq()
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    sharedSettings: _*
  ) settings (
    scalacOptions ++= Seq()
  ) dependsOn (core)

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings: _*
  ) settings (
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.0" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    scalacOptions ++= Seq()
  ) dependsOn (core)
}

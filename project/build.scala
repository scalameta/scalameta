import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import com.typesafe.sbt.pgp.PgpKeys._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.1",
    crossVersion := CrossVersion.full,
    version := "0.1.0-SNAPSHOT",
    organization := "org.scalareflect",
    description := "Scala host for Project Palladium",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-feature", "-optimise"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("scalahost.scala.home")
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
      <url>https://github.com/scalareflect/scalahost</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/scalareflect/scalahost.git</url>
        <connection>scm:git:git://github.com/scalareflect/scalahost.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalareflect/scalahost/issues</url>
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

  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("scalahost.plugin.jar", jar.getAbsolutePath)
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  lazy val root = Project(
    id = "root",
    base = file("root")
  ) settings (
    sharedSettings : _*
  ) settings (
    test in Test := (test in tests in Test).value,
    packagedArtifacts := Map.empty
  ) aggregate (plugin, tests)

  lazy val foundation = Project(
    id   = "scalahost-foundation",
    base = file("foundation")
  ) settings (
    publishableSettings: _*
  ) settings (
    scalaSource in Compile <<= (baseDirectory in Compile)(base => base),
    libraryDependencies += "org.scalareflect" % "core_2.11" % "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalareflect" % "core-foundation_2.11" % "0.1.0-SNAPSHOT",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "provided"),
    packagedArtifacts := Map.empty,
    scalacOptions ++= Seq()
  )

  lazy val plugin = Project(
    id   = "scalahost",
    base = file("plugin")
  ) settings (
    publishableSettings ++ assemblySettings: _*
  ) settings (
    scalaSource in Compile <<= (baseDirectory in Compile)(base => base / "src"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _ % "provided"),
    libraryDependencies += "org.scalareflect" % "core_2.11" % "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalareflect" % "core-foundation_2.11" % "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalareflect" % "interpreter_2.11" % "0.1.0-SNAPSHOT",
    test in assembly := {},
    jarName in assembly := name.value + "_" + scalaVersion.value + "-" + version.value + "-assembly.jar",
    assemblyOption in assembly ~= { _.copy(includeScala = false) },
    Keys.`package` in Compile := {
      val slimJar = (Keys.`package` in Compile).value
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      println("package: merged scalahost and its dependencies and produced a fat JAR")
      slimJar
    },
    packagedArtifact in Compile in packageBin := {
      val temp = (packagedArtifact in Compile in packageBin).value
      val (art, slimJar) = temp
      val fatJar = new File(crossTarget.value + "/" + (jarName in assembly).value)
      val _ = assembly.value
      IO.copy(List(fatJar -> slimJar), overwrite = true)
      println("packagedArtifact: merged scalahost and its dependencies and produced a fat JAR")
      (art, slimJar)
    },
    // scalacOptions ++= Seq("-Xprint:typer"),
    scalacOptions ++= Seq()
  ) dependsOn (foundation)

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalareflect" % "core_2.11" % "0.1.0-SNAPSHOT",
    scalacOptions ++= Seq()
  )

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings ++ usePluginSettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalareflect" % "core_2.11" % "0.1.0-SNAPSHOT",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    scalacOptions in Test <++= (Keys.`package` in Compile) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    },
    packagedArtifacts := Map.empty,
    scalacOptions ++= Seq()
  ) dependsOn (plugin)
}
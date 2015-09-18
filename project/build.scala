import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object build extends Build {
  lazy val root = Project(
    id = "root",
    base = file("root")
  ) settings (
    sharedSettings : _*
  ) settings (
    test in Test := (test in tests in Test).value,
    packagedArtifacts := Map.empty
  ) aggregate (foundation, tokens, scalameta, scalahost, tests)

  lazy val foundation = Project(
    id   = "foundation",
    base = file("foundation")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Fundamental helpers and utilities of scala.meta",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
  )

  lazy val tokens = Project(
    id   = "tokens",
    base = file("tokens")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Tokenization and token quasiquotes of scala.meta",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value
    )
  ) dependsOn (foundation)

  lazy val scalameta = Project(
    id   = "scalameta",
    base = file("scalameta")
  ) settings (
    publishableSettings: _*
  ) settings (
    description := "Metaprogramming and hosting APIs of scala.meta",
    libraryDependencies ++= Seq(
      "org.apache.ivy" % "ivy" % "2.4.0",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
    )
  ) dependsOn (foundation, tokens)

  lazy val scalahost = Project(
    id   = "scalahost",
    base = file("scalahost")
  ) settings (
    publishableSettings: _*
  ) settings (
    mergeSettings: _*
  ) settings (
    crossVersion := CrossVersion.full,
    description := "Scalac-based host of scala.meta",
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
  ) dependsOn (scalameta)

  lazy val exampleCompiletime = Project(
    id   = "example-compiletime",
    base = file("example-compiletime"),
    settings = publishableSettings ++ mergeSettings ++ Seq(
      libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
    ) 
  ) dependsOn(scalameta, scalahost)

  lazy val exampleTests = Project(
    id   = "example-tests",
    base = file("example-tests"),
    settings = sharedSettings ++ Seq(
      usePlugin(exampleCompiletime),
      dontPackage
    ) ++ exposeClasspaths("tests")
  ) dependsOn (exampleCompiletime)

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) settings (
    sharedSettings: _*
  ) settings (
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalatest" %% "scalatest" % "2.1.3" % "test",
      "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
    ),
    packagedArtifacts := Map.empty
  ) settings (
    exposeClasspaths("tests"): _*
  ) dependsOn (foundation, scalameta, scalahost)

  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion := "2.11.7",
    crossVersion := CrossVersion.binary,
    version := "0.1.0-SNAPSHOT",
    organization := "org.scalameta",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    scalacOptions in (Compile, doc) ++= Seq("-skip-packages", "scala.meta.internal.ast:scala.meta.internal.semantic:scala.meta.internal.tql"),
    scalacOptions in (Compile, doc) ++= Seq("-implicits", "-implicits-hide:.,scala.meta.syntactic.Api.XtensionInputLike,scala.meta.ui.Api.XtensionShow"),
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
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  )

  lazy val dontPackage = packagedArtifacts := Map.empty

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

  def exposeClasspaths(projectName: String) = Seq(
    sourceDirectory in Test := {
      val defaultValue = (sourceDirectory in Test).value
      System.setProperty("sbt.paths.tests.sources", defaultValue.getAbsolutePath)
      defaultValue
    },
    resourceDirectory in Test := {
      val defaultValue = (resourceDirectory in Test).value
      System.setProperty("sbt.paths.tests.resources", defaultValue.getAbsolutePath)
      defaultValue
    },
    fullClasspath in Test := {
      val defaultValue = (fullClasspath in Test).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      val scalaLibrary = classpath.map(_.toString).find(_.contains("scala-library")).get
      System.setProperty("sbt.paths.scalalibrary.classes", scalaLibrary)
      System.setProperty("sbt.paths.tests.classes", classpath.mkString(java.io.File.pathSeparator))
      defaultValue
    }
  )

  def usePlugin(plugin: ProjectReference) =
    scalacOptions <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("sbt.paths.plugin.jar", jar.getAbsolutePath)
      Seq("-Xplugin:" + jar.getAbsolutePath, "-Jdummy=" + jar.lastModified)
    }
}

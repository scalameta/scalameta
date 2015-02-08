import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import com.typesafe.sbt.pgp.PgpKeys._

object Settings {
  lazy val languageVersion = "2.11.5"
  lazy val metaVersion = "0.1.0-SNAPSHOT"

  lazy val sharedSettings: Seq[sbt.Def.Setting[_]] = Defaults.defaultSettings ++ Seq(
    scalaVersion := languageVersion,
    crossVersion := CrossVersion.full,
    version := metaVersion,
    organization := "org.scalameta",
    description := "Scala host for scala.meta",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    commands += cls,
    scalaHome := {
      val scalaHome = System.getProperty("scalahost.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    },
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
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
      <url>https://github.com/scalameta/scalahost</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/scalameta/scalahost.git</url>
        <connection>scm:git:git://github.com/scalameta/scalahost.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalameta/scalahost/issues</url>
      </issueManagement>
      <developers>
        <developer>
          <id>xeno-by</id>
          <name>Eugene Burmako</name>
          <url>http://xeno.by</url>
        </developer>
      </developers>
    )
  )

  def cls = Command.command("cls") { state =>
    // NOTE: probably only works in iTerm2
    // kudos to http://superuser.com/questions/576410/how-can-i-partially-clear-my-terminal-scrollback
    print("\u001b]50;ClearScrollback\u0007")
    state
  }

  lazy val mergeDependencies: Seq[sbt.Def.Setting[_]] = assemblySettings ++ Seq(
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

  lazy val publishableSettings: Seq[sbt.Def.Setting[_]] = sharedSettings ++ Seq(
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

  lazy val dontPackage = packagedArtifacts := Map.empty

  // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
  // add plugin timestamp to compiler options to trigger recompile of
  // main after editing the plugin. (Otherwise a 'clean' is needed.)
  def usePlugin(plugin: ProjectReference) =
    scalacOptions <++= (Keys.`package` in (plugin, Compile)) map { (jar: File) =>
      System.setProperty("sbt.paths.plugin.jar", jar.getAbsolutePath)
      Seq("-Xplugin:" + jar.getAbsolutePath, "-Jdummy=" + jar.lastModified)
    }

  def exposeClasspaths(projectName: String) = Seq(
    fullClasspath in Test := {
      val defaultValue = (fullClasspath in Test).value
      val classpath = defaultValue.files.map(_.getAbsolutePath)
      System.setProperty("sbt.paths.tests.classpath", classpath.mkString(java.io.File.pathSeparatorChar.toString))
      defaultValue
    },
    resourceDirectory in Test := {
      val defaultValue = (resourceDirectory in Test).value
      System.setProperty("sbt.paths.tests.resources", defaultValue.getAbsolutePath)
      defaultValue
    }
  )

  lazy val replIntegration = initialCommands in console := """
    import scala.meta._
    import scala.meta.internal.hosts.scalac.Scalahost
    val options = "-Xplugin:" + sys.props("sbt.paths.plugin.jar") + " -Xplugin-require:scalahost"
    implicit val c = Scalahost.mkToolboxContext(scala.reflect.runtime.currentMirror, options = options)
  """
}
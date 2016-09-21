import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import com.typesafe.sbt.pgp.PgpKeys._
import sbtunidoc.Plugin._
import UnidocKeys._
import java.io._
import org.scalameta.os._
import scala.compat.Platform.EOL

object build extends Build {
  lazy val ScalaVersions = Seq("2.11.8")
  lazy val LibraryVersion = "2.0.0-SNAPSHOT"

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
    publish := {
      // Others projects are published automatically because we aggregate.
      val publishDocs = (publish in readme).value
    },
    // TODO: The same thing for publishSigned doesn't work.
    // SBT calls publishSigned on aggregated projects, but ignores everything else.
    console := (console in scalameta in Compile).value
  ) aggregate (
    artifacts,
    common,
    dialects,
    inline,
    inputs,
    parsers,
    quasiquotes,
    scalameta,
    semantic,
    tokenizers,
    tokens,
    transversers,
    trees
  )

  lazy val artifacts = Project(
    id   = "artifacts",
    base = file("scalameta/artifacts")
  ) settings (
    publishableSettings,
    description := "Scala.meta's APIs for reflecting artifacts of Scala ecosystem",
    libraryDependencies += "org.apache.ivy" % "ivy" % "2.4.0",
    libraryDependencies += "org.scala-lang.modules" % "scala-asm" % "5.1.0-scala-1",
    enableMacros
  ) dependsOn (common, parsers, trees)

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
    libraryDependencies += "com.lihaoyi" %% "scalaparse" % "0.3.7",
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
  ) dependsOn (common, dialects, parsers, quasiquotes, tokenizers, transversers, trees, inline, artifacts, semantic)

  lazy val semantic = Project(
    id   = "semantic",
    base = file("scalameta/semantic")
  ) settings (
    publishableSettings,
    description := "Scala.meta's semantic APIs"
  ) dependsOn (common, trees, artifacts)

  lazy val readme = scalatex.ScalatexReadme(
    projectId = "readme",
    wd = file(""),
    url = "https://github.com/scalameta/scalameta/tree/master",
    source = "Readme"
  ) settings (
    exposePaths("readme", Runtime),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    // Workaround for https://github.com/lihaoyi/Scalatex/issues/25
    dependencyOverrides += "com.lihaoyi" %% "scalaparse" % "0.3.1",
    sources in Compile ++= List("os.scala").map(f => baseDirectory.value / "../project" / f),
    watchSources ++= baseDirectory.value.listFiles.toList,
    publish := {
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

      // make sure that we have a stable reference to the working copy that produced the website
      val currentSha = shell.check_output("git rev-parse HEAD", cwd = ".")
      val changed = shell.check_output("git diff --name-status", cwd = ".")
      if (changed.trim.nonEmpty) sys.error("repository " + new File(".").getAbsolutePath + " is dirty (has modified files)")
      val staged = shell.check_output("git diff --staged --name-status", cwd = ".")
      if (staged.trim.nonEmpty) sys.error("repository " + new File(".").getAbsolutePath + " is dirty (has staged files)")
      val untracked = shell.check_output("git ls-files --others --exclude-standard", cwd = ".")
      if (untracked.trim.nonEmpty) sys.error("repository " + new File(".").getAbsolutePath + " is dirty (has untracked files)")
      val (exitcode, stdout, stderr) = shell.exec(s"git branch -r --contains $currentSha")
      if (exitcode != 0 || stdout.isEmpty) sys.error("repository " + new File(".").getAbsolutePath + " doesn't contain commit " + currentSha)

      // commit and push the changes if any
      shell.call(s"git add -A", cwd = repo.getAbsolutePath)
      val nothingToCommit = "nothing to commit, working directory clean"
      try {
        val currentUrl = s"https://github.com/scalameta/scalameta/tree/" + currentSha.trim
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

  lazy val sharedSettings = Def.settings(
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
    scalacOptions ++= Seq("-Xfatal-warnings"),
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

  lazy val publishableSettings = Def.settings(
    sharedSettings,
    publishArtifact in Compile := true,
    publishArtifact in Test := false,
    credentials ++= secret.obtain("sonatype").map({
      case (username, password) => Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
    }).toList
  )

  lazy val mergeSettings: Seq[sbt.Def.Setting[_]] = Def.settings(
    assemblySettings,
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
}

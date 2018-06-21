package scala.meta.tests.semanticdb

import bloop.Cli
import bloop.cli.CommonOptions
import bloop.cli.ExitStatus
import bloop.config.Config.Mixed
import bloop.engine.NoPool
import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Paths
import scala.meta.io.AbsolutePath
import scala.meta.testkit.StringFS
import bloop.config.{Config => C}
import scala.meta.internal.io.FileIO
import scala.meta.internal.semanticdb.Index

/**
  * Utility class to reproduce incremental compilation with Zinc via Bloop.
  *
  * @param debug If true, prints out compilation logs to stdout.
  *              If false, is silent and prints nothing to stdout.
  */
class ZincProject(debug: Boolean = false) {
  private val tmp = Files.createTempDirectory("scalameta")
  val sourceroot: AbsolutePath = AbsolutePath(tmp)
  private val out = tmp.resolve("out")
  val targetroot: AbsolutePath = AbsolutePath(out.resolve("classes"))
  private val project: String = "name"
  setupBloop()

  /** Compile and throw error if compilation succeeded. */
  def assertDoesNotCompile(layout: String): Unit = {
    assertCompilesWithExitStatus(layout, ExitStatus.CompilationError)
  }

  /** Compile and throw error if compilation was not successful. */
  def assertCompiles(layout: String): Unit = {
    assertCompilesWithExitStatus(layout, ExitStatus.Ok)
  }

  /** Apply the StringFS markup changes to the sourceroot directory. */
  def applyFileChanges(changes: String): AbsolutePath = {
    StringFS.fromString(changes, sourceroot)
  }

  private def run(args: String*): (ExitStatus, String) = {
    val out = new ByteArrayOutputStream()
    val ps = new PrintStream(out)
    val common = CommonOptions(
      workingDirectory = tmp.toString,
      out = ps,
      err = ps,
      ngout = ps,
      ngerr = ps
    )
    val action = Cli.parse(args.toArray, common)
    val exit = Cli.run(action, NoPool, args.toArray)
    exit -> out.toString()
  }

  private def compile(): ExitStatus = {
    val (exit, out) = run("compile", project)
    if (debug) {
      println(out)
    }
    exit
  }

  private def assertCompilesWithExitStatus(layout: String, expected: ExitStatus): Unit = {
    applyFileChanges(layout)
    val exit = compile()
    assert(exit == expected, s"Expected $expected. Obtained $exit")
  }

  private def setupBloop(): Unit = {
    val classpath = this.getClass.getClassLoader match {
      case u: URLClassLoader =>
        u.getURLs.map(url => Paths.get(url.toURI))
    }
    val pluginjar = sys.props("sbt.paths.semanticdb-scalac-plugin.compile.jar")
    val scalacOptions = Array(
      "-Yrangepos",
      s"-P:semanticdb:sourceroot:$tmp",
      "-P:semanticdb:failures:error",
      "-Xplugin:" + pluginjar,
      "-Xplugin-require:semanticdb"
    )
    val file = new C.File(
      "1.0.0",
      C.Project(
        name = project,
        directory = tmp,
        sources = Array(tmp.resolve("src")),
        dependencies = Array(),
        classpath = classpath,
        out = out,
        analysisOut = out.resolve("analysis"),
        classesDir = targetroot.toNIO,
        scala = C.Scala(
          organization = "org.scala-lang",
          name = "scala-compiler",
          version = scala.util.Properties.versionNumberString,
          options = scalacOptions,
          jars = Array()),
        java = C.Java(Array()),
        sbt = C.Sbt("1.2.0", Nil),
        test = C.Test(Array(), C.TestOptions(Nil, Nil)),
        platform = C.Platform.Jvm(C.JvmConfig(None, Nil)),
        compileSetup = C.CompileSetup(
          Mixed,
          addLibraryToBootClasspath = true,
          addCompilerToClasspath = false,
          addExtraJarsToClasspath = false,
          manageBootClasspath = true,
          filterLibraryFromClasspath = true
        ),
        resolution = C.Resolution(Nil)
      )
    )
    val configDir = tmp.resolve(".bloop")
    Files.createDirectories(configDir)
    bloop.config.write(file, configDir.resolve("bloop.json"))
  }

}

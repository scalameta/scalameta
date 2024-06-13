package scala.meta.tests.semanticdb

import scala.meta.cli._
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import scala.meta.metac._
import scala.meta.tests.BuildInfo
import scala.meta.tests.SkipWindows
import scala.meta.tests.Slow

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Paths

import scala.concurrent.duration

import munit.FunSuite

class MetacScalaLibrary extends FunSuite {

  override val munitTimeout = new duration.FiniteDuration(3, duration.MINUTES)

  test("compile scala-library".tag(Slow).tag(SkipWindows)) {
    val exit = MetacScalaLibrary.process(Array())
    require(exit == 0, "failed to compile scala-library")
  }
}
object MetacScalaLibrary {
  def main(args: Array[String]): Unit = sys.exit(process(args))
  // Compile all of scala-library with metac and report any semanticdb errors.
  def process(args: Array[String]): Int = {
    val library = Paths.get("target").resolve("scala-library")
      .resolve(s"scala-${BuildInfo.scalaVersion}").resolve("src").resolve("library").toAbsolutePath
    assert(
      Files.isDirectory(library),
      s"$library is not a directory! Run `sbt download-scala-library`"
    )
    val classpath = this.getClass.getClassLoader.asInstanceOf[URLClassLoader].getURLs.map(_.getPath)
      .filter(_.contains("scala-library")).mkString(File.pathSeparator)
    val files = FileIO.listAllFilesRecursively(AbsolutePath(library)).map(_.toString)
      .filter(_.endsWith("scala"))
    val out = Files.createTempDirectory("metac")
    println(s"Compiling ${files.length} sources from scala-library...")
    val scalacArgs =
      List("-d", out.toString, "-classpath", classpath, "-P:semanticdb:failures:error") ++ files
    val settings = Settings().withScalacArgs(scalacArgs)
    val reporter = Reporter()
    val success = Metac.process(settings, reporter)
    println(out)
    if (success) 0 else 1
  }
}

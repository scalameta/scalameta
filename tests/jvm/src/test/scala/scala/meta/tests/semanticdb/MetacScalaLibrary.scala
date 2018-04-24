package scala.meta.tests.semanticdb

import java.io.File
import java.net.URLClassLoader
import java.nio.file.Files
import java.nio.file.Paths
import scala.meta.cli._
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath
import scala.meta.metac._
import scala.meta.tests.BuildInfo

// Compile all of scala-library with metac and report any semanticdb errors.
object MetacScalaLibrary {
  def main(args: Array[String]): Unit = {
    val library = Paths
      .get("target")
      .resolve("scala-library")
      .resolve(s"scala-${BuildInfo.scalaVersion}")
      .resolve("src")
      .resolve("library")
      .toAbsolutePath
    assert(Files.isDirectory(library), s"$library is not a directory! Run `sbt ci-metac`")
    val classpath = this.getClass.getClassLoader
      .asInstanceOf[URLClassLoader]
      .getURLs
      .map(_.getPath)
      .filter(_.contains("scala-library"))
      .mkString(File.pathSeparator)
    val files = FileIO
      .listAllFilesRecursively(AbsolutePath(library))
      .map(_.toString)
      .filter(_.endsWith("scala"))
    val out = Files.createTempDirectory("metac")
    println(s"Compiling ${files.length} sources from scala-library...")
    val scalacArgs = List(
      "-d",
      out.toString,
      "-classpath",
      classpath,
      "-P:semanticdb:failures:error"
    ) ++ files
    val settings = Settings().withScalacArgs(scalacArgs)
    val reporter = Reporter()
    val success = Metac.process(settings, reporter)
    println(out)
    sys.exit(if (success) 0 else 1)
  }
}

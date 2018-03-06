package scala.meta.cli

import java.io._

import io.github.soc.directories.ProjectDirectories
import org.langmeta.io.AbsolutePath

import scala.meta.internal.metacp._

object Metacp {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, System.out, System.err))
  }

  def process(args: Array[String]): Int = {
    process(args, System.out, System.err)
  }

  def process(args: Array[String], out: PrintStream, err: PrintStream): Int = {
    Settings.parse(args.toList) match {
      case Some(settings) =>
        val main = new Main(settings, out, err)
        main.process()
      case None =>
        1
    }
  }

  def process(classpath: List[AbsolutePath], cache: AbsolutePath): List[AbsolutePath] = {
    classpath.toParArray.map(p => process(p, cache)).toList
  }

  def process(in: AbsolutePath, cache: AbsolutePath): AbsolutePath = {
    if (in.isDirectory) in
    else if (!in.isFile) throw new IllegalArgumentException(s"$in is not a regular file")
    else {
      val checksum = Checksum(in)
      val out = cacheFile(cache, checksum)
      if (!out.isDirectory) {
        process(
          Array("-cp", in.toString, "-d", out.toString),
          System.out,
          System.err
        )
      }
      out
    }
  }

  def scalaLibrarySynthetics(scalaVersion: String, cache: AbsolutePath): AbsolutePath = {
    val out = cacheFile(cache, "scala-library-synthetics")
    if (!out.isDirectory) {
      ScalaLibrarySynthetics.process(scalaVersion, out)
    }
    out
  }

  def bootClasspath(cache: AbsolutePath): List[AbsolutePath] = {
    val classpath = sys.props
      .collectFirst {
        case (k, v) if k.endsWith(".boot.class.path") =>
          v.split(java.io.File.pathSeparatorChar)
            .iterator
            .map(p => AbsolutePath(p))
            .filter(_.isFile)
            .toList
      }
      .getOrElse(sys.error("failed to detect JDK classpath"))
    process(classpath, cache)
  }

  def defaultCacheDir: AbsolutePath = AbsolutePath(
    ProjectDirectories.fromProjectName("semanticdb").projectCacheDir
  )

  private def cacheFile(cache: AbsolutePath, name: String): AbsolutePath =
    cache.resolve(BuildInfo.version).resolve(name)
}

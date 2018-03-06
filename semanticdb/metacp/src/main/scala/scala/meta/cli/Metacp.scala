package scala.meta.cli

import java.io._
import java.nio.file.Files

import io.github.soc.directories.ProjectDirectories
import org.langmeta.internal.io.PlatformFileIO
import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath

import scala.meta.internal.metacp._

object Metacp {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, System.out, System.err))
  }

  def process(args: Array[String]): Int = {
    process(args, System.out, System.err)
  }

  def process(args: Array[String], out: PrintStream, err: PrintStream): Int = {
    Settings.parse(args.toList, out, err) match {
      case Some(settings) =>
        val mclasspath = process(settings)
        out.println(mclasspath.mkString(File.pathSeparator))
        0
      case None =>
        1
    }
  }

  def process(settings: Settings): List[AbsolutePath] = {
    settings.classpath.shallow.toParArray.map(p => processPath(p, settings)).toList
  }

  def processPath(in: AbsolutePath, settings: Settings): AbsolutePath = {
    if (in.isDirectory) {
      val out = settings.directories match {
        case DirectoryOutput.TempDirectory =>
          AbsolutePath(Files.createTempDirectory("semanticdb"))
        case DirectoryOutput.InPlace =>
          in
      }
      val exit = Main.process(settings.copy(classpath = Classpath(in :: Nil), d = out))
      assert(exit == 0, s"Failed to process $in")
      out
    } else if (!in.isFile) {
      throw new IllegalArgumentException(s"$in is not a regular file")
    } else {
      val checksum = Checksum(in)
      val out =
        cacheFile(settings, in.toNIO.getFileName.toString.stripSuffix(".jar") + "-" + checksum)
      if (!out.isFile) {
        PlatformFileIO.withJarFileSystem(out) { jar =>
          val exit = Main.process(settings.copy(classpath = Classpath(in :: Nil), d = jar))
          assert(exit == 0, s"Failed to process $in")
        }
      }
      out
    }
  }

  def scalaLibrarySynthetics(scalaVersion: String, settings: Settings): AbsolutePath = {
    val out = cacheFile(settings, "scala-library-synthetics")
    if (!out.isDirectory) {
      PlatformFileIO.withJarFileSystem(out) { jar =>
        ScalaLibrarySynthetics.process(scalaVersion, jar)
      }
    }
    out
  }

  def bootClasspath(settings: Settings): List[AbsolutePath] = {
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
    process(settings.copy(classpath = Classpath(classpath)))
  }

  def defaultCacheDir: AbsolutePath = AbsolutePath(
    ProjectDirectories.fromProjectName("semanticdb").projectCacheDir
  )

  private def cacheFile(settings: Settings, name: String): AbsolutePath =
    settings.d.resolve(BuildInfo.version).resolve(name + ".jar")
}

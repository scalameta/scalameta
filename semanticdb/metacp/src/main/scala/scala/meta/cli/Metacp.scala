package scala.meta.cli

import java.io._
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter

import io.github.soc.directories.ProjectDirectories
import org.langmeta.io.AbsolutePath

import scala.meta.internal.metacp._

object Metacp {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, System.out, System.err))
  }

  @deprecated("Use `process(args, System.out, System.err)` instead.", "3.4.0")
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

  def toMetacp(classpath: List[Path]): List[Path] = {
    classpath.toParArray.map(toMetacp).toList
  }

  def toMetacp(in: Path): Path = {
    if (Files.isDirectory(in)) in
    else {
      val hash = checksum(in)
      val out = cacheFile(hash)
      if (!Files.isDirectory(out)) {
        process(
          Array("-cp", in.toString, "-d", out.toString),
          System.out,
          System.err
        )
      }
      out
    }
  }

  def scalalib: Path = {
    val out = cacheFile("scalalib")
    if (!Files.isDirectory(out)) {
      Scalalib.process(out)
    }
    out
  }

  def jdk: List[Path] =
    sys.props
      .collectFirst {
        case (k, v) if k.endsWith(".boot.class.path") =>
          v.split(java.io.File.pathSeparatorChar)
            .iterator
            .map(p => Paths.get(p))
            .filter(Files.isRegularFile(_))
            .toList
      }
      .getOrElse(sys.error("failed to detect JDK classpath"))

  private def checksum(fileOrDir: Path): String = {
    val digest = MessageDigest.getInstance("md5")
    if (Files.isRegularFile(fileOrDir)) checksumFile(fileOrDir, digest)
    else checksumDirectory(fileOrDir, digest)
    val hash = DatatypeConverter.printHexBinary(digest.digest())
    hash
  }

  private def checksumDirectory(dir: Path, digest: MessageDigest): Unit = {
    checksumFile(dir, digest)
  }

  private def checksumFile(file: Path, digest: MessageDigest): Unit = {
    digest.update(file.toString.getBytes())
    digest.update(Files.getLastModifiedTime(file).toMillis.toByte)
    digest.update(Files.size(file).toByte)
  }

  private def cacheDir: Path = Paths.get(
    ProjectDirectories.fromProjectName("scalameta").projectCacheDir
  )

  private def cacheFile(hash: String): Path =
    cacheDir.resolve(BuildInfo.version).resolve(hash)

}

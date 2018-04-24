package scala.meta.metacp

import io.github.soc.directories.ProjectDirectories
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.internal.metacp.BuildInfo

final class Settings private (
    val cacheDir: AbsolutePath,
    val classpath: Classpath,
    val scalaLibrarySynthetics: Boolean,
    val par: Boolean
) {
  private def this() = {
    this(
      cacheDir = Settings.defaultCacheDir,
      classpath = Classpath(Nil),
      scalaLibrarySynthetics = false,
      par = false
    )
  }

  def withCacheDir(cacheDir: AbsolutePath): Settings = {
    copy(cacheDir = cacheDir)
  }

  def withClasspath(classpath: Classpath): Settings = {
    copy(classpath = classpath)
  }

  def withScalaLibrarySynthetics(include: Boolean): Settings = {
    copy(scalaLibrarySynthetics = include)
  }

  def withPar(par: Boolean): Settings = {
    copy(par = par)
  }

  private def copy(
      cacheDir: AbsolutePath = cacheDir,
      classpath: Classpath = classpath,
      scalaLibrarySynthetics: Boolean = scalaLibrarySynthetics,
      par: Boolean = par
  ): Settings = {
    new Settings(
      cacheDir = cacheDir,
      classpath = classpath,
      scalaLibrarySynthetics = scalaLibrarySynthetics,
      par = par
    )
  }
}

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--cache-dir" +: cacheDir +: rest if allowOptions =>
          loop(settings.copy(cacheDir = AbsolutePath(cacheDir)), true, rest)
        case "--exclude-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(scalaLibrarySynthetics = false), true, rest)
        case "--include-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(scalaLibrarySynthetics = true), true, rest)
        case "--par" +: rest if allowOptions =>
          loop(settings.copy(par = true), true, rest)
        case flag +: _ if allowOptions && flag.startsWith("-") =>
          reporter.out.println(s"unsupported flag $flag")
          None
        case classpath +: Nil =>
          Some(settings.copy(classpath = Classpath(classpath)))
        case classpath +: arg +: _ =>
          reporter.out.println(s"unsupported argument $arg")
          None
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), allowOptions = true, args)
  }

  def defaultCacheDir: AbsolutePath = {
    val cacheRoot = AbsolutePath(ProjectDirectories.fromProjectName("semanticdb").projectCacheDir)
    cacheRoot.resolve(BuildInfo.version)
  }

  def apply(): Settings = {
    new Settings()
  }
}

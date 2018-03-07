package scala.meta.metacp

import java.io.PrintStream
import io.github.soc.directories.ProjectDirectories
import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath
import scala.meta.cli.Metacp
import scala.meta.internal.metacp.BuildInfo

final class Settings private (
    val cacheDir: AbsolutePath,
    val classpath: Classpath,
    val includeScalaLibrarySynthetics: Boolean
) {
  private def this() = {
    this(
        cacheDir = Settings.defaultCacheDir,
        classpath = Classpath(Nil),
        includeScalaLibrarySynthetics = false
    )
  }

  def withCacheDir(cacheDir: AbsolutePath): Settings = {
    copy(cacheDir = cacheDir)
  }

  def withClasspath(classpath: Classpath): Settings = {
    copy(classpath = classpath)
  }

  def withIncludeScalaLibrarySynthetics(include: Boolean): Settings = {
    copy(includeScalaLibrarySynthetics = include)
  }

  private def copy(
      cacheDir: AbsolutePath = cacheDir,
      classpath: Classpath = classpath,
      includeScalaLibrarySynthetics: Boolean = includeScalaLibrarySynthetics): Settings = {
    new Settings(
      cacheDir = cacheDir,
      classpath = classpath,
      includeScalaLibrarySynthetics = includeScalaLibrarySynthetics
    )
  }
}

object Settings {
  def parse(args: List[String]): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--cache-dir" +: cacheDir +: rest if allowOptions =>
          loop(settings.copy(cacheDir = AbsolutePath(cacheDir)), true, rest)
        case "--exclude-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(includeScalaLibrarySynthetics = false), true, rest)
        case "--include-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(includeScalaLibrarySynthetics = true), true, rest)
        case flag +: _ if allowOptions && flag.startsWith("-") =>
          println(s"unsupported flag $flag")
          None
        case classpath +: Nil =>
          Some(settings.copy(classpath = Classpath(classpath)))
        case classpath +: arg +: _ =>
          println(s"unsupported argument $arg")
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

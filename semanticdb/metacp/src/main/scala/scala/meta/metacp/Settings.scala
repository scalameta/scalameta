package scala.meta.metacp

import io.github.soc.directories.ProjectDirectories
import scala.meta.cli._
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath
import scala.meta.internal.metacp.BuildInfo

final class Settings private (
    val cacheDir: AbsolutePath,
    val classpath: Classpath,
    val dependencyClasspath: Classpath,
    val scalaLibrarySynthetics: Boolean,
    val par: Boolean,
    val assumeJava: Set[String],
    val assumeScala: Set[String]
) {
  private def this() = {
    this(
      cacheDir = Settings.defaultCacheDir,
      classpath = Classpath(Nil),
      dependencyClasspath = Classpath(Nil),
      scalaLibrarySynthetics = false,
      par = false,
      assumeJava = Set("java"),
      assumeScala = Set.empty
    )
  }

  def fullClasspath: Classpath =
    Classpath(classpath.entries ++ dependencyClasspath.entries)

  def withCacheDir(cacheDir: AbsolutePath): Settings = {
    copy(cacheDir = cacheDir)
  }

  def withClasspath(classpath: Classpath): Settings = {
    copy(classpath = classpath)
  }

  def withDependencyClasspath(classpath: Classpath): Settings = {
    copy(dependencyClasspath = classpath)
  }

  def withScalaLibrarySynthetics(include: Boolean): Settings = {
    copy(scalaLibrarySynthetics = include)
  }

  def withAssumeJava(assumeJava: Set[String]): Settings = {
    copy(assumeJava = assumeJava)
  }

  def withAssumeScala(assumeScala: Set[String]): Settings = {
    copy(assumeScala = assumeScala)
  }

  def withPar(par: Boolean): Settings = {
    copy(par = par)
  }

  private def copy(
      cacheDir: AbsolutePath = cacheDir,
      classpath: Classpath = classpath,
      dependencyClasspath: Classpath = dependencyClasspath,
      scalaLibrarySynthetics: Boolean = scalaLibrarySynthetics,
      par: Boolean = par,
      assumeJava: Set[String] = assumeJava,
      assumeScala: Set[String] = assumeScala
  ): Settings = {
    new Settings(
      cacheDir = cacheDir,
      classpath = classpath,
      dependencyClasspath = dependencyClasspath,
      scalaLibrarySynthetics = scalaLibrarySynthetics,
      par = par,
      assumeJava = assumeJava,
      assumeScala = assumeScala
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
        case "--dependency-classpath" +: dependencyClasspath +: rest if allowOptions =>
          loop(settings.copy(dependencyClasspath = Classpath(dependencyClasspath)), true, rest)
        case "--exclude-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(scalaLibrarySynthetics = false), true, rest)
        case "--include-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(scalaLibrarySynthetics = true), true, rest)
        case "--par" +: rest if allowOptions =>
          loop(settings.copy(par = true), true, rest)
        case "--assume-java" +: fqn +: rest if allowOptions =>
          loop(settings.copy(assumeJava = settings.assumeJava + fqn), true, rest)
        case "--assume-scala" +: fqn +: rest if allowOptions =>
          loop(settings.copy(assumeScala = settings.assumeScala + fqn), true, rest)
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
    val projectDirectories = ProjectDirectories.from("org.scalameta", "", "SemanticDB")
    val cacheRoot = AbsolutePath(projectDirectories.cacheDir)
    cacheRoot.resolve(BuildInfo.version)
  }

  def apply(): Settings = {
    new Settings()
  }
}

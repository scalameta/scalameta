package scala.meta.metacp

import scala.meta.cli._
import scala.meta.internal.io.PathIO
import scala.meta.io.AbsolutePath
import scala.meta.io.Classpath

final class Settings private (
    val out: AbsolutePath,
    val classpath: Classpath,
    val dependencyClasspath: Classpath,
    val scalaLibrarySynthetics: Boolean,
    val par: Boolean,
    val verbose: Boolean,
    val usejavacp: Boolean,
    val stubBrokenSignatures: Boolean,
    val logBrokenSignatures: Boolean
) {
  private def this() = {
    this(
      out = Settings.defaultOut,
      classpath = Classpath(Nil),
      dependencyClasspath = Classpath(Nil),
      scalaLibrarySynthetics = false,
      par = false,
      verbose = false,
      usejavacp = false,
      stubBrokenSignatures = false,
      logBrokenSignatures = false
    )
  }

  def withOut(out: AbsolutePath): Settings = {
    copy(out = out)
  }

  @deprecated("Use withOut instead", "4.0.0")
  def withCacheDir(out: AbsolutePath): Settings = {
    copy(out = out)
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

  def withPar(par: Boolean): Settings = {
    copy(par = par)
  }

  def withVerbose(verbose: Boolean): Settings = {
    copy(verbose = verbose)
  }

  def withUsejavacp(usejavacp: Boolean): Settings = {
    copy(usejavacp = usejavacp)
  }

  def withStubBrokenSignatures(stubBrokenSignatures: Boolean): Settings = {
    copy(stubBrokenSignatures = stubBrokenSignatures)
  }

  def withLogBrokenSignatures(logBrokenSignatures: Boolean): Settings = {
    copy(logBrokenSignatures = logBrokenSignatures)
  }

  private def copy(
      out: AbsolutePath = out,
      classpath: Classpath = classpath,
      dependencyClasspath: Classpath = dependencyClasspath,
      scalaLibrarySynthetics: Boolean = scalaLibrarySynthetics,
      par: Boolean = par,
      verbose: Boolean = verbose,
      usejavacp: Boolean = usejavacp,
      stubBrokenSignatures: Boolean = stubBrokenSignatures,
      logBrokenSignatures: Boolean = logBrokenSignatures
  ): Settings = {
    new Settings(
      out = out,
      classpath = classpath,
      dependencyClasspath = dependencyClasspath,
      scalaLibrarySynthetics = scalaLibrarySynthetics,
      par = par,
      verbose = verbose,
      usejavacp = usejavacp,
      stubBrokenSignatures = stubBrokenSignatures,
      logBrokenSignatures = logBrokenSignatures
    )
  }
}

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--out" +: out +: rest if allowOptions =>
          loop(settings.copy(out = AbsolutePath(out)), true, rest)
        case "--cache-dir" +: _ +: _ if allowOptions =>
          reporter.err.println("--cache-dir is deprecated, use --out instead")
          None
        case "--dependency-classpath" +: dependencyClasspath +: rest if allowOptions =>
          loop(settings.copy(dependencyClasspath = Classpath(dependencyClasspath)), true, rest)
        case "--exclude-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(scalaLibrarySynthetics = false), true, rest)
        case "--include-scala-library-synthetics" +: rest if allowOptions =>
          loop(settings.copy(scalaLibrarySynthetics = true), true, rest)
        case "--par" +: rest if allowOptions =>
          loop(settings.copy(par = true), true, rest)
        case "--verbose" +: rest if allowOptions =>
          loop(settings.copy(verbose = true), true, rest)
        case "--usejavacp" +: rest if allowOptions =>
          loop(settings.copy(usejavacp = true), true, rest)
        case "--stub-broken-signatures" +: rest if allowOptions =>
          loop(settings.copy(stubBrokenSignatures = true), true, rest)
        case "--log-broken-signatures" +: rest if allowOptions =>
          loop(settings.copy(logBrokenSignatures = true), true, rest)
        case flag +: _ if allowOptions && flag.startsWith("-") =>
          reporter.err.println(s"unsupported flag $flag")
          None
        case classpath +: Nil =>
          Some(settings.copy(classpath = Classpath(classpath)))
        case classpath +: arg +: _ =>
          reporter.err.println(s"unsupported argument $arg")
          None
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), allowOptions = true, args)
  }

  def defaultOut: AbsolutePath = {
    PathIO.workingDirectory.resolve("out")
  }

  def apply(): Settings = {
    new Settings()
  }
}

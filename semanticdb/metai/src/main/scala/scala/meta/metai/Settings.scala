package scala.meta.metai

import scala.meta.cli.Reporter
import scala.meta.io.Classpath

final class Settings private (val classpath: Classpath, val verbose: Boolean) {
  private def this() = {
    this(Classpath(Nil), false)
  }

  def withClasspath(classpath: Classpath): Settings = {
    copy(classpath = classpath)
  }

  def withVerbose(verbose: Boolean): Settings = {
    copy(verbose = verbose)
  }

  private def copy(
      classpath: Classpath = this.classpath,
      verbose: Boolean = this.verbose): Settings = {
    new Settings(classpath, verbose)
  }
}

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--verbose" +: rest if allowOptions =>
          loop(settings.copy(verbose = true), true, rest)
        case flag +: _ if allowOptions && flag.startsWith("-") =>
          reporter.err.println(s"unsupported flag $flag")
          None
        case s_cp +: rest =>
          Some(settings.copy(classpath = settings.classpath ++ Classpath(s_cp)))
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), allowOptions = true, args)
  }

  def apply(): Settings = {
    new Settings()
  }
}

package scala.meta.metap

import java.io._
import java.nio.file._
import scala.meta.cli._

final class Settings private (
    val format: Format,
    val paths: List[Path]
) {
  private def this() = {
    this(format = Format.Compact, paths = Nil)
  }

  def withFormat(format: Format): Settings = {
    copy(format = format)
  }

  def withPaths(paths: List[Path]): Settings = {
    copy(paths = paths)
  }

  private def copy(format: Format = format, paths: List[Path] = paths): Settings = {
    new Settings(format = format, paths = paths)
  }
}

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, args)
        case "-compact" +: rest if allowOptions =>
          loop(settings.copy(format = Format.Compact), allowOptions = true, rest)
        case ("-detailed" | "-pretty") +: rest if allowOptions =>
          loop(settings.copy(format = Format.Detailed), allowOptions = true, rest)
        case "-proto" +: rest if allowOptions =>
          loop(settings.copy(format = Format.Proto), allowOptions = true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          reporter.err.println(s"unknown flag $flag")
          None
        case path +: rest =>
          val paths1 = settings.paths ++ path.split(File.pathSeparator).map(Paths.get(_))
          loop(settings.copy(paths = paths1), allowOptions = true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args)
  }

  def apply(): Settings = {
    new Settings()
  }
}

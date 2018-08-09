package scala.meta.metadiff

import java.io.File
import java.nio.file.{Path, Paths}
import scala.meta.cli.Reporter

final case class Settings(
    paths: List[Path] = List(),
    compareSymbols: Boolean = true,
    compareOccurrences: Boolean = true,
    compareSynthetics: Boolean = true,
    compareOrder: Boolean = true
)

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    def loop(
        settings: Settings,
        allowOptions: Boolean,
        args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "--syms" +: rest if allowOptions =>
          loop(settings.copy(compareSymbols = true), true, rest)
        case "--no-syms" +: rest if allowOptions =>
          loop(settings.copy(compareSymbols = false), true, rest)
        case "--occs" +: rest if allowOptions =>
          loop(settings.copy(compareOccurrences = true), true, rest)
        case "--no-occs" +: rest if allowOptions =>
          loop(settings.copy(compareOccurrences = false), true, rest)
        case "--synths" +: rest if allowOptions =>
          loop(settings.copy(compareSynthetics = true), true, rest)
        case "--no-synths" +: rest if allowOptions =>
          loop(settings.copy(compareSynthetics = false), true, rest)
        case "--order" +: rest if allowOptions =>
          loop(settings.copy(compareOrder = true), true, rest)
        case "--no-order" +: rest if allowOptions =>
          loop(settings.copy(compareOrder = false), true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          reporter.out.println(s"unknown flag $flag")
          None
        case pathStr +: rest =>
          val path = pathStr.split(File.pathSeparator).map(Paths.get(_))
          val paths1 = settings.paths ++ path
          loop(settings.copy(paths = paths1), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args).filter { settings =>
      if (settings.paths.length != 2)
        reporter.out.println("Expected exactly two paths to diff")
      settings.paths.length == 2
    }
  }
}

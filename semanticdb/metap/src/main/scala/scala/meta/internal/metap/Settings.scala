package scala.meta.internal.metap

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

final case class Settings(
    paths: Vector[Path] = Vector.empty,
    format: OutputFormat = OutputFormat.Pretty
)

sealed trait OutputFormat {
  def isPretty: Boolean = this == OutputFormat.Pretty
  def isProto: Boolean = this == OutputFormat.Proto
}
object OutputFormat {
  case object Pretty extends OutputFormat
  case object Proto extends OutputFormat
}

object Settings {
  def parse(args: List[String]): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, args)
        case "-pretty" +: rest if allowOptions =>
          loop(settings.copy(format = OutputFormat.Pretty), allowOptions = true, rest)
        case "-proto" +: rest if allowOptions =>
          loop(settings.copy(format = OutputFormat.Proto), allowOptions = true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          println(s"unknown flag $flag")
          None
        case path +: rest =>
          val paths1 = settings.paths ++ path.split(File.pathSeparator).map(Paths.get(_))
          loop(settings.copy(paths = paths1), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args)
  }
}

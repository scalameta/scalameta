package scala.meta.internal.metacp

import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

final case class Settings(
    cps: List[String] = Nil,
    d: AbsolutePath = PathIO.workingDirectory
)

object Settings {
  def parse(args: List[String]): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, args)
        case "-cp" +: cp +: rest if allowOptions =>
          val cps1 = settings.cps :+ cp
          loop(settings.copy(cps = cps1), true, rest)
        case "-d" +: d +: rest if allowOptions =>
          loop(settings.copy(d = AbsolutePath(d)), true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          println(s"unknown flag $flag")
          None
        case cp +: rest =>
          val cps1 = settings.cps :+ cp
          loop(settings.copy(cps = cps1), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args)
  }
}

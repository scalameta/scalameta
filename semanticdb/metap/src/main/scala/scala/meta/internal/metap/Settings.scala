package scala.meta.internal.metap

final case class Settings(
    paths: List[String] = Nil,
    pretty: Boolean = true,
    proto: Boolean = false
)

object Settings {
  def parse(args: List[String]): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, args)
        case "-pretty" +: rest if allowOptions =>
          loop(settings.copy(pretty = true, proto = false), true, rest)
        case "-proto" +: rest if allowOptions =>
          loop(settings.copy(pretty = false, proto = true), true, rest)
        case flag +: rest if allowOptions && flag.startsWith("-") =>
          println(s"unknown flag $flag")
          None
        case path +: rest =>
          val paths1 = settings.paths :+ path
          loop(settings.copy(paths = paths1), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(), true, args)
  }
}

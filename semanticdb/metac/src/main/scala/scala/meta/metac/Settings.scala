package scala.meta.metac

import scala.meta.cli._

final class Settings private (val scalacArgs: List[String]) {
  private def this() = {
    this(scalacArgs = Nil)
  }

  def withScalacArgs(scalacArgs: List[String]): Settings = {
    copy(scalacArgs = scalacArgs)
  }

  private def copy(scalacArgs: List[String] = scalacArgs): Settings = {
    new Settings(scalacArgs = scalacArgs)
  }
}

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    Some(new Settings(args))
  }

  def apply(): Settings = {
    new Settings()
  }
}

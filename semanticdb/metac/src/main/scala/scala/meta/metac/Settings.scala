package scala.meta.metac

import scala.meta.cli._

import scala.annotation.nowarn

final class Settings private (val scalacArgs: List[String]) {
  private def this() = this(scalacArgs = Nil)

  def withScalacArgs(scalacArgs: List[String]): Settings = copy(scalacArgs = scalacArgs)

  private def copy(scalacArgs: List[String]): Settings = new Settings(scalacArgs = scalacArgs)
}

object Settings {
  @nowarn("cat=unused-params") // binary compatibility
  def parse(args: List[String], reporter: Reporter): Option[Settings] = Some(new Settings(args))

  def apply(): Settings = new Settings()
}

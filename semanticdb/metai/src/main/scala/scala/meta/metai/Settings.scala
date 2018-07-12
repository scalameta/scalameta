package scala.meta.metai

import scala.meta.cli.Reporter
import scala.meta.io.Classpath

final class Settings private (val classpath: Classpath) {
  private def this() = {
    this(Classpath(Nil))
  }

  def withClasspath(classpath: Classpath): Settings = {
    copy(classpath = classpath)
  }

  private def copy(classpath: Classpath = this.classpath): Settings = {
    new Settings(classpath)
  }
}

object Settings {
  def parse(args: List[String], reporter: Reporter): Option[Settings] = {
    val classpath = Classpath(args.flatMap(Classpath(_).entries))
    Some(new Settings(classpath))
  }
  def apply(): Settings = {
    new Settings()
  }
}

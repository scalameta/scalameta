package scala.meta.cli

import scala.meta.internal.metacp._

object Metacp {
  def main(args: Array[String]): Unit = {
    Settings.parse(args.toList) match {
      case Some(settings) => sys.exit(process(settings))
      case None => sys.exit(1)
    }
  }

  def process(settings: Settings): Int = {
    Main.process(settings)
  }
}

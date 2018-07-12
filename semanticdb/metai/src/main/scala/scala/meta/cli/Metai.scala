package scala.meta.cli

import scala.meta.internal.metai.Main
import scala.meta.metai.Settings

object Metai {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, Reporter()))
  }

  def process(args: Array[String], reporter: Reporter): Int = {
    Settings.parse(args.toList, reporter) match {
      case Some(settings) =>
        val isSuccess = process(settings, reporter)
        if (isSuccess) 0
        else 1
      case None =>
        1
    }
  }

  def process(settings: Settings, reporter: Reporter): Boolean = {
    val main = new Main(settings, reporter)
    main.process()
  }
}

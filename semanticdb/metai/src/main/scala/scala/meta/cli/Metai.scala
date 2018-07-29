package scala.meta.cli

import scala.meta.internal.metai.Main
import scala.meta.io.Classpath
import scala.meta.metai.Settings

object Metai {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, Reporter()))
  }

  def process(args: Array[String], reporter: Reporter): Int = {
    Settings.parse(args.toList, reporter) match {
      case Some(settings) =>
        val in = settings.classpath
        val out = process(settings, reporter)
        if (in.entries.length == out.entries.length) 0
        else 1
      case None =>
        1
    }
  }

  def process(settings: Settings, reporter: Reporter): Classpath = {
    val main = new Main(settings, reporter)
    main.process()
  }
}

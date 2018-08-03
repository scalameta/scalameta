package scala.meta.cli

import scala.meta.internal.cli.Args
import scala.meta.internal.metai.Main
import scala.meta.io.Classpath
import scala.meta.metai.Result
import scala.meta.metai.Settings

object Metai {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, Reporter()))
  }

  def process(args: Array[String], reporter: Reporter): Int = {
    val expandedArgs = Args.expand(args)
    Settings.parse(expandedArgs, reporter) match {
      case Some(settings) =>
        val result = process(settings, reporter)
        if (result.isSuccess) 0
        else 1
      case None =>
        1
    }
  }

  def process(settings: Settings, reporter: Reporter): Result = {
    val main = new Main(settings, reporter)
    main.process()
  }
}

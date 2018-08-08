package scala.meta.cli

import java.io._
import scala.meta.internal.cli.Args
import scala.meta.internal.metacp._
import scala.meta.io._
import scala.meta.metacp._

object Metacp {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, Reporter()))
  }

  @deprecated("Use `process(Settings.parse(args.toList).get, Reporter())`.", "3.5.0")
  def process(args: Array[String]): Int = {
    process(args, Reporter())
  }

  @deprecated("Use `process(Settings.parse(args.toList).get, Reporter(out, err))`.", "3.5.0")
  def process(args: Array[String], out: PrintStream, err: PrintStream): Int = {
    process(args, Reporter().withOut(out).withErr(err))
  }

  private def process(args: Array[String], reporter: Reporter): Int = {
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

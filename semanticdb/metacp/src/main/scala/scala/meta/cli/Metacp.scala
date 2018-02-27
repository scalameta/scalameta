package scala.meta.cli

import java.io._
import scala.meta.internal.metacp._

object Metacp {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, System.out, System.err))
  }

  @deprecated("Use `process(args, System.out, System.err)` instead.", "3.4.0")
  def process(args: Array[String]): Int = {
    process(args, System.out, System.err)
  }

  def process(args: Array[String], out: PrintStream, err: PrintStream): Int = {
    Settings.parse(args.toList) match {
      case Some(settings) =>
        val main = new Main(settings, out, err)
        main.process()
      case None =>
        1
    }
  }
}

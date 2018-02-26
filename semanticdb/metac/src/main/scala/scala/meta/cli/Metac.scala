package scala.meta.cli

import java.io._
import scala.meta.internal.metac._

object Metac {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, System.out, System.err))
  }

  @deprecated("Use `process(args, System.out)` instead.", "3.4.0")
  def process(args: Array[String]): Int = {
    process(args, System.out, System.err)
  }

  def process(args: Array[String], out: PrintStream, err: PrintStream): Int = {
    val main = new Main(args, out, err)
    main.process()
  }
}

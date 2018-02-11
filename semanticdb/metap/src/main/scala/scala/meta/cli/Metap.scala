package scala.meta.cli

import scala.meta.internal.metap._

object Metap {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args))
  }

  def process(args: Array[String]): Int = {
    Main.process(args)
  }
}
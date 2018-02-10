package scala.meta.cli

import scala.meta.internal.metac._

object Metac {
  def main(args: Array[String]): Unit = {
    sys.exit(Main.process(args))
  }
}

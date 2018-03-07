package scala.meta.metacp

import java.io._

final case class Reporter(out: PrintStream, err: PrintStream)

object Reporter {
  def default: Reporter = Reporter(System.out, System.err)
}

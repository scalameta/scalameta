package scala.meta.cli

import java.io._
import scala.meta.internal.cli._

final class Reporter private (val out: PrintStream, val err: PrintStream) {
  private def this() = {
    this(out = System.out, err = System.err)
  }

  def withOut(out: PrintStream): Reporter = {
    copy(out = out)
  }

  def withSilentOut(): Reporter = {
    withOut(devnull)
  }

  def withErr(err: PrintStream): Reporter = {
    copy(err = err)
  }

  def withSilentErr(): Reporter = {
    withErr(devnull)
  }

  private def copy(out: PrintStream = out, err: PrintStream = err): Reporter = {
    new Reporter(out = out, err = err)
  }
}

object Reporter {
  def apply(): Reporter = {
    new Reporter()
  }
}

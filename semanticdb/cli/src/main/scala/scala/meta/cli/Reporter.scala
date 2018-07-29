package scala.meta.cli

import java.io._

final class Reporter private (val out: PrintStream, val err: PrintStream) {
  private def this() = {
    this(out = System.out, err = System.err)
  }

  def withOut(out: PrintStream): Reporter = {
    copy(out = out)
  }

  def silenceOut(): Reporter = {
    withOut(devnull)
  }

  def withErr(err: PrintStream): Reporter = {
    copy(err = err)
  }

  def silenceErr(): Reporter = {
    withErr(devnull)
  }

  private def copy(out: PrintStream = out, err: PrintStream = err): Reporter = {
    new Reporter(out = out, err = err)
  }

  private def devnull: PrintStream = {
    val os = new OutputStream { override def write(b: Int) = () }
    new PrintStream(os)
  }
}

object Reporter {
  def apply(): Reporter = {
    new Reporter()
  }
}

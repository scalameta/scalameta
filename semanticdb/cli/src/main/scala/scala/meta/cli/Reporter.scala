package scala.meta.cli

import java.io._

final class Reporter private (val out: LengthWriter, val err: LengthWriter) {
  private def this() = {
    this(out = new LengthWriter(new PrintWriter(System.out), 0), err = System.err)
  }

  def withOut(out: LengthWriter): Reporter = {
    copy(out = out)
  }

  def withErr(err: LengthWriter): Reporter = {
    copy(err = err)
  }

  private def copy(out: LengthWriter = out, err: LengthWriter = err): Reporter = {
    new Reporter(out = out, err = err)
  }
}

object Reporter {
  def apply(): Reporter = {
    new Reporter()
  }
}

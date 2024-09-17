package scala.meta.tests.cli

import scala.meta.cli._

import java.io._
import java.nio.charset.StandardCharsets._

object CliTestUtils {

  def withReporter[T](op: Reporter => T): (T, String, String) =
    communicate((out, err) => op(Reporter().withOut(out).withErr(err)))
  def communicate[T](op: (PrintStream, PrintStream) => T): (T, String, String) = {
    val outbaos = new ByteArrayOutputStream
    val outps = new PrintStream(outbaos, true, UTF_8.name)
    val errbaos = new ByteArrayOutputStream
    val errps = new PrintStream(errbaos, true, UTF_8.name)
    val result = op(outps, errps)
    val outs = new String(outbaos.toByteArray, UTF_8)
    val errs = new String(errbaos.toByteArray, UTF_8)
    (result, outs, errs)
  }
}

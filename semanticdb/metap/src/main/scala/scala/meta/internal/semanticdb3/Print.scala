package scala.meta.internal.semanticdb3

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.meta.cli._
import scala.meta.internal.metap.DocumentPrinter
import scala.meta.metap._

object Print {

  def document(format: Format, doc: TextDocument): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val settings = Settings().withFormat(format)
    val reporter = Reporter().withOut(ps).withErr(ps)
    val printer = new DocumentPrinter(settings, reporter, doc)
    printer.print()
    baos.toString()
  }

}

package scala.meta.internal.semanticdb3

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.meta.cli._
import scala.meta.internal.metap.Printer
import scala.meta.metap.Settings

object Print {

  def document(doc: TextDocument): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val printer = new Printer(ps, doc)
    printer.print()
    baos.toString()
  }

}

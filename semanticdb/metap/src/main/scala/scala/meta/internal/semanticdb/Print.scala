package scala.meta.internal.semanticdb

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.meta.cli._
import scala.meta.internal.metap.DocumentPrinter
import scala.meta.internal.metap.PrinterSymtab
import scala.meta.metap._

object Print {

  def document(format: Format, doc: TextDocument): String = {
    val symtab = PrinterSymtab.fromTextDocument(doc)
    withPrinter(format, doc, symtab) { printer =>
      printer.print()
    }
  }

  def tpe(format: Format, tpe: Type, symtab: PrinterSymtab): String = {
    withInfoPrinter(format, TextDocument(), symtab) { printer =>
      printer.pprint(tpe)
    }.trim
  }

  def constant(constant: Constant): String = {
    val symtab = PrinterSymtab.fromTextDocument(TextDocument())
    withInfoPrinter(Format.Detailed, TextDocument(), symtab) { printer =>
      printer.pprint(constant)
    }.trim
  }

  def signature(format: Format, signature: Signature, symtab: PrinterSymtab): String = {
    withInfoPrinter(format, TextDocument(), symtab) { printer =>
      printer.pprint(signature)
    }.trim
  }

  def info(format: Format, info: SymbolInformation, symtab: PrinterSymtab): String = {
    withPrinter(format, TextDocument().addSymbols(info), symtab) { printer =>
      printer.pprint(info)
    }.trim
  }

  def synthetic(
      format: Format,
      doc: TextDocument,
      synthetic: Synthetic,
      symtab: PrinterSymtab): String = {
    withPrinter(format, doc, symtab) { printer =>
      printer.pprint(synthetic)
    }.trim
  }

  def tree(format: Format, doc: TextDocument, tree: Tree, symtab: PrinterSymtab): String = {
    withPrinter(format, doc, symtab) { printer =>
      printer.pprint(tree, None)
    }.trim
  }

  private def withInfoPrinter(format: Format, doc: TextDocument, symtab: PrinterSymtab)(
      fn: DocumentPrinter#InfoPrinter => Unit): String = {
    withPrinter(format, TextDocument(), symtab) { printer =>
      val notes = new printer.InfoNotes()
      val infoPrinter = new printer.InfoPrinter(notes)
      fn(infoPrinter)
    }.trim
  }

  private def withPrinter(format: Format, doc: TextDocument, symtab: PrinterSymtab)(
      fn: DocumentPrinter => Unit): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val settings = Settings().withFormat(format)
    val reporter = Reporter().withOut(ps).withSilentErr()
    val printer = new DocumentPrinter(settings, reporter, doc, symtab)
    fn(printer)
    baos.toString().trim
  }

}

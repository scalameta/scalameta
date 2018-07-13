package scala.meta.internal.metap

import scala.meta.cli._
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Language._
import scala.meta.metap._

class DocumentPrinter(settings: Settings, reporter: Reporter, doc: TextDocument)
    extends BasePrinter(settings, reporter, doc)
    with SymbolInformationPrinter
    with OccurrencePrinter
    with DiagnosticPrinter
    with SyntheticPrinter
    with NewSyntheticPrinter {

  def print(): Unit = {
    out.println(doc.uri)
    out.println(s"-" * doc.uri.length)
    out.println("")

    out.println(s"Summary:")
    out.println(s"Schema => SemanticDB v${doc.schema.value}")
    out.println(s"Uri => ${doc.uri}")
    out.println(s"Text => ${if (doc.text.nonEmpty) "non-empty" else "empty"}")
    doc.language match {
      case SCALA => out.println("Language => Scala")
      case JAVA => out.println("Language => Java")
      case _ => out.println("Language => Unknown")
    }
    if (doc.symbols.nonEmpty) out.println(s"Symbols => ${doc.symbols.length} entries")
    if (doc.occurrences.nonEmpty) out.println(s"Occurrences => ${doc.occurrences.length} entries")
    if (doc.diagnostics.nonEmpty) out.println(s"Diagnostics => ${doc.diagnostics.length} entries")
    if (doc.synthetics.nonEmpty) out.println(s"Synthetics => ${doc.synthetics.length} entries")

    if (doc.symbols.nonEmpty) {
      out.println("")
      out.println("Symbols:")
      doc.symbols.sorted.foreach(pprint)
    }

    if (doc.occurrences.nonEmpty) {
      out.println("")
      out.println("Occurrences:")
      doc.occurrences.sorted.foreach(pprint)
    }

    if (doc.diagnostics.nonEmpty) {
      out.println("")
      out.println("Diagnostics:")
      doc.diagnostics.sorted.foreach(pprint)
    }

    if (doc.synthetics.nonEmpty) {
      out.println("")
      out.println("Synthetics:")
      doc.synthetics.sorted.foreach(pprint)
    }

    if (doc.newSynthetics.nonEmpty) {
      out.println("")
      out.println("New-style Synthetics:")
      doc.newSynthetics.sorted.foreach(pprint)
    }
  }
}

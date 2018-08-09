package scala.meta.internal.metadiff

import java.nio.file.Path
import scala.meta.cli._
import scala.meta.internal.semanticdb.Locator
import scala.meta.metadiff.Settings
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.metadiff.diff._

class Main(settings: Settings, reporter: Reporter) {

  private def collectPayloads(root: Path): Map[String, s.TextDocument] = {
    val builder = Map.newBuilder[String, s.TextDocument]
    Locator(root) { (path, payload) =>
      payload.documents foreach { doc =>
        builder += doc.uri -> doc
      }
    }
    builder.result()
  }

  private def diffDocument(
      docFrom: s.TextDocument,
      docTo: s.TextDocument): Diff = {

    val diffSymbols =
      if (settings.compareSymbols) {

        val symsFrom = docFrom.symbols.map(s => s.symbol -> s).toMap
        val symsTo = docTo.symbols.map(s => s.symbol -> s).toMap

        val orderDiff = diffLines(
          docFrom.symbols.map(_.symbol).toList,
          docTo.symbols.map(_.symbol).toList)

        val fullDiff = diffConcat(
          orderDiff.extractLines.getOrElse(docFrom.symbols.map(_.symbol)).map {
            sym =>
              val symDiff =
                if (symsFrom.contains(sym) && symsTo.contains(sym))
                  diffLines(symsFrom(sym).toProtoString.lines, symsTo(sym).toProtoString.lines)
                else if (symsFrom.contains(sym))
                  diffPatch('-', Some(symsFrom(sym).toProtoString))
                else
                  diffPatch('+', Some(symsTo(sym).toProtoString))
              symDiff.name(s"symbol '$sym'")
          }
        )

        diffConcat(
          if (settings.compareOrder) orderDiff.name("symbol info order")
          else None,
          fullDiff
        )
      } else None

    val diffOccs =
      if (settings.compareOccurrences) {

        val occsFrom = docFrom.occurrences.groupBy(_.symbol)
        val occsTo = docTo.occurrences.groupBy(_.symbol)

        val occsOrderFrom =
          if (settings.compareOrder) docFrom.occurrences
          else docFrom.occurrences.sortBy(_.range)
        val occsOrderTo =
          if (settings.compareOrder) docTo.occurrences
          else docTo.occurrences.sortBy(_.range)

        val orderDiff = diffLines(
          occsOrderFrom.map(occ => occ.symbol + " " + occ.role).toList,
          occsOrderTo.map(occ => occ.symbol + " " + occ.role).toList
        )
        def display(occ: s.SymbolOccurrence): String =
          s"${occ.role} ${occ.range.str}"
        def displayEOL(occ: s.SymbolOccurrence): String =
          s"${occ.role} ${occ.range.str}$EOL"

        val fullDiff = diffConcat(
          orderDiff.extractLines
            .map { lines =>
              lines.map { line =>
                line.substring(0, line.lastIndexOf(' '))
              }.distinct
            }
            .getOrElse(occsOrderFrom.map(_.symbol).distinct)
            .map { occ =>
              val occDiff =
                if (occsFrom.contains(occ) && occsTo.contains(occ))
                  diffLines(
                    occsFrom(occ).map(display),
                    occsTo(occ).map(display))
                else if (occsFrom.contains(occ))
                  diffPatch('-', Some(occsFrom(occ).map(displayEOL).mkString))
                else
                  diffPatch('-', Some(occsTo(occ).map(displayEOL).mkString))
              occDiff.name(s"occurrence symbol '$occ'")
            }
        )

        diffConcat(
          orderDiff.name("occurrence order"),
          fullDiff
        )
      } else None

    val diffSynths =
      if (settings.compareSynthetics) {
        val linesSynthsFrom = s.TextDocument(synthetics = docFrom.synthetics).toProtoString.lines
        val linesSynthsTo = s.TextDocument(synthetics = docTo.synthetics).toProtoString.lines
        diffLines(linesSynthsFrom, linesSynthsTo, contextSize = 5).name("synthetics")
      } else None

    diffConcat(
      diffObj(docFrom.schema, docTo.schema).name("schema"),
      diffObj(docFrom.language, docTo.language).name("language"),
      diffSymbols,
      diffOccs,
      diffSynths
    )
  }

  def process(): Boolean = {
    val List(rootFrom, rootTo) = settings.paths
    val payloadsFrom = collectPayloads(rootFrom)
    val payloadsTo = collectPayloads(rootTo)
    val pathsFrom = payloadsFrom.keySet
    val pathsTo = payloadsTo.keySet
    val pathsShared = pathsFrom & pathsTo
    (pathsFrom | pathsTo).toSeq.sorted foreach { path =>
      if (pathsShared(path)) {
        val payloadFrom = payloadsFrom(path)
        val payloadTo = payloadsTo(path)
        val diff = diffHeader(path, path, diffDocument(payloadFrom, payloadTo))
        diff.foreach(reporter.out.print)
      } else if (pathsFrom(path)) {
        reporter.out.println(s"--- $path")
        reporter.out.println(s"+++")
      } else if (pathsTo(path)) {
        reporter.out.println("---")
        reporter.out.println(s"+++ $path")
      }
    }
    true
  }

  implicit val pathOrdering: Ordering[Path] = Ordering.by(_.toString)
  implicit val rangeOrdering: Ordering[Option[s.Range]] = Ordering.by(
    _.map(r => (r.startLine, r.startCharacter, r.endLine, r.endCharacter)))

  implicit class OptRangeOps(rOpt: Option[s.Range]) {
    def str: String = rOpt.fold("") { r =>
      s"${r.startLine}:${r.startCharacter}-${r.endLine}:${r.endCharacter}"
    }
  }

}

package scala.meta.cli

import java.nio.file._
import java.util._
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.math.Ordering
import scala.util.control.NonFatal
import scala.meta.internal.semanticdb3._
import SymbolInformation._, Kind._, Property._
import SymbolOccurrence._, Role._
import Diagnostic._, Severity._

object Metap {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args))
  }

  def process(args: Array[String]): Int = {
    var failed = false
    args.foreach { arg =>
      try {
        val stream = Files.newInputStream(Paths.get(arg))
        try {
          val documents = TextDocuments.parseFrom(stream)
          documents.documents.foreach(pprint)
        } finally {
          stream.close()
        }
      } catch {
        case NonFatal(ex) =>
          println(s"error: can't decompile $arg")
          ex.printStackTrace()
          failed = true
      }
    }
    if (failed) 1 else 0
  }

  private def pprint(doc: TextDocument): Unit = {
    println(doc.uri)
    println(s"-" * doc.uri.length)
    println("")

    println(s"Summary:")
    println(s"Schema => SemanticDB v${doc.schema.value}")
    println(s"Uri => ${doc.uri}")
    println(s"Text => ${if (doc.text.nonEmpty) "non-empty" else "empty"}")
    println(s"Language => ${doc.language}")
    println(s"Symbols => ${doc.symbols.length} entries")
    println(s"Occurrences => ${doc.occurrences.length} entries")
    println(s"Diagnostics => ${doc.diagnostics.length} entries")
    println(s"Synthetics => ${doc.synthetics.length} entries")

    if (doc.symbols.nonEmpty) {
      println("")
      println("Symbols:")
      doc.symbols.sorted.foreach(pprint(_, doc))
    }

    if (doc.occurrences.nonEmpty) {
      println("")
      println("Occurrences:")
      doc.occurrences.sorted.foreach(pprint(_, doc))
    }

    if (doc.diagnostics.nonEmpty) {
      println("")
      println("Diagnostics:")
      doc.diagnostics.sorted.foreach(pprint(_, doc))
    }

    if (doc.synthetics.nonEmpty) {
      println("")
      println("Synthetics:")
      doc.synthetics.sorted.foreach(pprint(_, doc))
    }
  }

  private val cache = new WeakHashMap[TextDocument, Array[Int]]
  private def offset(doc: TextDocument, line: Int): Int = {
    var lineIndices = cache.get(doc)
    if (lineIndices == null) {
      val chars = doc.text.toArray
      val buf = new mutable.ArrayBuffer[Int]
      buf += 0
      var i = 0
      while (i < chars.length) {
        if (chars(i) == '\n') buf += (i + 1)
        i += 1
      }
      if (buf.last != chars.length) buf += chars.length // sentinel value used for binary search
      lineIndices = buf.toArray
      cache.put(doc, lineIndices)
    }
    lineIndices(line)
  }

  private def pprint(range: Option[Range], doc: Option[TextDocument]): Unit = {
    range.foreach { range =>
      print("[")
      print(range.startLine)
      print(":")
      print(range.startCharacter)
      print("..")
      print(range.endLine)
      print(":")
      print(range.endCharacter)
      print(")")
      doc match {
        case Some(doc) if doc.text.nonEmpty =>
          val startOffset = offset(doc, range.startLine) + range.startCharacter
          val endOffset = offset(doc, range.endLine) + range.endCharacter
          val text = {
            if (startOffset >= endOffset) "ε"
            else doc.text.substring(startOffset, endOffset)
          }
          print(s": $text")
      }
    }
  }

  private def pprint(info: SymbolInformation, doc: TextDocument): Unit = {
    print(s"${info.symbol} => ")
    def has(prop: Property) = (info.properties & prop.value) != 0
    if (has(PRIVATE)) print("private ")
    if (has(PROTECTED)) print("protected ")
    if (has(ABSTRACT)) print("abstract ")
    if (has(FINAL)) print("final ")
    if (has(SEALED)) print("sealed ")
    if (has(IMPLICIT)) print("implicit ")
    if (has(LAZY)) print("lazy ")
    if (has(CASE)) print("case ")
    if (has(COVARIANT)) print("covariant ")
    if (has(CONTRAVARIANT)) print("contravariant ")
    if (info.kind == VAL) print("val ")
    if (info.kind == VAR) print("var ")
    if (info.kind == DEF) print("def ")
    if (info.kind == PRIMARY_CONSTRUCTOR) print("primaryctor ")
    if (info.kind == SECONDARY_CONSTRUCTOR) print("secondaryctor ")
    if (info.kind == MACRO) print("macro ")
    if (info.kind == TYPE) print("type ")
    if (info.kind == PARAMETER) print("param ")
    if (info.kind == TYPE_PARAMETER) print("typeparam ")
    if (info.kind == OBJECT) print("object ")
    if (info.kind == PACKAGE) print("package ")
    if (info.kind == PACKAGE_OBJECT) print("package object ")
    if (info.kind == CLASS) print("class ")
    if (info.kind == TRAIT) print("trait ")
    print(info.name)
    info.kind match {
      case VAL | VAR | DEF | PRIMARY_CONSTRUCTOR |
           SECONDARY_CONSTRUCTOR | MACRO | TYPE | PARAMETER | TYPE_PARAMETER =>
        info.signature match {
          case Some(sig) =>
            print(s": ${sig.text}")
            println("")
            val occs = sig.occurrences.sorted
            occs.foreach { occ => print("  "); pprint(occ, sig) }
          case _ =>
            println("")
        }
        info.overrides.foreach(sym => println(s"  overrides $sym"))
      case OBJECT | PACKAGE | PACKAGE_OBJECT | CLASS | TRAIT =>
        info.members match {
          case _ :: _ => println(s".{+${info.members.length}} members")
          case Nil => println("")
        }
        info.overrides.sorted.foreach(sym => println(s"  extends $sym"))
      case _ =>
        ()
    }
  }

  private def pprint(occ: SymbolOccurrence, doc: TextDocument): Unit = {
    pprint(occ.range, Some(doc))
    occ.role match {
      case REFERENCE => print(" => ")
      case DEFINITION => print(" <= ")
      case _ => print(" ??? ")
    }
    println(occ.symbol)
  }

  private def pprint(diag: Diagnostic, doc: TextDocument): Unit = {
    pprint(diag.range, None)
    diag.severity match {
      case ERROR => print("[error] ")
      case WARNING => print("[warning] ")
      case INFORMATION => print("[info] ")
      case HINT => print("[hint] ")
      case _ => print("[???] ")
    }
    println(diag.message)
  }

  private def pprint(synth: Synthetic, doc: TextDocument): Unit = {
    pprint(synth.range, Some(doc))
    print(" => ")
    synth.text match {
      case Some(text) =>
        println(text.text)
        val occs = text.occurrences.sorted
        occs.foreach { occ => print("  "); pprint(occ, text) }
      case _ =>
        println("???")
    }
  }

  private implicit def rangeOrder: Ordering[Range] =
    Ordering.by(r => (r.startLine, r.startCharacter, r.endLine, r.endCharacter))
  private implicit def infoOrder: Ordering[SymbolInformation] =
    Ordering.by(_.symbol)
  private implicit def occOrder: Ordering[SymbolOccurrence] =
    Ordering.by(_.range)
  private implicit def diagOrder: Ordering[Diagnostic] =
    Ordering.by(_.range)
  private implicit def synthOrder: Ordering[Synthetic] =
    Ordering.by(_.range)
}

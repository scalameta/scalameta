package scala.meta.cli

import java.io._
import scala.util.control.NonFatal
import scala.meta.internal.semanticdb3._

object Metap {
  def process(args: Array[String]): Boolean = {
    var failed = false
    args.foreach { arg =>
      try {
        val stream = new FileInputStream(new File(arg))
        try {
          val documents = TextDocuments.parseFrom(stream)
          documents.documents.foreach { d =>
            val buf = new StringBuilder
            buf.append(s"${d.uri} (")
            buf.append(s"${d.symbols.length} symbols, ")
            buf.append(s"${d.occurrences.length} occurrences, ")
            buf.append(s"${d.diagnostics.length} diagnostics, ")
            buf.append(s"${d.synthetics.length} synthetics)")
            println(buf.toString)
          }
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
    !failed
  }

  def main(args: Array[String]): Unit = {
    sys.exit(if (process(args)) 0 else 1)
  }
}
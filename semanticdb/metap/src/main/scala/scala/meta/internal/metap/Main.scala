package scala.meta.internal.metap

import scala.meta.internal.semanticdb._
import scala.meta.cli._
import scala.meta.metap._

class Main(settings: Settings, reporter: Reporter) {
  def process(): Boolean = {

    var success = true
    var first = true
    Locator(settings.paths) { (path, payload) =>
      if (first) {
        first = false
      } else {
        reporter.out.println("")
      }
      try {
        if (settings.format.isProto) {
          reporter.out.println(payload.toProtoString)
        } else {
          payload.documents.foreach { document =>
            val printer = new DocumentPrinter(settings, reporter, document)
            printer.print()
          }
        }
      } catch {
        case ex: Throwable =>
          reporter.err.println(s"error: can't decompile $path")
          ex.printStackTrace(reporter.err)
          success = false
      }
    }
    success
  }
}

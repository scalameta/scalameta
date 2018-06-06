package scala.meta.internal.metap

import scala.meta.internal.semanticdb3._
import scala.meta.cli._
import scala.meta.metap._
import scala.util.control.NonFatal

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
        case NonFatal(ex) =>
          reporter.out.println(s"error: can't decompile $path")
          ex.printStackTrace(reporter.out)
          success = false
      }
    }
    success
  }
}

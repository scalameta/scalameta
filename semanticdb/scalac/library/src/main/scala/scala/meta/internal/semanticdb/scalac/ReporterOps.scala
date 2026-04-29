package scala.meta.internal.semanticdb.scalac

import scala.collection.mutable
import scala.reflect.internal.util.{Position => gPosition}
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps {
  self: ReflectionToolkit =>

  // Hack, keep track of how many messages we have returns for each path to avoid
  // duplicate messages. The key is System.identityHashCode to keep memory usage low.
  private val returnedMessagesByPath = mutable.Map.empty[g.CompilationUnit, Int]
  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedDiagnostics: List[(gPosition, Int, String)] = g.reporter match {
      case r: StoreReporter =>
        val infos = r.infos
        // drop messages that have been reported before.
        val toDrop = returnedMessagesByPath.put(unit, infos.size).getOrElse(0)
        infos.iterator.drop(toDrop).filter(_.pos.source == unit.source)
          .map(info => (info.pos, info.severity.id, info.msg)).toList
      case _ => Nil
    }
  }
}

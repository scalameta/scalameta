package scala.meta.internal.semanticdb.scalac

import scala.collection.mutable
import scala.tools.nsc.reporters.StoreReporter

trait ReporterOps {
  self: ReflectionToolkit =>

  // Hack, keep track of how many messages we have returns for each path to avoid
  // duplicate messages. The key is System.identityHashCode to keep memory usage low.
  private val returnedMessagesByPath = mutable.Map.empty[g.CompilationUnit, Int]
  implicit class XtensionCompilationUnitReporter(unit: g.CompilationUnit) {
    def hijackedDiagnostics: List[StoreReporter.Info] = g.reporter match {
      case r: StoreReporter =>
        val infos = r.infos
        // drop messages that have been reported before.
        val toDrop = returnedMessagesByPath.put(unit, infos.size).getOrElse(0)
        infos.iterator.drop(toDrop).filter(_.pos.source == unit.source).toList
      case _ => Nil
    }
  }
}

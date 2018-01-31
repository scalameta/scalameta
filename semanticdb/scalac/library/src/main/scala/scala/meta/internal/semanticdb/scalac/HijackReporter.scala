package scala.meta.internal.semanticdb.scalac

trait HijackReporter { self: SemanticdbPlugin =>

  def hijackReporter(): Unit = {
    if (!isSupportedCompiler) return

    g.reporter match {
      case _: SemanticdbReporter => // do nothing, already hijacked
      case underlying =>
        val semanticdbReporter = new SemanticdbReporter(underlying)
        g.reporter = semanticdbReporter
    }
  }
}

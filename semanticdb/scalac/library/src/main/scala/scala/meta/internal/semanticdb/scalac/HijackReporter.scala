package scala.meta.internal.semanticdb.scalac

trait HijackReporter { self: SemanticdbPlugin =>

  def hijackReporter(): Option[SemanticdbReporter] = {
    if (!isSupportedCompiler) return None

    g.reporter match {
      case reporter: SemanticdbReporter => Some(reporter) // do nothing, already hijacked
      case underlying =>
        val semanticdbReporter = new SemanticdbReporter(underlying)
        g.reporter = semanticdbReporter
        Some(semanticdbReporter)
    }
  }
}

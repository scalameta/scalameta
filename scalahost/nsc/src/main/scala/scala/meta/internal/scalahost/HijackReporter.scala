package scala.meta.internal
package scalahost

trait HijackReporter { self: ScalahostPlugin =>

  def hijackReporter(): Unit = {
    g.reporter match {
      case _: ScalahostReporter => // do nothing, already hijacked
      case underlying =>
        val scalahostReporter = new ScalahostReporter(underlying)
        g.reporter = scalahostReporter
    }
  }
}

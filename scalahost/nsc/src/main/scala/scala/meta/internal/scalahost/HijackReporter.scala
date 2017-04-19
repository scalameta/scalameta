package scala.meta.internal
package scalahost

import scala.meta.internal.scalahost.compilers.ScalahostReporter

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

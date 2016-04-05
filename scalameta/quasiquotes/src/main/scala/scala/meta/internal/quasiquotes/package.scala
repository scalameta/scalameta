package scala.meta
package internal

package object quasiquotes {
  implicit class XtensionQuasiquoteDebug(debug: scala.meta.internal.debug.Debug.type) {
    def logQuasiquote(op: => Unit): Unit = {
      if (sys.props("quasiquote.debug") != null) debug.log(op)
    }
  }
}
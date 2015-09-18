package scala.meta
package internal

package object quasiquotes {
  implicit class XtensionQuasiquoteDebug(debug: org.scalameta.debug.Debug.type) {
    def quasiquote = sys.props("quasiquote.debug") != null
  }
}
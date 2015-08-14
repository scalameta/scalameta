package scala.meta
package internal
package quasiquotes

package object ast {
  implicit class XtensionQuasiquoteDebug(debug: org.scalameta.debug.Debug.type) {
    def quasiquote = sys.props("quasiquote.debug") != null
  }
}
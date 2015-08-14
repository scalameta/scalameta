package scala.meta
package internal

package object semantic {
  implicit class XtensionHygieneDebug(debug: org.scalameta.debug.Debug.type) {
    def hygiene = sys.props("hygiene.debug") != null
  }
}
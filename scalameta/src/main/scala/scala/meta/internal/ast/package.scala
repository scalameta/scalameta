package scala.meta
package internal

package object ast {
  implicit class XtensionConvertDebug(debug: org.scalameta.debug.Debug.type) {
    def convert = sys.props("convert.debug") != null
  }
}
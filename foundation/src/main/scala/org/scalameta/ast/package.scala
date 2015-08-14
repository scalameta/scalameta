package org.scalameta

package object ast {
  implicit class XtensionAstDebug(debug: org.scalameta.debug.Debug.type) {
    def ast = sys.props("ast.debug") != null
  }
}
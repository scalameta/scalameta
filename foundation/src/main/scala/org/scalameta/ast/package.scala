package org.scalameta

package object ast {
  implicit class XtensionAstDebug(debug: org.scalameta.debug.Debug.type) {
    def logAst(op: => Unit): Unit = {
      if (sys.props("ast.debug") != null) debug.log(op)
    }
  }
}
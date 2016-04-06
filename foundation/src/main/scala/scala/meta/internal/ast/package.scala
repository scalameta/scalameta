package scala.meta
package internal

package object ast {
  implicit class XtensionAstDebug(debug: scala.meta.internal.debug.Debug.type) {
    def logAst(op: => Unit): Unit = {
      if (sys.props("ast.debug") != null) debug.log(op)
    }
  }
}
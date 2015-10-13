package scala.meta
package internal

package object ast {
  implicit class XtensionConvertDebug(debug: org.scalameta.debug.Debug.type) {
    def logConvert(op: => Unit): Unit = {
      if (sys.props("convert.debug") != null) debug.log(op)
    }
  }

  implicit class XtensionMergeDebug(debug: org.scalameta.debug.Debug.type) {
    def logMerge(op: => Unit): Unit = {
      if (sys.props("merge.debug") != null) debug.log(op)
    }
  }
}
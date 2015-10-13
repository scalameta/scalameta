package scala.meta
package internal.hosts

package object scalac {
  implicit class XtensionScalahostDebug(debug: org.scalameta.debug.Debug.type) {
    def logScalahost(op: => Unit): Unit = {
      if (sys.props("scalahost.debug") != null) debug.log(op)
    }
  }
}
package scala.meta
package internal

package object artifacts {
  implicit class XtensionArtifactDebug(debug: org.scalameta.debug.Debug.type) {
    def logArtifact(op: => Unit): Unit = {
      if (sys.props("artifact.debug") != null) debug.log(op)
    }
  }
}
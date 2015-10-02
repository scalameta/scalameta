package scala.meta
package internal

package object artifacts {
  implicit class XtensionArtifactDebug(debug: org.scalameta.debug.Debug.type) {
    def artifact = sys.props("artifact.debug") != null
  }
}
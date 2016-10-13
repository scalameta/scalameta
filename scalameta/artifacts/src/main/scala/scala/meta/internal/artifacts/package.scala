package scala.meta
package internal

package object artifacts {
  def logArtifact(op: => Unit): Unit = {
    if (sys.props("artifact.debug") != null) op
  }
}
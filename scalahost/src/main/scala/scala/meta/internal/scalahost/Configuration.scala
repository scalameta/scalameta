package scala.meta.internal
package scalahost

object Configuration {
  // TODO: change the default to true, once we're reasonably confident
  def strictMode = sys.props("scalahost.strict") != null
  def dumpDatabase = sys.props("scalahost.dump") != null
}
package scala.meta
package projects

trait Resource {
  def url: String
  def stream: java.io.InputStream
}
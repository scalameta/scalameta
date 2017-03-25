package scala.meta
package semantic
package v1

import java.net.URI
import java.nio.charset._

import org.scalameta.adt._
import scala.io._
import scala.meta.prettyprinters._
import scala.meta.internal.semantic.v1._
import scala.meta.io.AbsolutePath

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the low-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
case class Location(addr: Address, start: Int, end: Int) {
  override def toString = s"""Location(Address("${addr.syntax}"), $start, $end)"""
}

@root trait Address {
  def syntax: String
  def structure: String
  def content: String
}

object Address {


  @leaf class File(path: AbsolutePath) extends Address {
    override def content = path.slurp
    override def syntax = s"file:${path.absolute}"
    override def structure = s"""Address.File("$path")"""
    override def toString = syntax
  }
  object File {
    def apply(path: AbsolutePath): File = new File(path)
    def apply(path: String): File = apply(AbsolutePath.fromAbsoluteOrRelative(path))
    def apply(file: java.io.File): File = apply(AbsolutePath(file))
  }

  @leaf class Snippet(content: String) extends Address {
    override def syntax = s"snippet:$content"
    override def structure = s"""Address.Snippet("$content")"""
    override def toString = syntax
  }

  def apply(s: String): Address = {
    val uri = new URI(s)
    uri.getScheme match {
      case "file" => Address.File(uri.getSchemeSpecificPart)
      case "snippet" => Address.Snippet(uri.getSchemeSpecificPart)
      case _ => sys.error(s"unsupported address: $s")
    }
  }
}

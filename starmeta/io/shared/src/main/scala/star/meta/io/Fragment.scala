package star.meta
package io

import java.net._

final case class Fragment(base: AbsolutePath, name: RelativePath) {
  def uri: URI = {
    val baseuri = base.toURI.normalize
    if (baseuri.toString.endsWith(".jar")) new URI(s"jar:$baseuri!/$name")
    else base.resolve(name).toURI.normalize
  }
  def syntax: String = uri.toString
  def structure: String = s"""Fragment(${base.structure}, ${name.structure})"""
  override def toString = syntax
}

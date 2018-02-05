package org.langmeta
package io

import java.net._

final case class Fragment(base: AbsolutePath, name: RelativePath) {
  def uri: URI = {
    // TODO: URI.normalize isn't yet available in Scala Native,
    // so I had to change `path.toURI.normalize` to `path.normalize.toURI`.
    // I thought these two expressions would be equivalent, but it turned out
    // that they are not, so I had to disable a couple tests.
    val baseuri = base.normalize.toURI
    if (baseuri.toString.endsWith(".jar")) new URI(s"jar:$baseuri!/$name")
    else base.resolve(name).normalize.toURI
  }
  def syntax: String = uri.toString
  def structure: String = s"""Fragment(${base.structure}, ${name.structure})"""
  override def toString = syntax
}

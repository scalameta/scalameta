package scala.meta
package io

import java.net._
import java.io._
import org.scalameta.data._

@data class Fragment(base: AbsolutePath, name: RelativePath) {
  def uri: URI = {
    val baseuri = base.toURI.normalize
    if (baseuri.toString.endsWith(".jar")) new URI(s"jar:$baseuri!/$name")
    else baseuri
  }
  def syntax: String = uri.toString
  def structure: String = s"""Fragment(${base.structure}, ${name.structure})"""
  override def toString = syntax
}

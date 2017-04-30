package scala.meta
package io

import java.net._
import java.io._
import org.scalameta.data._

@data class Fragment(base: AbsolutePath, name: RelativePath) {
  def uri: URI = {
    val suri = if (base.toString.endsWith(".jar")) s"jar:file:$base!/$name" else s"file:$base/$name"
    new URI(suri).normalize
  }
  def syntax: String = uri.toString
  def structure: String = s"""Fragment(${base.structure}, ${name.structure})"""
  override def toString = syntax
}

package org.langmeta
package io

import java.net._
import org.langmeta.internal.io.PathIO

final case class Fragment(base: AbsolutePath, name: RelativePath) {
  def uri: URI = {
    val baseuri = base.toURI.normalize
    if (baseuri.toString.endsWith(".jar")) {
      val reluri = PathIO.toUnix(name.toString)
      new URI(s"jar:$baseuri!/$reluri")
    } else {
      base.resolve(name).toURI.normalize
    }
  }
  def syntax: String = uri.toString
  def structure: String = s"""Fragment(${base.structure}, ${name.structure})"""
  override def toString = syntax
}

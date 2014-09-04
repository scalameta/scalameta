package scala
package org.scalameta.reflection

import scala.tools.nsc.Global

trait Helpers {
  val global: Global
  import global._

  implicit class RichHelperTree[T <: Tree](tree: T) {
    def copyAttrs(other: Tree): T = tree.copyAttrs(other)
  }
}
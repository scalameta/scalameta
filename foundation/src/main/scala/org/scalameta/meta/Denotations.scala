package org.scalameta.meta

import scala.{meta => api}
import scala.meta._

trait Denotations {
  self: Toolkit =>

  trait CanHaveDenot[T <: Tree]
  object CanHaveDenot {
    implicit def Name[T <: api.Name]: CanHaveDenot[T] = null
  }
}

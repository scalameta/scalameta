package org.scalameta.reflection

import scala.{meta => api}
import scala.meta._

trait MetaToolkit {
  trait CanHaveDenot[T <: Tree]
  object CanHaveDenot {
    implicit def Name[T <: api.Name]: CanHaveDenot[T] = null
  }
}

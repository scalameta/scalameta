package scala.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.Global
import scala.meta.internal.scalahost.v1.OnlineMirror

object Mirror {
  def apply(global: Global): Mirror = new OnlineMirror(global)
}
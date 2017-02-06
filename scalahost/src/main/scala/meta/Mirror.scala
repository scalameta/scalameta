package scala.meta

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.Global
import scala.meta.internal.scalahost.v1.online.{Mirror => OnlineMirror}
import scala.meta.internal.scalahost.v1.offline.{Mirror => OfflineMirror}

object Mirror {
  def apply(global: Global): Mirror = new OnlineMirror(global)
  def apply(classpath: String, sourcepath: String): Mirror =
    new OfflineMirror(classpath, sourcepath)
}

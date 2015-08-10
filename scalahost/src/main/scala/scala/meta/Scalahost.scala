package scala.meta

import scala.reflect.ClassTag
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.{meta => m}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.projects.{Context => ScalametaProjectContext}
import scala.meta.internal.hosts.scalac.contexts.{GlobalContext => ScalahostGlobalContextImpl}
import scala.meta.internal.hosts.scalac.contexts.{ProjectContext => ScalahostProjectContextImpl}

trait ScalahostGlobalContext[G <: ScalaGlobal] extends ScalametaSemanticContext { self =>
  val g: G
  protected def toMtree[T <: m.Tree : ClassTag](gtree: g.Tree): T
  object decorators {
    implicit class ScalahostGlobalContextTree(gtree: g.Tree) {
      def toMtree[T <: m.Tree : ClassTag]: T = self.toMtree[T](gtree)
    }
  }
}

trait ScalahostProjectContext extends ScalametaProjectContext with ScalametaSemanticContext {
}

object Scalahost {
  def mkGlobalContext[G <: ScalaGlobal](g: G): ScalahostGlobalContext[G] =
    new ScalahostGlobalContextImpl[G](g)
  def mkProjectContext(sourcepath: String, classpath: String): ScalahostProjectContext =
    new ScalahostProjectContextImpl(sourcepath, classpath)
}

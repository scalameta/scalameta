package scala.meta

import scala.reflect.ClassTag
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.{meta => m}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.projects.{Context => ScalametaProjectContext}
import scala.meta.internal.hosts.scalac.contexts.{GlobalContext => ScalahostGlobalContextImpl}
import scala.meta.internal.hosts.scalac.contexts.{StandaloneContext => ScalahostStandaloneContextImpl}
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

trait ScalahostStandaloneContext extends ScalametaSemanticContext {
  def define(code: String): Source
}

trait ScalahostProjectContext extends ScalametaProjectContext with ScalametaSemanticContext {
}

object Scalahost {
  // TODO: we probably need mkGlobalContext to become private[meta]
  // there should preferably be only one way to do it
  // where "it" means creating a scala.meta-enabled context for/from scalac artifacts
  def mkGlobalContext[G <: ScalaGlobal](g: G): ScalahostGlobalContext[G] =
    new ScalahostGlobalContextImpl[G](g)
  // TODO: replace all usages of mkStandaloneContext with mkProjectContext and then delete it
  def mkStandaloneContext(options: String = ""): ScalahostStandaloneContext =
    new ScalahostStandaloneContextImpl(options)
  def mkProjectContext(sourcepath: String, classpath: String): ScalahostProjectContext =
    new ScalahostProjectContextImpl(sourcepath, classpath)
}

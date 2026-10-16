package scala.meta.internal.semanticdb.scalac

import scala.reflect.macros.Attachments
import scala.tools.nsc.Global
import scala.tools.nsc.doc.ScaladocGlobal
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.interpreter.ReplGlobal

trait ReflectionToolkit {

  val global: Global
  import global._
  lazy val g: global.type = global

  lazy val isDocCompiler = global.isInstanceOf[ScaladocGlobal]
  lazy val isReplCompiler = global.isInstanceOf[ReplGlobal]
  lazy val isInteractiveCompiler = global.isInstanceOf[InteractiveGlobal]
  // NOTE: InteractiveGlobal does not work with semanticdb-scalac, in scalameta/language-server
  // we tried to enable semanticdb-scalac with the presentation compiler and it resulted
  // in cryptic infinite while loops while completing scope members.
  lazy val isSupportedCompiler = !isDocCompiler && !isReplCompiler && !isInteractiveCompiler

  // NOTE: this boilerplate is unfortunately necessary, because we don't expose Attachable in the public API
  trait Attachable[-T] {
    def attachments(carrier: T): Attachments { type Pos = Position }
  }
  object Attachable {
    implicit object TreeAttachable extends Attachable[Tree] {
      def attachments(carrier: Tree): Attachments { type Pos = Position } = carrier.attachments
    }
  }
  implicit class XtensionAttachable[T: Attachable](carrier: T) {
    def attachments: Attachments { type Pos = Position } = implicitly[Attachable[T]]
      .attachments(carrier)
  }

  object CompoundTypeTreeOf {
    def unapply[T: Attachable](carrier: T) = carrier match {
      case CompoundTypeTree(templ) =>
        val att = templ.attachments.get[CompoundTypeTreeOriginalAttachment]
        templ.removeAttachment[CompoundTypeTreeOriginalAttachment]
        att.map(att => CompoundTypeTree(treeCopy.Template(templ, att.parents, templ.self, att.stats)))
      case _ => None
    }
  }

}

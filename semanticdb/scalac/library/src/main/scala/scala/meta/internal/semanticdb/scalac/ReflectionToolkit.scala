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

  def isInteractiveCompiler: Boolean = global.isInstanceOf[InteractiveGlobal]
  // NOTE: InteractiveGlobal does not work with semanticdb-scalac, in scalameta/language-server
  // we tried to enable semanticdb-scalac with the presentation compiler and it resulted
  // in cryptic infinite while loops while completing scope members.
  def isSupportedCompiler: Boolean = isSupportedCompiler(interactiveOK = false)

  def isSupportedCompiler(interactiveOK: Boolean): Boolean = global match {
    case _: ScaladocGlobal | _: ReplGlobal => false
    case _: InteractiveGlobal => interactiveOK
    case _ => true
  }

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
    private val strictTreeCopier = newStrictTreeCopier
    def unapply[T](carrier: T) = carrier match {
      case CompoundTypeTree(templ) => templ.attachments.get[CompoundTypeTreeOriginalAttachment]
          .map { att =>
            // the copy has the attachments of `templ`, and a traversal must not extract it again
            val copy = strictTreeCopier.Template(templ, att.parents, templ.self, att.stats)
            CompoundTypeTree(copy.removeAttachment[CompoundTypeTreeOriginalAttachment])
          }
      case _ => None
    }
  }

}

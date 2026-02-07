package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb => s}

import scala.tools.scalap.scalax.rules.scalasig._

trait AnnotationOps {
  self: Scalacp =>

  import AnnotationOps._

  private def parseEntry(ref: Int): Any = node.scalaSig.parseEntry(ref)

  implicit class XtensionAnnotation(ann: Int) {
    def toSemanticOpt: Option[s.AnnotationTree] = parseEntry(ann) match {
      case ann: AnnotInfo => ann.refs match {
          case head +: tail => parseEntry(head) match {
              case tpe: Type =>
                Some(s.AnnotationTree(tpe.toSemanticTpe, tail.map(x => toTree(parseEntry(x)))))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

    def toSemantic: s.AnnotationTree = toSemanticOpt.getOrElse(s.AnnotationTree())
  }

  implicit class XtensionAttribute(obj: AttributeInfo) {
    def toSemanticOpt: Option[s.AnnotationTree] = obj.typeRef.toSemanticTpe match {
      case tpe: s.TypeRef if syntheticAnnotationsSymbols.contains(tpe.symbol) => None
      case tpe => Some(s.AnnotationTree(tpe, obj.values.map(pairToTree)))
    }

  }

}

object AnnotationOps {

  import scala.tools.scalap.scalax.rules.~

  private val syntheticAnnotationsSymbols = Set("scala/reflect/macros/internal/macroImpl#")

  private def toTree(obj: Any): s.Tree = s.Constant.opt(obj)
    .fold[s.Tree](s.NoTree)(s.LiteralTree.apply)

  private def pairToTree(obj: String ~ Any): s.AssignTree = s
    .AssignTree(s.IdTree(obj._1), toTree(obj._2))

}

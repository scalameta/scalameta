package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb => s}

import scala.tools.scalap.scalax.rules.scalasig._
import scala.tools.scalap.scalax.rules.~

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

  private def toTree(obj: Any): s.Tree = obj match {
    case x: ExternalSymbol => toTree(ConstantType(x).toSemanticTpe)
    case x: Symbol => s.IdTree(x.toSemantic)
    case x: Type => toTree(x.toSemanticTpe)
    case _ => s.Constant.opt(obj).fold[s.Tree](s.NoTree)(s.LiteralTree.apply)
  }

  private def toTree(obj: s.Type): s.Tree = obj match {
    case x: s.ConstantType => s.LiteralTree(x.constant)
    case x: s.SingleType => typeToTree(x.prefix, x.symbol)
    case x: s.TypeRef =>
      val func = typeToTree(x.prefix, x.symbol)
      if (x.typeArguments.isEmpty) func else s.TypeApplyTree(func, x.typeArguments)
    case _ => s.NoTree
  }

  private def typeToTree(prefix: s.Type, symbol: String): s.Tree = {
    val pre = toTree(prefix)
    val sym = s.IdTree(symbol)
    if (pre eq s.NoTree) sym else s.SelectTree(pre, Some(sym))
  }

  private def pairToTree(obj: String ~ Any): s.AssignTree = s
    .AssignTree(s.IdTree(obj._1), toTree(obj._2))

}

object AnnotationOps {

  private val syntheticAnnotationsSymbols = Set("scala/reflect/macros/internal/macroImpl#")

}

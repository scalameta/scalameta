package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.inputs._
import scala.meta.internal.semanticdb.Implicits._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

trait SyntheticOps {
  self: SemanticdbOps =>
  import g._

  implicit class XtensionGTreeSTree(gTree: g.Tree) {

    def toSemanticTree: s.Tree = gTree match {
      case gTree: g.Apply => s.ApplyTree(
          function = gTree.fun.toSemanticQualifierTree,
          arguments = gTree.args.map(_.toSemanticTree)
        )
      case gTree: g.TypeApply => s.TypeApplyTree(
          function = gTree.fun.toSemanticQualifierTree,
          typeArguments = gTree.args.map(_.tpe.toSemanticTpe)
        )
      case gTree: g.Select => gTree.toSemanticId
      case gTree: g.Ident => gTree.toSemanticId
      case gTree: g.This => gTree.toSemanticId
      case gTree: g.Typed if gTree.hasAttachment[g.analyzer.MacroExpansionAttachment] =>
        val expandeeTree = gTree.attachments.get[g.analyzer.MacroExpansionAttachment].get.expandee
        val beforeExpansion =
          if (expandeeTree.pos.isRange) expandeeTree.toSemanticOriginal
          else expandeeTree.toSemanticTree
        s.MacroExpansionTree(beforeExpansion = beforeExpansion, tpe = gTree.tpt.tpe.toSemanticTpe)
      case _ => s.NoTree
    }

    def toSemanticQualifierTree: s.Tree = gTree match {
      case sel @ Select(qual, _) if sel.symbol.owner != qual.symbol =>
        s.SelectTree(qualifier = qual.toSemanticId, id = Some(sel.toSemanticId))
      case fun => fun.toSemanticId
    }

    def toSemanticId: s.IdTree = s.IdTree(symbol = gTree.symbol.toSemantic)

    def toSemanticOriginal: s.Tree = s.OriginalTree(range = Some(gTree.pos.toMeta.toRange))

  }

}

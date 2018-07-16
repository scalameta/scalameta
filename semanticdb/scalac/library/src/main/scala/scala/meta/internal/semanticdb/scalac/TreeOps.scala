package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._

trait TreeOps { self: SemanticdbOps =>
  import g._

  implicit class XtensionGTreeSyntheticTerm(gTree: g.Tree) {
    def toSemanticTree: s.Tree = gTree match {
      case gTree: g.Apply => s.ApplyTree(
        fn = gTree.fun.toSemanticTree,
        args = gTree.args.map(_.toSemanticTree)
      )
      case gTree: g.TypeApply => s.TypeApplyTree(
        fn = gTree.fun.toSemanticTree,
        targs = gTree.args.map(_.tpe.toSemanticTpe)
      )
      case gTree: g.Select => s.SelectTree(
        qual = gTree.qualifier.toSemanticTree,
        id = Some(s.IdTree(gTree.symbol.toSemantic))
      )
      case gTree: g.Ident => s.IdTree(
        sym = gTree.symbol.toSemantic
      )
      case gTree: g.This => s.IdTree(
        sym = gTree.symbol.toSemantic
      )
      case _ =>
        println(s"No match on: $gTree ${gTree.getClass}")
        s.Tree.Empty
    }
  }

}

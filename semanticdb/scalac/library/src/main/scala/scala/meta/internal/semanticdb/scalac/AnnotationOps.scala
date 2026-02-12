package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}

trait AnnotationOps {
  self: SemanticdbOps =>

  implicit class XtensionAnnotationInfo(gann: g.AnnotationInfo) {
    def toSemantic: s.AnnotationTree = s.AnnotationTree(
      gann.atp.toSemanticTpe,
      if (gann.args.nonEmpty) gann.args.map(_.toSemanticTree)
      else gann.assocs.map { case (gname, garg) =>
        s.AssignTree(s.IdTree(gname.toString()), toTree(garg))
      }
    )
  }

  private def toTree(garg: g.ClassfileAnnotArg): s.Tree = garg match {
    case garg: g.LiteralAnnotArg => toTree(garg.const)
    case garg: g.ArrayAnnotArg => s.ApplyTree(s.NoTree, garg.args.map(toTree))
    case garg: g.NestedAnnotArg => garg.annInfo.toSemantic
    case _ => s.NoTree
  }

  private def toTree(garg: g.Constant): s.Tree = garg.value match {
    case x: g.Symbol => s.IdTree(x.toSemantic)
    case x: g.Type => toTree(x.toSemanticTpe)
    case x => s.Constant.opt(x).fold[s.Tree](s.NoTree)(s.LiteralTree.apply)
  }

  private def toTree(sarg: s.Type): s.Tree = sarg match {
    case sarg: s.ConstantType => s.LiteralTree(sarg.constant)
    case sarg: s.SingleType => typeToTree(sarg.prefix, sarg.symbol)
    case sarg: s.TypeRef =>
      val func = typeToTree(sarg.prefix, sarg.symbol)
      if (sarg.typeArguments.isEmpty) func else s.TypeApplyTree(func, sarg.typeArguments)
    case _ => s.NoTree
  }

  private def typeToTree(sarg: s.Type, ssym: String): s.Tree = {
    val pre = toTree(sarg)
    val sym = s.IdTree(ssym)
    if (pre eq s.NoTree) sym else s.SelectTree(pre, Some(sym))
  }

}

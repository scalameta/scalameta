package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}

trait AnnotationOps {
  self: SemanticdbOps =>

  implicit class XtensionAnnotationInfo(gann: g.AnnotationInfo) {
    def toSemantic: s.AnnotationTree = s.AnnotationTree(
      gann.atp.toSemanticTpe,
      if (gann.args.nonEmpty) gann.args.map(_.toSemanticTree)
      else gann.assocs.map { case (gname, garg) =>
        s.AssignTree(s.IdTree(gname.toString()), getClassFileAnnotArg(garg))
      }
    )
  }

  private def getClassFileAnnotArg(garg: g.ClassfileAnnotArg): s.Tree = garg match {
    case garg: g.LiteralAnnotArg => s.LiteralTree(s.Constant(garg.const.value))
    case garg: g.ArrayAnnotArg => s.ApplyTree(s.NoTree, garg.args.map(getClassFileAnnotArg))
    case garg: g.NestedAnnotArg => garg.annInfo.toSemantic
    case _ => s.NoTree
  }

}

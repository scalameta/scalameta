package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}

trait AnnotationOps {
  self: SemanticdbOps =>

  implicit class XtensionAnnotationInfo(gann: g.AnnotationInfo) {
    def toSemantic: s.AnnotationTree = {
      val args =
        if (gann.args.nonEmpty) {
          val gtemps = gann.args.iterator.map(_.symbol).filter(isArtifact)
          val original = originalFromArgs(gtemps)
          gann.args.map(valueOf(_, original).toSemanticTree)
        } else gann.assocs.map { case (gname, garg) =>
          s.AssignTree(s.IdTree(gname.toString()), toTree(garg))
        }
      s.AnnotationTree(gann.atp.toSemanticTpe, args)
    }

    private def isArtifact(gsym: g.Symbol): Boolean = (gsym ne null) && gsym.isArtifact

    // 2.12 binds a named argument to a temporary, in a block that the annotation keeps as original
    private def valueOf(garg: g.Tree, original: g.Tree): g.Tree = original match {
      case x: g.Block =>
        val gsym = garg.symbol
        if (!isArtifact(gsym)) garg
        else x.stats.collectFirst { case v: g.ValDef if v.symbol eq gsym => v.rhs }.getOrElse(garg)
      case _ => garg
    }

    /* return the tree that scalac typed `gann` from. When scalac abstracts an existential in an
     * inferred type, it copies `gann` without that tree, so we take the tree from the annotation
     * at the same position: in the info of the annotated symbol, whose local dummy owns `args`,
     * or in the type of a tree of the document */
    private def originalFromArgs(args: Iterator[g.Symbol]): g.Tree = {
      val gpos = gann.pos
      var gorig = gann.original
      // we use .exists in several places to stop traversal early
      if (gorig.isEmpty && args.hasNext && !args.exists(_.owner.safeOwner.info.exists {
          case t: g.AnnotatedType => t.annotations.exists(a =>
              a.pos == gpos && {
                gorig = a.original
                gorig.nonEmpty
              },
            )
          case _ => false
        })) documentAnnotationOriginal(gpos)
      else gorig
    }
  }

  private def toTree(garg: g.ClassfileAnnotArg): s.Tree = garg match {
    case garg: g.LiteralAnnotArg => toTree(garg.const)
    case garg: g.ArrayAnnotArg => s.ApplyTree(s.NoTree, garg.args.map(toTree))
    case garg: g.NestedAnnotArg => garg.annInfo.toSemantic
    case _ => s.NoTree
  }

  private[scalac] def toTree(garg: g.Constant): s.Tree = garg.value match {
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

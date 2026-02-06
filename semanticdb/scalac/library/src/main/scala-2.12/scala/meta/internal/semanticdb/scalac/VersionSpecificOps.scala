package scala.meta.internal.semanticdb.scalac

trait VersionSpecificOps {
  self: SemanticdbOps =>

  object OriginalTreeOf {
    def unapply[T: Attachable](carrier: T): Option[g.Tree] = carrier.attachments
      .get[g.analyzer.OriginalTreeAttachment].map(_.original)
  }

  /**
   * NamedApplyInfo is only returned in Scala 2.12 or newer.
   */
  object NamedApplyBlock {
    def unapply(block: g.Block): Option[Option[g.analyzer.NamedApplyInfo]] = block.attachments
      .get[g.analyzer.NamedApplyInfo].map(Some(_))
  }

  def getSyntheticSAMClass(gt: g.Function): Option[g.Symbol] = gt.attachments.get[global.SAMFunction]
    .map(_.synthCls)

  object AssignOrNamedArg {
    def unapply(gtree: g.Tree): Option[(g.Tree, g.Tree)] = gtree match {
      case gtree: g.AssignOrNamedArg => Some((gtree.lhs, gtree.rhs))
      case _ => None
    }
  }

}

package scala.meta.internal.semanticdb.scalac

trait VersionSpecificOps {
  self: SemanticdbOps =>

  object OriginalTreeOf {
    def unapply[T: Attachable](carrier: T): Option[g.Tree] = carrier.attachments
      .get[g.analyzer.OriginalTreeAttachment].map(_.original)
  }

  object AssignOrNamedArg {
    def unapply(gtree: g.Tree): Option[(g.Tree, g.Tree)] = gtree match {
      case gtree: g.AssignOrNamedArg => Some((gtree.lhs, gtree.rhs))
      case _ => None
    }
  }

}

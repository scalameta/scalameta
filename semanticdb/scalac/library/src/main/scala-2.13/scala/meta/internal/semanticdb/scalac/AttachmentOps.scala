package scala.meta.internal.semanticdb.scalac

trait AttachmentOps { self: SemanticdbOps =>

  object OriginalTreeOf {
    def unapply[T: Attachable](carrier: T): Option[g.Tree] =
      carrier.attachments.get[g.analyzer.OriginalTreeAttachment].map(_.original)
  }

}

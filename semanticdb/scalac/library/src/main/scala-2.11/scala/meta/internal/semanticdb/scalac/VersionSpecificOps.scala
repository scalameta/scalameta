package scala.meta.internal.semanticdb.scalac

trait VersionSpecificOps {
  self: SemanticdbOps =>

  object OriginalTreeOf {
    def unapply[T: Attachable](carrier: T): Option[g.Tree] = None
  }

  /**
   * NamedApplyInfo is only returned in Scala 2.12 or newer.
   */
  object NamedApplyBlock {
    def unapply(block: g.Block): Option[Option[g.analyzer.NamedApplyInfo]] =
      if (block.stats.forall(stat =>
          stat.symbol != null && stat.symbol.isArtifact &&
            (stat match {
              case g.ValDef(_, name, _, _)
                  if name.startsWith(g.termNames.NAMEDARG_PREFIX) ||
                    name.startsWith(g.termNames.QUAL_PREFIX) => true
              case _ => false
            })
        )) Some(None)
      else None
  }
}

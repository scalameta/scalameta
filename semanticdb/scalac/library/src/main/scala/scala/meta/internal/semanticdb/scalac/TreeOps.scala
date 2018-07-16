package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._

trait TreeOps { self: SemanticdbOps =>
  import g._

  implicit class XtensionGTreeSyntheticTerm(gTree: g.Tree) {
    def asSyntheticTerm: s.SyntheticTerm = gTree match {
      case gTree: g.Apply => s.SyntheticApply(
        sym = gTree.symbol.toSemantic
      )
      case gTree: g.TypeApply =>
        gTree.fun.asSyntheticApply.copy(
          typeArgs = gTree.args.map(_.tpe.toSemanticTpe)
        )
      case gTree: g.Select =>
        val sym = gTree.symbol.toSemantic
        if (sym.desc.isMethod) s.SyntheticApply(
          sym = sym
        )
        else s.SyntheticValue(
          sym = sym
        )
      case _ =>
        println(s"No match on: $gTree ${gTree.getClass}")
        s.SyntheticTerm.Empty
    }
    def asSyntheticApply = {
      println(s"attempting $gTree as SyntheticApply")
      asSyntheticTerm.asInstanceOf[s.SyntheticApply]
    }
  }

}

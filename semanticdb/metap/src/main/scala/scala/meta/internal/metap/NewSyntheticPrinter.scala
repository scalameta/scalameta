package scala.meta.internal.metap

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._

trait NewSyntheticPrinter extends BasePrinter with RangePrinter with SymbolInformationPrinter with ConstantPrinter {

  // YIKES!
  val syntheticInfoPrinter = new InfoPrinter(new InfoNotes)

  def pprint(synth: s.NewSynthetic): Unit = {
    opt(synth.range)(pprint)
    opt(": ", doc.substring(synth.range))(out.print)
    out.print(" => ")
    pprint(synth.tree)
    out.println()
  }

  def pprint(tree: s.Tree): Unit = {
    tree match {
      case tree: s.ApplyTree =>
        pprint(tree.fn)
        out.print("(")
        rep(tree.args, ",")(pprint)
        out.print(")")
      case tree: s.FunctionTree =>
      case tree: s.IdTree =>
        out.print(tree.sym.desc.name)
      case tree: s.LiteralTree =>
        pprint(tree.const)
      case tree: s.MacroExpansionTree =>
      case tree: s.OriginalTree =>
        out.print("<")
        opt(doc.substring(tree.range))(out.print)
        out.print(">")
      case tree: s.SelectTree =>
        pprint(tree.qual)
        out.print(".")
        opt(tree.id)(pprint)
      case tree: s.TypeApplyTree =>
        pprint(tree.fn)
        out.print("[")
        rep(tree.targs, ",")(syntheticInfoPrinter.pprint)
        out.print("]")
      case _ =>
    }
  }

  implicit def newSynthOrder: Ordering[s.NewSynthetic] = Ordering.by(_.range)

}

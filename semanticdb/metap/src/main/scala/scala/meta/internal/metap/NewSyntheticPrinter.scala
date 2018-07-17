package scala.meta.internal.metap

import scala.meta.internal.{semanticdb => s}

trait NewSyntheticPrinter extends BasePrinter with RangePrinter with SymbolInformationPrinter with ConstantPrinter {
  self =>

  def pprint(synth: s.NewSynthetic): Unit = {
    opt(synth.range)(pprint)
    opt(": <", doc.substring(synth.range), ">")(out.print)
    out.print(" => ")
    pprint(synth.tree)
  }

  def pprint(tree: s.Tree): Unit = {
    val infoNotes = new InfoNotes
    val treePrinter = new TreePrinter(infoNotes)
    treePrinter.pprint(tree)
    out.println()

    if (settings.format.isDetailed) {
      printInfosVisited(infoNotes.visited)
    }
  }

  implicit def newSynthOrder: Ordering[s.NewSynthetic] = Ordering.by(_.range)

  private class TreePrinter(notes: InfoNotes) extends InfoPrinter(notes) {

    def pprint(tree: s.Tree): Unit = {
      tree match {
        case tree: s.ApplyTree =>
          pprint(tree.fn)
          out.print("(")
          rep(tree.args, ",")(pprint)
          out.print(")")
        case tree: s.FunctionTree =>
        case tree: s.IdTree =>
          pprint(tree.sym, Reference)
        case tree: s.LiteralTree =>
          self.pprint(tree.const)
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
          rep(tree.targs, ",")(pprint)
          out.print("]")
        case s.Tree.Empty =>
          out.print("<?>")
      }
    }

  }

}

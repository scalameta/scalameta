package scala.meta.internal.metap

import scala.meta.internal.{semanticdb => s}

trait SyntheticPrinter extends BasePrinter with RangePrinter with SymbolInformationPrinter with ConstantPrinter {
  self =>

  private def shorten(s: String): String = {
    val newline = s.indexOf('\n')
    if (newline == -1) {
      if (s.length > 100) s.takeRight(100) + "..."
      else s
    } else s.substring(0, newline) + "..."
  }

  def pprint(synth: s.Synthetic): Unit = {
    opt(synth.range)(pprint)
    opt(": <", doc.substring(synth.range).map(shorten), ">")(out.print)
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

  implicit def newSynthOrder: Ordering[s.Synthetic] = Ordering.by(_.range)

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
          opt(doc.substring(tree.range).map(shorten))(out.print)
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

package scala.meta.internal.metap

import scala.collection.mutable
import scala.meta.internal.semanticdb._

trait SyntheticPrinter extends BasePrinter with RangePrinter with SymbolInformationPrinter {

  def pprint(synth: Synthetic): Unit = {
    opt(synth.range)(pprint)
    opt(": ", doc.substring(synth.range), "")(out.print)
    out.print(" => ")
    pprint(synth.tree, synth.range)
  }

  def pprint(tree: Tree, originalRange: Option[Range]): Unit = {
    val infoNotes = new InfoNotes
    val treePrinter = new TreePrinter(infoNotes, originalRange)
    treePrinter.pprint(tree)
    out.println()

    if (settings.format.isDetailed) {
      val printed = mutable.Set[String]()
      infoNotes.visited.foreach { info =>
        if (!printed(info.symbol)) {
          printed += info.symbol
          out.print("  ")
          out.print(info.name)
          out.print(" => ")
          out.println(info.symbol)
        }
      }
    }
  }

  implicit def syntheticOrder: Ordering[Synthetic] = Ordering.by(_.range)

  private class TreePrinter(notes: InfoNotes, originalRange: Option[Range])
      extends InfoPrinter(notes) {

    def pprint(tree: Tree): Unit = {
      tree match {
        case tree: ApplyTree =>
          pprint(tree.fn)
          out.print("(")
          rep(tree.args, ", ")(pprint)
          out.print(")")
        case tree: FunctionTree =>
          out.print("{")
          rep("(", tree.params, ", ", ") => ")(pprint)
          pprint(tree.term)
          out.print("}")
        case tree: IdTree =>
          pprint(tree.sym, Reference)
        case tree: LiteralTree =>
          pprint(tree.const)
        case tree: MacroExpansionTree =>
          out.print("(??? : ")
          pprint(tree.tpe)
          out.print(")")
        case tree: OriginalTree =>
          if (tree.range == originalRange && originalRange.nonEmpty) {
            out.print("*")
          } else {
            out.print("orig(")
            opt(doc.substring(tree.range))(out.print)
            out.print(")")
          }
        case tree: SelectTree =>
          pprint(tree.qual)
          out.print(".")
          opt(tree.id)(pprint)
        case tree: TypeApplyTree =>
          pprint(tree.fn)
          out.print("[")
          rep(tree.targs, ", ")(pprint)
          out.print("]")
        case NoTree =>
          out.print("<?>")
      }
    }

  }

}

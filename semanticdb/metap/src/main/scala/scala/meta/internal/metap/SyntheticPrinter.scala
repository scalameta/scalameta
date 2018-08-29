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
          out.print(info.displayName)
          out.print(" => ")
          out.println(info.symbol)
        }
      }
    }
  }

  implicit def syntheticOrder: Ordering[Synthetic] = Ordering.by(_.range)

  class TreePrinter(notes: InfoNotes, originalRange: Option[Range]) extends InfoPrinter(notes) {

    def pprint(tree: Tree): Unit = {
      tree match {
        case tree: ApplyTree =>
          pprint(tree.function)
          out.print("(")
          rep(tree.arguments, ", ")(pprint)
          out.print(")")
        case tree: FunctionTree =>
          out.print("{")
          rep("(", tree.parameters, ", ", ") => ")(pprint)
          pprint(tree.body)
          out.print("}")
        case tree: IdTree =>
          pprint(tree.symbol, Reference)
        case tree: LiteralTree =>
          pprint(tree.constant)
        case tree: MacroExpansionTree =>
          out.print("(`macro-expandee` : ")
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
          pprint(tree.qualifier)
          out.print(".")
          opt(tree.id)(pprint)
        case tree: TypeApplyTree =>
          pprint(tree.function)
          out.print("[")
          rep(tree.typeArguments, ", ")(pprint)
          out.print("]")
        case NoTree =>
          out.print("<?>")
      }
    }

  }

}

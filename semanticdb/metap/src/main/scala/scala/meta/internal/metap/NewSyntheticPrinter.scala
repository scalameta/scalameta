package scala.meta.internal.metap

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._

trait NewSyntheticPrinter extends BasePrinter with RangePrinter with SymbolInformationPrinter with ConstantPrinter {
  self =>

  def pprint(synth: s.NewSynthetic): Unit = {
    opt(synth.range)(pprint)
    opt(": ", doc.substring(synth.range))(out.print)
    out.print(" => ")
    pprint(synth.tree)
    out.println()
  }

  def pprint(tree: s.Tree): Unit = {
    val treePrinter = new TreePrinter()
    treePrinter.pprint(tree)
    val result = treePrinter.sb.toString()
    val resultDoc = s.TextDocument(text = result)
    out.println(result)
    rep("  ", treePrinter.occs, "  ") {
      case (range, sym) =>
        pprint(range)
        opt(": ", resultDoc.substring(Option(range)))(out.print)
        out.println(sym)
    }
  }

  implicit def newSynthOrder: Ordering[s.NewSynthetic] = Ordering.by(_.range)

  private class TreePrinter(val sb: StringBuilder = new StringBuilder, var occs: List[(s.Range, String)] = Nil) extends InfoPrinter(new InfoNotes) {

    def printOcc(str: String, sym: String): Unit = {
      val start = sb.length
      sb ++= str
      val end = sb.length
      occs = occs :+ (s.Range(
        startCharacter = start, endCharacter = end
      ), sym)
    }

    def print(str: String): Unit =
      sb ++= str

    def pprint(tree: s.Tree): Unit = {
      tree match {
        case tree: s.ApplyTree =>
          pprint(tree.fn)
          print("(")
          rep(tree.args, ",")(pprint)
          print(")")
        case tree: s.FunctionTree =>
        case tree: s.IdTree =>
          out.print(tree.sym.desc.name)
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
          print("[")
          rep(tree.targs, ",")(pprint)
          print("]")
        case _ =>
      }
    }

  }

}

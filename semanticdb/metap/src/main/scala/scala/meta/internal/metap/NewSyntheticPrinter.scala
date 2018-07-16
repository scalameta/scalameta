package scala.meta.internal.metap

import scala.meta.internal.{semanticdb => s}

trait NewSyntheticPrinter extends BasePrinter with RangePrinter with SymbolInformationPrinter {

  // YIKES!
  val syntheticInfoPrinter = new InfoPrinter(null)

  def pprint(synth: s.NewSynthetic): Unit = {
    opt(range(synth))(pprint)
    opt(": ", doc.substring(range(synth)))(out.print)
    out.print(" => ")

    synth match {
      case synth: s.ImplicitConversionSynthetic =>
        opt(synth.call)(pprint)
      case synth: s.InferredTypeSynthetic =>
        rep("InferredType[", synth.types, ", ", "]")(syntheticInfoPrinter.pprint)
      case s.NewSynthetic.Empty => sys.error("empty new synthetic")
    }
    out.println()
  }

  def pprint(synthTerm: s.SyntheticTerm): Unit = {
    synthTerm match {
      case synthTerm: s.SyntheticApply =>
        out.print(synthTerm.sym)
        rep("[", synthTerm.typeArgs, ",", "]")(syntheticInfoPrinter.pprint)
        rep("(", synthTerm.args, ",", ")")(pprint)
      case synthTerm: s.SyntheticValue =>
        out.print(synthTerm.sym)
      case _ =>
    }
  }

  implicit def newSynthOrder: Ordering[s.NewSynthetic] = {
    Ordering.by(s => (range(s), s.asMessage.sealedValue.number))
  }

  private def range(synth: s.NewSynthetic): Option[s.Range] = synth match {
    case synth: s.ImplicitConversionSynthetic => synth.range
    case synth: s.InferredTypeSynthetic => synth.range
    case s.NewSynthetic.Empty => sys.error("empty new synthetic")
  }
}

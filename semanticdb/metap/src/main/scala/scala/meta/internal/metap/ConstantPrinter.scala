package scala.meta.internal.metap

import scala.meta.internal.semanticdb._

trait ConstantPrinter extends BasePrinter {

  def pprint(const: Constant): Unit = {
    const match {
      case NoConstant =>
        out.print("<?>")
      case UnitConstant() =>
        out.print("()")
      case BooleanConstant(true) =>
        out.print(true)
      case BooleanConstant(false) =>
        out.print(false)
      case ByteConstant(value) =>
        out.print(value.toByte)
      case ShortConstant(value) =>
        out.print(value.toShort)
      case CharConstant(value) =>
        out.print("'" + value.toChar + "'")
      case IntConstant(value) =>
        out.print(value)
      case LongConstant(value) =>
        out.print(value + "L")
      case FloatConstant(value) =>
        out.print(value + "f")
      case DoubleConstant(value) =>
        out.print(value)
      case StringConstant(value) =>
        out.print("\"" + value + "\"")
      case NullConstant() =>
        out.print("null")
    }
  }

}

package org.scalameta.reflection

import org.scalameta.invariants._

trait Platform {
  self: GlobalToolkit =>

  import global._
  import definitions._

  implicit class RichPlatformType(tpe: Type) {
    def jvmsig: String = {
      transformedType(tpe) match {
        case TypeRef(_, sym, args) =>
          require(args.nonEmpty ==> (sym == ArrayClass && args.length == 1))
          if (sym == UnitClass) "V"
          else if (sym == BooleanClass) "Z"
          else if (sym == CharClass) "C"
          else if (sym == ByteClass) "B"
          else if (sym == ShortClass) "S"
          else if (sym == IntClass) "I"
          else if (sym == FloatClass) "F"
          else if (sym == LongClass) "J"
          else if (sym == DoubleClass) "D"
          else if (sym == ArrayClass) "[" + args.head.jvmsig
          else "L" + sym.fullName.replace(".", "/") + ";"
        case MethodType(params, ret) =>
          s"(" + params.map(_.jvmsig).mkString("") + ")" + ret.jvmsig
      }
    }
  }

  implicit class RichPlatformSymbol(sym: Symbol) {
    def jvmsig: String = {
      // NOTE: unfortunately, this simple-looking facility generates side effects that corrupt the state of the compiler
      // in particular, mixin composition stops working correctly, at least for `object Main extends App`
      // exitingDelambdafy(new genASM.JPlainBuilder(null, false).descriptor(sym))
      if (sym.isConstructor) sym.info.erasure.asInstanceOf[MethodType].copy(resultType = UnitClass.tpe).jvmsig
      else sym.info.jvmsig
    }
  }
}
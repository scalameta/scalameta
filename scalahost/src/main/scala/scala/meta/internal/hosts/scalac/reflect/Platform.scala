package scala.meta.internal.hosts.scalac
package reflect

import org.scalameta.invariants._

trait Platform {
  self: ReflectToolkit =>

  import global._
  import definitions._

  implicit class RichPlatformType(tpe: Type) {
    // TODO: I don't like having to reproduce this logic by hand
    // but I have no idea what compiler API to use here
    private def jvmname(sym0: Symbol): String = {
      def loop(sym: Symbol): String = {
        val prefix = {
          if (sym.owner == RootClass || sym.owner == EmptyPackageClass) ""
          else {
            val ownerName = loop(sym.owner)
            val separator = if (sym.owner.isPackageClass) "/" else if (sym.owner.isModuleClass) "" else "$"
            ownerName + separator
          }
        }
        val suffix = if ((sym.isModule || sym.isModuleClass) && !sym.hasPackageFlag) "$" else ""
        prefix + sym.name.encoded + suffix
      }
      loop(sym0)
    }
    def jvmsig: String = {
      transformedType(tpe) match {
        case TypeRef(_, sym, args) =>
          require(args.nonEmpty ==> (sym == ArrayClass && args.length == 1))
          if (sym == UnitClass || sym == BoxedUnitClass) "V"
          else if (sym == BooleanClass) "Z"
          else if (sym == CharClass) "C"
          else if (sym == ByteClass) "B"
          else if (sym == ShortClass) "S"
          else if (sym == IntClass) "I"
          else if (sym == FloatClass) "F"
          else if (sym == LongClass) "J"
          else if (sym == DoubleClass) "D"
          else if (sym == ArrayClass) "[" + args.head.jvmsig
          else "L" + jvmname(sym) + ";"
        case MethodType(params, ret) =>
          s"(" + params.map(_.jvmsig).mkString("") + ")" + ret.jvmsig
        case ConstantType(value) =>
          value.tpe.widen.jvmsig
      }
    }
  }

  implicit class RichPlatformSymbol(sym: Symbol) {
    def jvmsig: String = {
      // NOTE: unfortunately, this simple-looking facility generates side effects that corrupt the state of the compiler
      // in particular, mixin composition stops working correctly, at least for `object Main extends App`
      // exitingDelambdafy(new genASM.JPlainBuilder(null, false).descriptor(sym))
      if (sym.isConstructor) sym.info.erasure.require[MethodType].copy(resultType = UnitClass.tpe).jvmsig
      else sym.info.jvmsig
    }
  }
}
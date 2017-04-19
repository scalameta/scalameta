package scala.meta.internal
package scalahost
package v1
package online

import scala.{meta => m}
import scala.meta.semantic.v1._

trait SymbolOps { self: Mirror =>

  implicit class XtensionGSymbolMSymbol(sym: g.Symbol) {
    def toSemantic: m.Symbol = {
      if (sym == null || sym == g.NoSymbol) return Symbol.None
      if (sym.isOverloaded) return Symbol.Multi(sym.alternatives.map(_.toSemantic))
      if (sym.isModuleClass) return sym.asClass.module.toSemantic
      if (sym.isRootPackage) return Symbol.Global(Symbol.None, Signature.Term("_root_"))
      if (sym.isEmptyPackage) return Symbol.Global(Symbol.None, Signature.Term("_empty_"))

      def isLocal(sym: g.Symbol): Boolean = {
        def definitelyGlobal = sym.hasPackageFlag
        def definitelyLocal =
          sym.name.decoded.startsWith(g.nme.LOCALDUMMY_PREFIX) ||
            sym.name.decoded == g.tpnme.REFINE_CLASS_NAME ||
            (sym.owner.isMethod && !sym.isParameter) ||
            ((sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter)
        !definitelyGlobal && (definitelyLocal || isLocal(sym.owner))
      }
      if (isLocal(sym)) return Symbol.Local(sym.pos.toSemantic)

      val owner = sym.owner.toSemantic
      val signature = {
        def name(sym: g.Symbol) = sym.name.decoded.stripSuffix(g.nme.LOCAL_SUFFIX_STRING)
        def jvmSignature(sym: g.MethodSymbol): String = {
          // NOTE: unfortunately, this simple-looking facility generates side effects that corrupt the state of the compiler
          // in particular, mixin composition stops working correctly, at least for `object Main extends App`
          // val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
          // exitingDelambdafy(new genASM.JPlainBuilder(null, false).descriptor(sym))
          def encode(tpe: g.Type): String = {
            val g.TypeRef(_, sym, args) = tpe
            require(args.isEmpty || sym == g.definitions.ArrayClass)
            if (sym == g.definitions.UnitClass) "V"
            else if (sym == g.definitions.BooleanClass) "Z"
            else if (sym == g.definitions.CharClass) "C"
            else if (sym == g.definitions.ByteClass) "B"
            else if (sym == g.definitions.ShortClass) "S"
            else if (sym == g.definitions.IntClass) "I"
            else if (sym == g.definitions.FloatClass) "F"
            else if (sym == g.definitions.LongClass) "J"
            else if (sym == g.definitions.DoubleClass) "D"
            else if (sym == g.definitions.ArrayClass) "[" + encode(args.head)
            else "L" + sym.fullName.replace(".", "/") + ";"
          }
          val g.MethodType(params, ret) = sym.info.erasure
          val jvmRet = if (!sym.isConstructor) ret else g.definitions.UnitClass.toType
          "(" + params.map(param => encode(param.info)).mkString("") + ")" + encode(jvmRet)
        }

        if (sym.isMethod && !sym.asMethod.isGetter) Signature.Method(name(sym), jvmSignature(sym.asMethod))
        else if (sym.isTypeParameter) Signature.TypeParameter(name(sym))
        else if (sym.isValueParameter || sym.isParamAccessor) Signature.TermParameter(name(sym))
        else if (sym.owner.thisSym == sym) Signature.Self(name(sym))
        else if (sym.isType) Signature.Type(name(sym))
        else Signature.Term(name(sym))
      }
      Symbol.Global(owner, signature)
    }
  }
}

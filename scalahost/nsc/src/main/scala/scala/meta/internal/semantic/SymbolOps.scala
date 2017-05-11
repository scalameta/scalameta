package scala.meta.internal
package semantic

import scala.{meta => m}
import scala.meta.internal.inputs._

trait SymbolOps { self: DatabaseOps =>

  implicit class XtensionGSymbolMSymbol(sym: g.Symbol) {
    def toSemantic: m.Symbol = {
      if (sym == null || sym == g.NoSymbol) return m.Symbol.None
      if (sym.isOverloaded) return m.Symbol.Multi(sym.alternatives.map(_.toSemantic))
      if (sym.isModuleClass) return sym.asClass.module.toSemantic
      if (sym.isRootPackage) return m.Symbol.Global(m.Symbol.None, m.Signature.Term("_root_"))
      if (sym.isEmptyPackage) return m.Symbol.Global(m.Symbol.None, m.Signature.Term("_empty_"))

      def isLocal(sym: g.Symbol): Boolean = {
        def definitelyGlobal = sym.hasPackageFlag
        def definitelyLocal =
          sym.name.decoded.startsWith(g.nme.LOCALDUMMY_PREFIX) ||
            sym.name.decoded == g.tpnme.REFINE_CLASS_NAME ||
            (sym.owner.isMethod && !sym.isParameter) ||
            ((sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter)
        !definitelyGlobal && (definitelyLocal || isLocal(sym.owner))
      }
      if (isLocal(sym)) return m.Symbol.Local(sym.pos.toMeta.syntax)

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

        if (sym.isMethod && !sym.asMethod.isGetter)
          m.Signature.Method(name(sym), jvmSignature(sym.asMethod))
        else if (sym.isTypeParameter) m.Signature.TypeParameter(name(sym))
        else if (sym.isValueParameter || sym.isParamAccessor) m.Signature.TermParameter(name(sym))
        else if (sym.owner.thisSym == sym) m.Signature.Self(name(sym))
        else if (sym.isType) m.Signature.Type(name(sym))
        else m.Signature.Term(name(sym))
      }
      m.Symbol.Global(owner, signature)
    }
  }
}

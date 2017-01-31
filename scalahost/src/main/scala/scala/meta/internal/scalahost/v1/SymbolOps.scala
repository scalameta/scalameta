package scala.meta.internal
package scalahost
package v1

import scala.{meta => m}

trait SymbolOps extends ReflectionToolkit {
  implicit class XtensionGSymbol(sym: g.Symbol) {
    def toSemantic: m.Symbol = {
      def symbolId(sym: g.Symbol): String = {
        if (sym == null || sym == g.NoSymbol) return null
        if (sym.isOverloaded) return sym.alternatives.flatMap(alt => Option(symbolId(alt))).mkString(";")
        if (sym.isModuleClass) return symbolId(sym.asClass.module)
        if (sym.isRootPackage) return "_root_."
        if (sym.isEmptyPackage) return "_empty_."

        def isLocal(sym: g.Symbol): Boolean = {
          def definitelyGlobal = sym.hasPackageFlag
          def definitelyLocal =
            sym.name.decoded.startsWith(g.nme.LOCALDUMMY_PREFIX) ||
            sym.name.decoded == g.tpnme.REFINE_CLASS_NAME ||
            (sym.owner.isMethod && !sym.isParameter) ||
            ((sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter)
          !definitelyGlobal && (definitelyLocal || isLocal(sym.owner))
        }
        if (isLocal(sym)) {
          val jfile = sym.pos.source.file.file
          if (jfile == null) return null
          val uri = jfile.toURI.toString
          val point = sym.pos.point
          return s"$uri@$point"
        }

        var result = symbolId(sym.owner)
        def pretty(sym: g.Symbol) = sym.name.decoded.stripSuffix(g.nme.LOCAL_SUFFIX_STRING)
        result += {
          def encodeType(sym: g.Symbol): String = pretty(sym) + "#"
          def encodeTerm(sym: g.Symbol): String = pretty(sym) + "."
          def encodeMethod(sym: g.Symbol): String = {
            // NOTE: unfortunately, this simple-looking facility generates side effects that corrupt the state of the compiler
            // in particular, mixin composition stops working correctly, at least for `object Main extends App`
            // val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
            // exitingDelambdafy(new genASM.JPlainBuilder(null, false).descriptor(sym))
            def jvmSignature(tpe: g.Type): String = {
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
              else if (sym == g.definitions.ArrayClass) "[" + jvmSignature(args.head)
              else "L" + sym.fullName.replace(".", "/") + ";"
            }
            val g.MethodType(params, ret) = sym.info.erasure
            val jvmRet = if (!sym.isConstructor) ret else g.definitions.UnitClass.toType
            s"${pretty(sym)}(" + params.map(param => jvmSignature(param.info)).mkString("") + ")" + jvmSignature(jvmRet) + "."
          }
          def encodeTparam(sym: g.Symbol): String = "[" + pretty(sym) + "]"
          def encodeParam(sym: g.Symbol): String = "(" + pretty(sym) + ")"
          def encodeSelf(sym: g.Symbol): String = pretty(sym) + "=>"

          if (sym.isMethod && !sym.asMethod.isGetter) encodeMethod(sym)
          else if (sym.isTypeParameter) encodeTparam(sym)
          else if (sym.isValueParameter) encodeParam(sym)
          else if (sym.owner.thisSym == sym) encodeSelf(sym)
          else if (sym.isType) encodeType(sym)
          else encodeTerm(sym)
        }
        result
      }
      val id = symbolId(sym)
      if (id != null) m.Symbol(id) else null
    }
  }
}
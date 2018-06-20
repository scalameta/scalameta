package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SingletonType.{Tag => st}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.tools.scalap.scalax.rules.scalasig._

trait TypeOps { self: Scalacp =>
  implicit class XtensionTypeSType(tpe: Type) {
    def toSemantic(linkMode: LinkMode): s.Type = {
      def loop(tpe: Type): s.Type = {
        tpe match {
          case ByNameType(tpe) =>
            val stpe = loop(tpe)
            s.ByNameType(stpe)
          case RepeatedType(tpe) =>
            val stpe = loop(tpe)
            s.RepeatedType(stpe)
          case TypeRefType(pre, sym, args) =>
            val spre = if (tpe.hasNontrivialPrefix) loop(pre) else s.NoType
            val ssym = sym.ssym
            val sargs = args.map(loop)
            s.TypeRef(spre, ssym, sargs)
          case SingleType(pre, sym) =>
            val stag = st.SYMBOL
            val spre = if (tpe.hasNontrivialPrefix) loop(pre) else s.NoType
            val ssym = {
              // NOTE: Due to some unclear reason, Scalac sometimes saves
              // (or Scalap sometimes loads) single types that point to
              // companion classes, not module classes (see #1392).
              // We assume that it's a mistake and work around accordingly.
              val raw = sym.ssym
              if (raw.endsWith("#")) raw.stripSuffix("#") + "."
              else raw
            }
            s.SingletonType(stag, spre, ssym, 0, "")
          case ThisType(sym) =>
            val stag = st.THIS
            val ssym = sym.ssym
            s.SingletonType(stag, s.NoType, ssym, 0, "")
          // // FIXME: https://github.com/scalameta/scalameta/issues/1291
          // case g.SuperType(gpre, gmix) =>
          //   ???
          // // FIXME: Could it be that this missing pattern matching case may have been
          // // the reason for mysterious Metacp crashes?
          // // https://github.com/scalameta/scalameta/issues/1494
          // case g.ConstantType(g.Constant(sym: g.TermSymbol)) if sym.hasFlag(gf.JAVA_ENUM) =>
          //   ???
          case ConstantType(underlying: Type) =>
            loop(underlying) match {
              case s.NoType => s.NoType
              case sarg =>
                val ssym = "java.lang.Class#"
                val sargs = sarg :: Nil
                s.TypeRef(s.NoType, ssym, sargs)
            }
          case ConstantType(const) =>
            def floatBits(x: Float) = java.lang.Float.floatToRawIntBits(x).toLong
            def doubleBits(x: Double) = java.lang.Double.doubleToRawLongBits(x)
            const match {
              case () =>
                s.SingletonType(st.UNIT, s.NoType, Symbols.None, 0, "")
              case false =>
                s.SingletonType(st.BOOLEAN, s.NoType, Symbols.None, 0, "")
              case true =>
                s.SingletonType(st.BOOLEAN, s.NoType, Symbols.None, 1, "")
              case x: Byte =>
                s.SingletonType(st.BYTE, s.NoType, Symbols.None, x.toLong, "")
              case x: Short =>
                s.SingletonType(st.SHORT, s.NoType, Symbols.None, x.toLong, "")
              case x: Char =>
                s.SingletonType(st.CHAR, s.NoType, Symbols.None, x.toLong, "")
              case x: Int =>
                s.SingletonType(st.INT, s.NoType, Symbols.None, x.toLong, "")
              case x: Long =>
                s.SingletonType(st.LONG, s.NoType, Symbols.None, x, "")
              case x: Float =>
                s.SingletonType(st.FLOAT, s.NoType, Symbols.None, floatBits(x), "")
              case x: Double =>
                s.SingletonType(st.DOUBLE, s.NoType, Symbols.None, doubleBits(x), "")
              case x: String =>
                s.SingletonType(st.STRING, s.NoType, Symbols.None, 0, x)
              case null =>
                s.SingletonType(st.NULL, s.NoType, Symbols.None, 0, "")
              case other =>
                sys.error(s"unsupported const $other")
            }
          case RefinedType(sym, parents) =>
            val sparents = parents.map(loop)
            val stpe = s.WithType(sparents)
            val sdecls = Some(sym.children.sscope(HardlinkChildren))
            s.StructuralType(stpe, sdecls)
          case AnnotatedType(tpe, anns) =>
            val sanns = anns.reverse.map(_.toSemantic)
            val stpe = loop(tpe)
            s.AnnotatedType(sanns, stpe)
          case ExistentialType(tpe, tparams) =>
            val stpe = loop(tpe)
            val sdecls = Some(tparams.sscope(HardlinkChildren))
            s.ExistentialType(stpe, sdecls)
          case ClassInfoType(sym, parents) =>
            val stparams = Some(s.Scope())
            val sparents = parents.map(loop)
            val sdecls = Some(sym.semanticdbDecls.sscope(linkMode))
            s.ClassInfoType(stparams, sparents, sdecls)
          case _: NullaryMethodType | _: MethodType =>
            val stparams = Some(s.Scope())
            val sparamss = tpe.paramss.map(_.sscope(linkMode))
            val sret = loop(tpe.ret)
            s.MethodType(stparams, sparamss, sret)
          case TypeBoundsType(lo, hi) =>
            val stparams = Some(s.Scope())
            val slo = loop(lo)
            val shi = loop(hi)
            s.TypeType(stparams, slo, shi)
          case PolyType(tpe, tparams) =>
            loop(tpe) match {
              case s.NoType => s.NoType
              case t: s.ClassInfoType =>
                val stparams = tparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.MethodType =>
                val stparams = tparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.TypeType =>
                val stparams = tparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case stpe =>
                val stparams = tparams.sscope(HardlinkChildren)
                s.UniversalType(Some(stparams), stpe)
            }
          case NoType =>
            s.NoType
          case NoPrefixType =>
            s.NoType
          case other =>
            sys.error(s"unsupported type $other")
        }
      }
      loop(tpe)
    }
  }

  implicit class XtensionType(tpe: Type) {
    def prefix: Type = {
      tpe match {
        case TypeRefType(pre, _, _) => pre
        case SingleType(pre, _) => pre
        case _ => NoType
      }
    }
    def symbol: Symbol = {
      tpe match {
        case TypeRefType(_, sym, _) => sym
        case SingleType(_, sym) => sym
        case ThisType(sym) => sym
        case _ => NoSymbol
      }
    }
    // FIXME: https://github.com/scalameta/scalameta/issues/1343
    def hasNontrivialPrefix: Boolean = {
      val kind = tpe.prefix.symbol.kind
      kind != k.OBJECT && kind != k.PACKAGE && kind != k.PACKAGE_OBJECT
    }
    def paramss: List[List[SymbolInfoSymbol]] = {
      tpe match {
        case NullaryMethodType(_) =>
          Nil
        case MethodType(tpe, params) =>
          val symbolInfoParams = params.map(_.asInstanceOf[SymbolInfoSymbol])
          symbolInfoParams.toList +: tpe.paramss
        case _ =>
          Nil
      }
    }
    def ret: Type = {
      tpe match {
        case NullaryMethodType(tpe) => tpe.ret
        case MethodType(tpe, _) => tpe.ret
        case _ => tpe
      }
    }
  }

  private object ByNameType {
    def unapply(tpe: Type): Option[Type] = {
      tpe match {
        case TypeRefType(_, sym, List(tpe)) if sym.name == "<byname>" => Some(tpe)
        case _ => None
      }
    }
  }

  private object RepeatedType {
    def unapply(tpe: Type): Option[Type] = {
      tpe match {
        case TypeRefType(_, sym, List(tpe)) if sym.name == "<repeated>" => Some(tpe)
        case _ => None
      }
    }
  }
}

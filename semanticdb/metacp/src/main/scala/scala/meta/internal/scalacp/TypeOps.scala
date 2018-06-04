package scala.meta.internal.scalacp

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.SingletonType.{Tag => st}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.Type.{Tag => t}
import scala.tools.scalap.scalax.rules.scalasig._

trait TypeOps { self: Scalacp =>
  implicit class XtensionTypeSType(tpe: Type) {
    def toSemantic(linkMode: LinkMode): Option[s.Type] = {
      def loop(tpe: Type): Option[s.Type] = {
        tpe match {
          case ByNameType(tpe) =>
            val stag = t.BY_NAME_TYPE
            val stpe = loop(tpe)
            Some(s.Type(tag = stag, byNameType = Some(s.ByNameType(stpe))))
          case RepeatedType(tpe) =>
            val stag = t.REPEATED_TYPE
            val stpe = loop(tpe)
            Some(s.Type(tag = stag, repeatedType = Some(s.RepeatedType(stpe))))
          case TypeRefType(pre, sym, args) =>
            val stag = t.TYPE_REF
            val spre = if (tpe.hasNontrivialPrefix) loop(pre) else None
            val ssym = sym.ssym
            val sargs = args.flatMap(loop)
            Some(s.Type(tag = stag, typeRef = Some(s.TypeRef(spre, ssym, sargs))))
          case SingleType(pre, sym) =>
            val stag = t.SINGLETON_TYPE
            val stpe = {
              val stag = st.SYMBOL
              val spre = if (tpe.hasNontrivialPrefix) loop(pre) else None
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
            }
            Some(s.Type(tag = stag, singletonType = Some(stpe)))
          case ThisType(sym) =>
            val stag = t.SINGLETON_TYPE
            val stpe = {
              val stag = st.THIS
              val ssym = sym.ssym
              s.SingletonType(stag, None, ssym, 0, "")
            }
            Some(s.Type(tag = stag, singletonType = Some(stpe)))
          // // FIXME: https://github.com/scalameta/scalameta/issues/1291
          // case g.SuperType(gpre, gmix) =>
          //   ???
          // // FIXME: Could it be that this missing pattern matching case may have been
          // // the reason for mysterious Metacp crashes?
          // // https://github.com/scalameta/scalameta/issues/1494
          // case g.ConstantType(g.Constant(sym: g.TermSymbol)) if sym.hasFlag(gf.JAVA_ENUM) =>
          //   ???
          case ConstantType(underlying: Type) =>
            loop(underlying).map { sarg =>
              val stag = t.TYPE_REF
              val ssym = "java.lang.Class#"
              val sargs = sarg :: Nil
              s.Type(tag = stag, typeRef = Some(s.TypeRef(None, ssym, sargs)))
            }
          case ConstantType(const) =>
            val stag = t.SINGLETON_TYPE
            val stpe = {
              def floatBits(x: Float) = java.lang.Float.floatToRawIntBits(x).toLong
              def doubleBits(x: Double) = java.lang.Double.doubleToRawLongBits(x)
              const match {
                case () => s.SingletonType(st.UNIT, None, Symbols.None, 0, "")
                case false => s.SingletonType(st.BOOLEAN, None, Symbols.None, 0, "")
                case true => s.SingletonType(st.BOOLEAN, None, Symbols.None, 1, "")
                case x: Byte => s.SingletonType(st.BYTE, None, Symbols.None, x.toLong, "")
                case x: Short => s.SingletonType(st.SHORT, None, Symbols.None, x.toLong, "")
                case x: Char => s.SingletonType(st.CHAR, None, Symbols.None, x.toLong, "")
                case x: Int => s.SingletonType(st.INT, None, Symbols.None, x.toLong, "")
                case x: Long => s.SingletonType(st.LONG, None, Symbols.None, x, "")
                case x: Float => s.SingletonType(st.FLOAT, None, Symbols.None, floatBits(x), "")
                case x: Double => s.SingletonType(st.DOUBLE, None, Symbols.None, doubleBits(x), "")
                case x: String => s.SingletonType(st.STRING, None, Symbols.None, 0, x)
                case null => s.SingletonType(st.NULL, None, Symbols.None, 0, "")
                case other => sys.error(s"unsupported const $other")
              }
            }
            Some(s.Type(tag = stag, singletonType = Some(stpe)))
          case RefinedType(sym, parents) =>
            val stag = t.STRUCTURAL_TYPE
            val stpe = {
              val sparents = parents.flatMap(loop)
              Some(s.Type(tag = t.WITH_TYPE, withType = Some(s.WithType(sparents))))
            }
            val sdecls = Some(sym.children.sscope(HardlinkChildren))
            Some(s.Type(tag = stag, structuralType = Some(s.StructuralType(stpe, sdecls))))
          case AnnotatedType(tpe, anns) =>
            val stag = t.ANNOTATED_TYPE
            val sanns = anns.reverse.map(_.toSemantic)
            val stpe = loop(tpe)
            Some(s.Type(tag = stag, annotatedType = Some(s.AnnotatedType(sanns, stpe))))
          case ExistentialType(tpe, tparams) =>
            val stag = t.EXISTENTIAL_TYPE
            val stpe = loop(tpe)
            val sdecls = Some(tparams.sscope(HardlinkChildren))
            Some(s.Type(tag = stag, existentialType = Some(s.ExistentialType(stpe, sdecls))))
          case ClassInfoType(sym, parents) =>
            val stag = t.CLASS_INFO_TYPE
            val sparents = parents.flatMap(loop)
            val sdecls = Some(sym.semanticdbDecls.sscope(linkMode))
            Some(s.Type(tag = stag, classInfoType = Some(s.ClassInfoType(None, sparents, sdecls))))
          case _: NullaryMethodType | _: MethodType =>
            val stag = t.METHOD_TYPE
            val sparamss = tpe.paramss.map(_.sscope(linkMode))
            val sret = loop(tpe.ret)
            Some(s.Type(tag = stag, methodType = Some(s.MethodType(None, sparamss, sret))))
          case TypeBoundsType(lo, hi) =>
            val stag = t.TYPE_TYPE
            val slo = loop(lo)
            val shi = loop(hi)
            Some(s.Type(tag = stag, typeType = Some(s.TypeType(None, slo, shi))))
          case PolyType(tpe, tparams) =>
            val stpe = loop(tpe)
            stpe.map { stpe =>
              if (stpe.tag == t.CLASS_INFO_TYPE) {
                val stparams = tparams.sscope(linkMode)
                stpe.update(_.classInfoType.typeParameters := stparams)
              } else if (stpe.tag == t.METHOD_TYPE) {
                val stparams = tparams.sscope(linkMode)
                stpe.update(_.methodType.typeParameters := stparams)
              } else if (stpe.tag == t.TYPE_TYPE) {
                val stparams = tparams.sscope(linkMode)
                stpe.update(_.typeType.typeParameters := stparams)
              } else {
                val stag = t.UNIVERSAL_TYPE
                val stparams = tparams.sscope(HardlinkChildren)
                s.Type(
                  tag = stag,
                  universalType = Some(s.UniversalType(Some(stparams), Some(stpe))))
              }
            }
          case NoType =>
            None
          case NoPrefixType =>
            None
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

package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.scalacp._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Scala._
import scala.reflect.internal.{Flags => gf}

trait TypeOps { self: SemanticdbOps =>
  implicit class XtensionGTypeSType(gtpe: g.Type) {
    def toSemanticTpe: s.Type = {
      def loop(gtpe: g.Type): s.Type = {
        gtpe match {
          case ByNameType(gtpe) =>
            val stpe = loop(gtpe)
            s.ByNameType(stpe)
          case RepeatedType(gtpe) =>
            val stpe = loop(gtpe)
            s.RepeatedType(stpe)
          case g.TypeRef(gpre, gsym, gargs) =>
            val spre = if (gtpe.hasTrivialPrefix) s.NoType else loop(gpre)
            val ssym = gsym.ssym
            val sargs = gargs.map(loop)
            s.TypeRef(spre, ssym, sargs)
          case g.SingleType(gpre, gsym) =>
            val spre = if (gtpe.hasTrivialPrefix) s.NoType else loop(gpre)
            val ssym = gsym.ssym
            s.SingleType(spre, ssym)
          case g.ThisType(gsym) =>
            val ssym = gsym.ssym
            s.ThisType(ssym)
          case g.SuperType(gpre, gmix) =>
            val spre = loop(gpre.typeSymbol.tpe)
            val ssym = gmix.typeSymbol.ssym
            s.SuperType(spre, ssym)
          case g.ConstantType(g.Constant(sym: g.TermSymbol)) if sym.hasFlag(gf.JAVA_ENUM) =>
            loop(g.SingleType(sym.owner.thisPrefix, sym))
          case g.ConstantType(g.Constant(_: g.Type)) =>
            loop(gtpe.widen)
          case g.ConstantType(gconst) =>
            val sconst = s.Constant(gconst.value)
            s.ConstantType(sconst)
          case g.RefinedType(gparents, gdecls) =>
            val sparents = gparents.map(loop)
            val stpe = s.WithType(sparents)
            val sdecls = Some(gdecls.semanticdbDecls.sscope(HardlinkChildren))
            s.StructuralType(stpe, sdecls)
          case g.AnnotatedType(ganns, gtpe) =>
            val sanns = ganns.reverse.map(_.toSemantic)
            val stpe = loop(gtpe)
            s.AnnotatedType(sanns, stpe)
          case g.ExistentialType(gtparams, gtpe) =>
            val stpe = loop(gtpe)
            val sdecls = Some(gtparams.sscope(HardlinkChildren))
            s.ExistentialType(stpe, sdecls)
          case g.PolyType(gtparams, gtpe) =>
            loop(gtpe) match {
              case s.NoType =>
                s.NoType
              case stpe =>
                val stparams = gtparams.sscope(HardlinkChildren)
                s.UniversalType(Some(stparams), stpe)
            }
          case g.NoType =>
            s.NoType
          case g.NoPrefix =>
            s.NoType
          case g.ErrorType =>
            s.NoType
          case gother =>
            sys.error(s"unsupported type ${gother}: ${g.showRaw(gother)}")
        }
      }
      loop(gtpe)
    }
    def toSemanticSig(linkMode: LinkMode): s.Signature = {
      def loop(gtpe: g.Type): s.Signature = {
        gtpe match {
          case g.ClassInfoType(gparents, _, gclass) =>
            val stparams = Some(s.Scope())
            val sparents = gparents.map(_.toSemanticTpe)
            val sself = gclass.self.toSemanticTpe
            val sdecls = Some(gclass.semanticdbDecls.sscope(linkMode))
            s.ClassSignature(stparams, sparents, sself, sdecls)
          case g.NullaryMethodType(gtpe) =>
            val stparams = Some(s.Scope())
            val stpe = gtpe.toSemanticTpe
            s.MethodSignature(stparams, Nil, stpe)
          case gtpe: g.MethodType =>
            def flatten(gtpe: g.Type): (List[List[g.Symbol]], g.Type) = {
              gtpe match {
                case g.MethodType(ghead, gtpe) =>
                  val (gtail, gret) = flatten(gtpe)
                  (ghead :: gtail, gret)
                case gother =>
                  (Nil, gother)
              }
            }
            val (gparamss, gret) = flatten(gtpe)
            val stparams = Some(s.Scope())
            val sparamss = gparamss.map(_.sscope(linkMode))
            val sret = gret.toSemanticTpe
            s.MethodSignature(stparams, sparamss, sret)
          case g.TypeBounds(glo, ghi) =>
            val stparams = Some(s.Scope())
            val slo = glo.toSemanticTpe
            val shi = ghi.toSemanticTpe
            s.TypeSignature(stparams, slo, shi)
          case g.PolyType(gtparams, gtpe) =>
            loop(gtpe) match {
              case s.NoSignature =>
                s.NoSignature
              case t: s.ClassSignature =>
                val stparams = gtparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.MethodSignature =>
                val stparams = gtparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.TypeSignature =>
                val stparams = gtparams.sscope(linkMode)
                t.copy(typeParameters = Some(stparams))
              case t: s.ValueSignature =>
                val stparams = gtparams.sscope(HardlinkChildren)
                val stpe = t.tpe
                s.ValueSignature(s.UniversalType(Some(stparams), stpe))
            }
          case g.NoType =>
            s.NoSignature
          case g.ErrorType =>
            s.NoSignature
          case gother =>
            s.ValueSignature(gother.toSemanticTpe)
        }
      }
      loop(gtpe)
    }
  }

  implicit class XtensionGType(gtpe: g.Type) {
    def hasTrivialPrefix: Boolean = {
      gtpe match {
        case g.TypeRef(gpre, gsym, _) => checkTrivialPrefix(gpre, gsym)
        case g.SingleType(gpre, gsym) => checkTrivialPrefix(gpre, gsym)
        case _ => false
      }
    }
    private def checkTrivialPrefix(gpre: g.Type, gsym: g.Symbol): Boolean = {
      gpre =:= gsym.owner.thisType
    }
  }

  object ByNameType {
    def unapply(gtpe: g.Type): Option[g.Type] = gtpe match {
      case g.TypeRef(_, g.definitions.ByNameParamClass, garg :: Nil) => Some(garg)
      case _ => None
    }
  }

  object RepeatedType {
    def unapply(gtpe: g.Type): Option[g.Type] = gtpe match {
      case g.TypeRef(_, g.definitions.RepeatedParamClass, garg :: Nil) => Some(garg)
      case g.TypeRef(_, g.definitions.JavaRepeatedParamClass, garg :: Nil) => Some(garg)
      case _ => None
    }
  }
}

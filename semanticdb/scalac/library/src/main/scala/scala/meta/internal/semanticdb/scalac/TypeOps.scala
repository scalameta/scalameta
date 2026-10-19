package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.{semanticdb => s}

import scala.annotation.tailrec
import scala.reflect.internal.{Flags => gf}

trait TypeOps {
  self: SemanticdbOps =>
  implicit class XtensionGTypeSType(gtpe: g.Type) {
    def toSemanticTpe: s.Type = {
      // A repeated type (`T*`) is only valid in parameter position; scalac widens a Scala repeated
      // to `scala.Seq[T]` everywhere else (e.g. the synthetic `unapplySeq` return), but leaves it
      // un-widened in some pickled positions such as a case class's synthetic
      // `AbstractFunction1[T*, C]` parent. Widen any nested Scala repeated so we never emit a
      // RepeatedType as a type argument. Scoped to `RepeatedParamClass`: a Java repeated
      // (`JavaRepeatedParamClass`, which would widen to `Array[T]`, not `Seq[T]`) only ever occurs
      // in parameter position, so it never reaches here. See scalameta/scalameta#1497.
      def widenRepeated(gtpe: g.Type): s.Type = gtpe match {
        case gtpe: g.TypeRef
            if gtpe.sym == g.definitions.RepeatedParamClass && gtpe.args.lengthCompare(1) == 0 =>
          s.TypeRef(s.NoType, "scala/package.Seq#", loop(gtpe.args.head) :: Nil)
        case _ => loop(gtpe)
      }
      def loop(gtpe: g.Type): s.Type = gtpe match {
        case gtpe: g.TypeRef =>
          val gsym = gtpe.sym
          gtpe.args match {
            case garg :: Nil if gsym == g.definitions.ByNameParamClass => s.ByNameType(loop(garg))
            case garg :: Nil
                if gsym == g.definitions.RepeatedParamClass ||
                  gsym == g.definitions.JavaRepeatedParamClass => s.RepeatedType(loop(garg))
            case gargs =>
              val spre = if (gtpe.hasTrivialPrefix) s.NoType else loop(gtpe.pre)
              s.TypeRef(spre, gsym.ssym, gargs.map(widenRepeated))
          }
        case gtpe: g.SingleType =>
          val spre = if (gtpe.hasTrivialPrefix) s.NoType else loop(gtpe.pre)
          s.SingleType(spre, gtpe.sym.ssym)
        case gtpe: g.ThisType => s.ThisType(gtpe.sym.ssym)
        case gtpe: g.SuperType =>
          val spre = loop(gtpe.thistpe.typeSymbol.tpe)
          s.SuperType(spre, gtpe.supertpe.typeSymbol.ssym)
        case gtpe: g.ConstantType => gtpe.value.value match {
            case sym: g.TermSymbol if sym.hasFlag(gf.JAVA_ENUM) =>
              loop(g.SingleType(sym.owner.thisPrefix, sym))
            case _: g.Type => loop(gtpe.widen)
            case value => s.ConstantType(s.Constant(value))
          }
        case gtpe: g.RefinedType =>
          val stpe = s.WithType(gtpe.parents.map(loop))
          val sdecls = Some(gtpe.decls.semanticdbDecls.sscope(HardlinkChildren))
          s.StructuralType(stpe, sdecls)
        case gtpe: g.AnnotatedType =>
          val sanns = gtpe.annotations.reverse.map(_.toSemantic)
          s.AnnotatedType(sanns, loop(gtpe.underlying))
        case gtpe: g.ExistentialType =>
          val stpe = loop(gtpe.underlying)
          s.ExistentialType(stpe, Some(gtpe.quantified.sscope(HardlinkChildren)))
        case gtpe: g.PolyType => loop(gtpe.resultType) match {
            case s.NoType => s.NoType
            case stpe =>
              val stparams = gtpe.typeParams.sscope(HardlinkChildren)
              s.UniversalType(Some(stparams), stpe)
          }
        // the bound of an existential `val` in a self type keeps the type of its getter
        case gtpe: g.NullaryMethodType => loop(gtpe.resultType)
        case g.NoType => s.NoType
        case g.NoPrefix => s.NoType
        case g.ErrorType => s.NoType

        /**
         * From the Scala compiler:
         * ```
         * An object representing an unknown type, used during type inference. If you see
         * WildcardType outside of inference it is almost certainly a bug.
         * ```
         * This is essentially the same as ErrorType here.
         */
        case g.WildcardType => s.NoType
        case _: g.BoundedWildcardType => s.NoType
        case _: g.ClassInfoType => s.NoType
        case _ => s.NoType
      }
      loop(gtpe)
    }
    def toSemanticSig(linkMode: LinkMode): s.Signature = {
      def loop(gtpe: g.Type): s.Signature = gtpe match {
        case gtpe: g.ClassInfoType =>
          val stparams = Some(s.Scope())
          val sparents = gtpe.parents.map(_.toSemanticTpe)
          val gclass = gtpe.typeSymbol
          val sself = gclass.self.toSemanticTpe
          val sdecls = Some(gclass.semanticdbDecls.sscope(linkMode))
          s.ClassSignature(stparams, sparents, sself, sdecls)
        case gtpe: g.NullaryMethodType =>
          val stparams = Some(s.Scope())
          val stpe = gtpe.resultType.toSemanticTpe
          s.MethodSignature(stparams, Nil, stpe)
        case gtpe: g.MethodType =>
          val sparamss = List.newBuilder[s.Scope]
          @tailrec
          def flatten(gtpe: g.Type): g.Type = gtpe match {
            case gtpe: g.MethodType =>
              sparamss += gtpe.params.sscope(linkMode)
              flatten(gtpe.resultType)
            case gret => gret
          }
          val sret = flatten(gtpe).toSemanticTpe
          s.MethodSignature(Some(s.Scope()), sparamss.result(), sret)
        case gtpe: g.TypeBounds =>
          val stparams = Some(s.Scope())
          val slo = gtpe.lo.toSemanticTpe
          val shi = gtpe.hi.toSemanticTpe
          s.TypeSignature(stparams, slo, shi)
        case gtpe: g.PolyType =>
          val gtparams = gtpe.typeParams
          loop(gtpe.resultType) match {
            case s.NoSignature => s.NoSignature
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
        case g.NoType => s.NoSignature
        case g.ErrorType => s.NoSignature
        case gother => s.ValueSignature(gother.toSemanticTpe)
      }
      loop(gtpe)
    }
  }

  implicit class XtensionGType(gtpe: g.Type) {
    def hasTrivialPrefix: Boolean = gtpe match {
      case gtpe: g.TypeRef => checkTrivialPrefix(gtpe.pre, gtpe.sym)
      case gtpe: g.SingleType => checkTrivialPrefix(gtpe.pre, gtpe.sym)
      case _ => false
    }
    private def checkTrivialPrefix(gpre: g.Type, gsym: g.Symbol): Boolean = gpre =:=
      gsym.owner.thisType
  }
}

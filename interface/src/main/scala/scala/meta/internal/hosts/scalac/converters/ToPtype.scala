package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.internal.Flags._
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.internal.{ast => p}
import scala.meta.internal.{hygiene => h}
import java.util.UUID.randomUUID

// This module exposes a method that can convert scala.reflect types into equivalent scala.meta types.
// It is impossible to attach custom information to types, so we can't guarantee high-fidelity
// of the conversion in the sense that toPtree guarantees.
//
// Nevertheless, this shouldn't be a problem for us, because our representation doesn't care about
// particularities of short names, fully-qualified names, etc. The only potentially negative effect here
// is aesthetics (and prettyprinting, which we'll need to adapt to account for hygiene - but even before that
// we have show[Semantics], so it's relatively fine).
trait ToPtype extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToPtype(gtpe: g.Type) {
    def toPtype: p.Type = gtpe.toPtypeArg.require[p.Type]
    def toPtypeArg: p.Type.Arg = tpeCache.getOrElseUpdate(gtpe, {
      val result = gtpe match {
        case g.NoPrefix =>
          unreachable
        case g.NoType =>
          unreachable
        case g.SuperType(thistpe, supertpe) =>
          require(thistpe.isInstanceOf[g.ThisType] && thistpe.typeSymbol.isType && supertpe.typeSymbol.isType)
          val superdumb = p.Term.Super(Some(thistpe.typeSymbol.name.toString), Some(supertpe.typeSymbol.name.toString)).withOriginal(gtpe)
          val superpre = thistpe
          val supersym = if (supertpe.isInstanceOf[g.RefinedType]) g.NoSymbol else supertpe.typeSymbol
          p.Type.Singleton(superdumb.withDenot(thistpe, supersym))
        case g.ThisType(sym) =>
          require(sym.isClass)
          if (sym.isModuleClass) p.Type.Singleton(sym.module.asTerm.rawcvt(g.Ident(sym.module)).withOriginal(gtpe))
          else p.Type.Singleton(p.Term.This(Some(sym.name.toString)).withDenot(sym).withOriginal(gtpe))
        case g.SingleType(pre, sym) =>
          require(sym.isTerm)
          val ref = (pre match {
            case g.NoPrefix =>
              sym.asTerm.precvt(pre, g.Ident(sym))
            case pre if pre.typeSymbol.isStaticOwner =>
              sym.asTerm.precvt(pre, g.Ident(sym))
            case pre: g.SingletonType =>
              val p.Type.Singleton(preref) = pre.toPtype
              p.Term.Select(preref, sym.asTerm.precvt(pre, g.Ident(sym)).withOriginal(gtpe))
            case pre @ g.TypeRef(g.NoPrefix, quant, Nil) if quant.hasFlag(DEFERRED | EXISTENTIAL) =>
              require(quant.name.endsWith(g.nme.SINGLETON_SUFFIX))
              val prename = g.Ident(quant.name.toString.stripSuffix(g.nme.SINGLETON_SUFFIX)).alias
              val preref = p.Term.Name(prename).withDenot(quant).withOriginal(quant)
              p.Term.Select(preref, sym.asTerm.precvt(pre, g.Ident(sym)).withOriginal(gtpe))
            case pre: g.TypeRef =>
              // TODO: wow, so much for the hypothesis that all post-typer types are representable with syntax
              // here's one for you: take a look at `context.unit.synthetics.get` in Typers.scala
              // the prefix of the selection is typed as `Typers.this.global.CompilationUnit#synthetics.type`
              // from what I can see, we should represent this type as an existential, i.e.
              // `_1.synthetics.type forSome { val _1: Typers.this.global.CompilationUnit }`
              // however that representation would require non-trivial effort to pull off
              // (because we'll have to carry around that p.Type.Existential and unwrap it when anyone wants to use it)
              // therefore for now I'm just putting a stub here
              sym.asTerm.precvt(pre, g.Ident(sym))
            case _ =>
              sys.error(s"unsupported type $gtpe, prefix = ${pre.getClass}, structure = ${g.showRaw(gtpe, printIds = true, printTypes = true)}")
          }).withOriginal(gtpe)
          // NOTE: we can't just emit p.Type.Singleton(p.Term.Name(...).withDenot(pre, sym))
          // because in some situations (when the prefix is not stable) that will be a lie
          // because naked names are supposed to be usable without a prefix
          p.Type.Singleton(ref)
        case g.TypeRef(pre, sym, args) =>
          require(sym.isType)
          if (sym == g.definitions.RepeatedParamClass) {
            p.Type.Arg.Repeated(args.head.toPtype)
          } else if (sym == g.definitions.ByNameParamClass) {
            p.Type.Arg.ByName(args.head.toPtype)
          } else {
            val pref = ({
              if (sym.isModuleClass) {
                g.SingleType(pre, sym.module).toPtype
              } else {
                pre match {
                  case g.NoPrefix =>
                    sym.asType.precvt(pre, g.Ident(sym))
                  case pre if pre.typeSymbol.isStaticOwner =>
                    sym.asType.precvt(pre, g.Ident(sym))
                  case pre: g.SingletonType =>
                    val p.Type.Singleton(preref) = pre.toPtype
                    p.Type.Select(preref, sym.asType.precvt(pre, g.Ident(sym)).withOriginal(gtpe))
                  case _ =>
                    p.Type.Project(pre.toPtype, sym.asType.precvt(pre, g.Ident(sym)).withOriginal(gtpe))
                }
              }
            }).withOriginal(gtpe)
            val pargs = args.map(_.toPtype)
            if (args.isEmpty) pref
            else {
              if (g.definitions.FunctionClass.seq.contains(sym)) p.Type.Function(pargs.init, pargs.last)
              else if (g.definitions.TupleClass.seq.contains(sym) && pargs.length > 1) p.Type.Tuple(pargs)
              else if (sym.name.looksLikeInfix && pref.isInstanceOf[p.Type.Name] && pargs.length == 2) p.Type.ApplyInfix(pargs(0), pref.require[p.Type.Name], pargs(1))
              else p.Type.Apply(pref, pargs)
            }
          }
        case g.RefinedType(parents, decls) =>
          // TODO: detect `val x, y: Int`
          val pdecls = decls.toLogical.map(_.toPmember(g.NoPrefix)).toList // TODO: actually, prefix here is not empty
          p.Type.Compound(parents.map(_.toPtype), pdecls.map(_.stat))
        case g.ExistentialType(quants, underlying) =>
          // TODO: infer type placeholders where they were specified explicitly
          require(quants.forall(quant => quant.isType && quant.hasFlag(DEFERRED | EXISTENTIAL)))
          val pquants = quants.toLogical.map(_.toPmember(g.NoPrefix)).toList // TODO: actually, prefix here is not empty
          p.Type.Existential(underlying.toPtype, pquants.map(_.stat))
        case g.AnnotatedType(annots, underlying) =>
          p.Type.Annotate(underlying.toPtype, annots.toPannots)
        case g.ConstantType(const) =>
          const.rawcvt
        case tpe @ g.PolyType(tparams, ret) =>
          // NOTE: it turns out that we can't avoid polytypes here
          // even though we never need to carry around type signatures of our members (those members are their own type signatures!)
          // there are legitimate polytypes, namely type lambdas
          tpe match {
            case EtaReduce(tpe) =>
              tpe.toPtype
            case _ =>
              // NOTE: `[A]T` is represented as `({ type λ[A] = T })#λ`
              // NOTE: it's good that we cache the gtpe => ptpe conversion
              // because otherwise, when repeatedly faced with the same polytype, we'd keep on churning out new hsymbols
              // and those would not compare equal on the scala.meta side
              val hsymbol = h.Symbol.Local(randomUUID().toString)
              val pname = p.Type.Name("λ", h.Denotation.Precomputed(h.Prefix.Zero, hsymbol), h.Sigma.Naive)
              val ptparams = tparams.toLogical.map(_.toPmember(g.NoPrefix).require[p.Type.Param])
              val plambda = p.Defn.Type(Nil, pname, ptparams, ret.toPtype)
              p.Type.Project(p.Type.Compound(Nil, List(plambda)), pname)
          }
        case _ =>
          sys.error(s"unsupported type $gtpe, designation = ${gtpe.getClass}, structure = ${g.showRaw(gtpe, printIds = true, printTypes = true)}")
      }
      result.withOriginal(gtpe)
    })
  }
}
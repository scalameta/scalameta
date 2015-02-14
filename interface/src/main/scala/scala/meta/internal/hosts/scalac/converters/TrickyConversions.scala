package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.internal.{ast => p}

// This module provides utilities to do tricky conversions solely within the scala.meta tree hierarchy:
// 1) Constructor calls <-> Types
// 2) Pattern types <-> Types
// These conversions need to account for metadata that we've attached or about to attach to scala.meta trees,
// so we can't move them to `org.scalameta.meta` and need this module to be part of the SemanticContext cake.
trait TrickyConversions extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichCtorRefTree(ptpe: p.Type) {
    def ctorRef(gctor: g.Symbol): p.Term = {
      object pTypes {
        def unapply(tpes: Seq[p.Type.Arg]): Option[Seq[p.Type]] = {
          if (tpes.forall(_.isInstanceOf[p.Type])) Some(tpes.map(_.require[p.Type]))
          else None
        }
      }
      val result = ptpe match {
        case p.Type.Name(value) => p.Ctor.Name(value).withDenot(gctor)
        case p.Type.Select(qual, name) => p.Ctor.Ref.Select(qual, p.Ctor.Name(name.value).withDenot(qual.originalTree.requireGet.tpe, gctor))
        case p.Type.Project(qual, name) => p.Ctor.Ref.Project(qual, p.Ctor.Name(name.value).withDenot(qual.originalTree.requireGet.tpe, gctor))
        case p.Type.Function(pTypes(params), ret) => p.Term.ApplyType(p.Ctor.Ref.Function(p.Ctor.Name("=>").withDenot(gctor)), params :+ ret)
        case p.Type.Annotate(tpe, annots) => p.Term.Annotate(tpe.ctorRef(gctor), annots)
        case p.Type.Apply(tpe, args) => p.Term.ApplyType(tpe.ctorRef(gctor), args)
        case _ => unreachable
      }
      val original = ptpe.scratchpad.collect{case ScratchpadDatum.Original(goriginal) => goriginal}.head
      result.appendScratchpad(ScratchpadDatum.Original(original))
    }
  }

  protected implicit class RichCtorTpeTerm(ptree: p.Term) {
    def ctorTpe: p.Type = {
      def loop(ptree: p.Term): p.Type = {
        val result = ptree match {
          case p.Ctor.Name(value) => p.Type.Name(value).withDenot(ptree.originalPre.requireGet, ptree.originalSym.requireGet)
          case p.Ctor.Ref.Select(qual, name) => p.Type.Select(qual, loop(name).require[p.Type.Name])
          case p.Ctor.Ref.Project(qual, name) => p.Type.Project(qual, loop(name).require[p.Type.Name])
          case p.Ctor.Ref.Function(_) => unreachable
          case p.Term.ApplyType(p.Ctor.Ref.Function(_), targs) => p.Type.Function(targs.init, targs.last)
          case p.Term.ApplyType(callee, targs) => p.Type.Apply(loop(callee), targs)
          case p.Term.Annotate(annottee, annots) => p.Type.Annotate(loop(annottee), annots)
          case _ => unreachable
        }
        result.withOriginal(ptree.originalTree.requireGet)
      }
      ptree match {
        case p.Term.Apply(callee, _) => callee.ctorTpe
        case _ => loop(ptree)
      }
    }
    def ctorArgss: Seq[Seq[p.Term.Arg]] = {
      ptree match {
        case _: p.Ctor.Ref => Nil
        case p.Term.ApplyType(callee, _) => callee.ctorArgss
        case p.Term.Apply(callee, args) => callee.ctorArgss :+ args
        case p.Term.Annotate(annottee, _) => annottee.ctorArgss
        case _ => unreachable
      }
    }
  }

  protected implicit class RichPatTpeTree(ptpe: p.Type) {
    def patTpe: p.Pat.Type = {
      def loop(ptpe: p.Type): p.Pat.Type = {
        val result = ptpe match {
          case ptpe: p.Type.Name => ptpe
          case ptpe: p.Type.Select => ptpe
          case p.Type.Project(pqual, pname) => p.Pat.Type.Project(loop(pqual), pname)
          case ptpe: p.Type.Singleton => ptpe
          case p.Type.Apply(ptpe, args) => p.Pat.Type.Apply(loop(ptpe), args.map(loop))
          case p.Type.ApplyInfix(plhs, pop, prhs) => p.Pat.Type.ApplyInfix(loop(plhs), pop, loop(prhs))
          case p.Type.Function(pparams, pres) => p.Pat.Type.Function(pparams.map(param => loop(param.require[p.Type])), loop(pres))
          case p.Type.Tuple(pelements) => p.Pat.Type.Tuple(pelements.map(loop))
          case p.Type.Compound(ptpes, prefinement) => p.Pat.Type.Compound(ptpes.map(loop), prefinement)
          case p.Type.Existential(ptpe, pquants) => p.Pat.Type.Existential(loop(ptpe), pquants)
          case p.Type.Annotate(ptpe, pannots) => p.Pat.Type.Annotate(loop(ptpe), pannots)
          case ptpe: p.Type.Placeholder => ptpe
          case ptpe: p.Lit => ptpe
        }
        val original = ptpe.scratchpad.collect{case ScratchpadDatum.Original(goriginal) => goriginal}.head
        result.appendScratchpad(ScratchpadDatum.Original(original))
      }
      loop(ptpe)
    }
  }
}
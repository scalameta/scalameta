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
import scala.{meta => papi}
import scala.meta.internal.{ast => p}
import scala.meta.internal.{hygiene => h}
import scala.reflect.internal.Flags._

// This module provides functionality for scala.reflect -> scala.meta conversions
// to keep track of scala.reflect attributes.
// By the way, not all semantic information can be expressed with scala.meta's denotations (yet?),
// so sometimes we have to save the original convertees to help us later.
trait Attributes extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected sealed trait ScratchpadDatum
  protected object ScratchpadDatum {
    case class Denotation(gpre: g.Type, lsym: l.Symbol) extends ScratchpadDatum
    case class Original(goriginal: Any) extends ScratchpadDatum
  }

  protected trait CanHaveDenot[T <: Tree]
  protected object CanHaveDenot {
    implicit def Name[T <: papi.Name]: CanHaveDenot[T] = null
  }

  protected implicit class RichAttributesTree[T <: p.Tree](ptree: T) {
    private def denot(gpre: g.Type, lsym: l.Symbol): h.Denotation = {
      require(gpre != g.NoType)
      val hpre = {
        if (gpre == g.NoPrefix) h.Prefix.Zero
        else h.Prefix.Type(gpre.toPtype)
      }
      val hsym = symbolTable.convert(lsym)
      h.Denotation.Precomputed(hpre, hsym)
    }
    def withDenot(gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      ptree.withDenot(gsym.toLogical)
    }
    def withDenot(lsym: l.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      def defaultpre(gsym: g.Symbol): g.Type = {
        if (gsym.hasFlag(EXISTENTIAL | PARAM)) g.NoPrefix
        else if (gsym.isConstructor) defaultpre(gsym.owner)
        else gsym.owner.thisType
      }
      ptree.withDenot(defaultpre(lsym.gsymbol), lsym)
    }
    def withDenot(gpre: g.Type, gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      ptree.withDenot(gpre, gsym.toLogical)
    }
    def withDenot(gpre: g.Type, lsym: l.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      val ptree0 = ptree // NOTE: this is here only to provide an unqualified Ident for the `require` macro
      require(ptree0.isInstanceOf[p.Name] && gpre != null && ((lsym == l.None) ==> ptree.isInstanceOf[p.Term.Super]))
      val scratchpad = ptree.scratchpad :+ ScratchpadDatum.Denotation(gpre, lsym)
      val ptree1 = ptree match {
        case ptree: p.Name.Anonymous => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Term.Name => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Type.Name => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        // TODO: some ctor refs don't have corresponding constructor symbols in Scala (namely, ones for traits)
        // in these cases, our lsym is going to be a symbol of the trait in question
        // we need to account for that in `symbolTable.convert` and create a constructor symbol of our own
        case ptree: p.Ctor.Name => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Term.This => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Term.Super => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.PrivateThis => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.PrivateWithin => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.ProtectedThis => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case ptree: p.Mod.ProtectedWithin => ptree.copy(denot = denot(gpre, lsym), sigma = h.Sigma.Naive)
        case _ => unreachable
      }
      ptree1.withScratchpad(scratchpad).asInstanceOf[T]
    }
    def withOriginal(gtree: g.Tree): T = ptree.appendScratchpad(ScratchpadDatum.Original(gtree)).asInstanceOf[T]
    def withOriginal(gtpe: g.Type): T = ptree.appendScratchpad(ScratchpadDatum.Original(gtpe)).asInstanceOf[T]
    def withOriginal(lsym: l.Symbol): T = ptree.appendScratchpad(ScratchpadDatum.Original(lsym)).asInstanceOf[T]
    def withOriginal(gsym: g.Symbol): T = ptree.withOriginal(gsym.toLogical)
    def withOriginal(gannot: g.AnnotationInfo): T = ptree.appendScratchpad(ScratchpadDatum.Original(gannot)).asInstanceOf[T]
    def originalTree: Option[g.Tree] = {
      ptree.scratchpad.collect { case ScratchpadDatum.Original(goriginal: g.Tree) => goriginal }.headOption
    }
    def originalTpe: Option[g.Type] = {
      val fromOriginal = ptree.scratchpad.collect { case ScratchpadDatum.Original(goriginal: g.Type) => goriginal }.headOption
      val fromAnnot = ptree.scratchpad.collect { case ScratchpadDatum.Original(goriginal: g.AnnotationInfo) => goriginal }.headOption
      fromOriginal.orElse(fromAnnot.map(_.tpe))
    }
    def originalPre: Option[g.Type] = {
      ptree.scratchpad.collect { case ScratchpadDatum.Denotation(gpre: g.Type, _) => gpre }.headOption
    }
    def originalSym: Option[l.Symbol] = {
      val fromOriginal = ptree.scratchpad.collect { case ScratchpadDatum.Original(loriginal: l.Symbol) => loriginal }.headOption
      val fromDenot = ptree.scratchpad.collect { case ScratchpadDatum.Denotation(_, lsym: l.Symbol) => lsym }.headOption
      fromOriginal.orElse(fromDenot)
    }
  }
}
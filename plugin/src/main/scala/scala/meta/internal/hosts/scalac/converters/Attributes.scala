package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.ClassTag
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.reflect.internal.Flags._

// This module provides functionality for scala.reflect -> scala.meta conversions
// to keep track of scala.reflect attributes.
// By the way, not all semantic information can be expressed with scala.meta's denotations (yet?),
// so sometimes we have to save the original convertees to help us later.
trait Attributes extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected trait CanHaveDenot[T <: Tree]
  protected object CanHaveDenot { implicit def Name[T <: mapi.Name]: CanHaveDenot[T] = null }
  protected trait CanHaveTpe[T <: Tree]
  protected object CanHaveTpe {
    implicit def Term[T <: mapi.Term]: CanHaveTpe[T] = null
    implicit def TermParam[T <: mapi.Term.Param]: CanHaveTpe[T] = null
    implicit def Weird[T <: mapi.Term with mapi.Term.Param]: CanHaveTpe[T] = null
  }

  protected implicit class RichAttributesTree[T <: m.Tree : ClassTag](mtree: T) {
    private def denot(gpre: g.Type, lsym: l.Symbol): s.Denotation = {
      require(gpre != g.NoType)
      val hpre = {
        if (gpre == g.NoPrefix) s.Prefix.Zero
        else s.Prefix.Type(gpre.toMtype)
      }
      val ssym = symbolTable.convert(lsym)
      s.Denotation.Single(hpre, ssym)
    }
    def withDenot(gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      mtree.withDenot(gsym.toLogical)
    }
    def tryDenot(gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      if (gsym == null || gsym == g.NoSymbol) mtree
      else mtree.withDenot(gsym)
    }
    def withDenot(lsym: l.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      mtree.withDenot(lsym.gsymbol.prefix, lsym)
    }
    def withDenot(gpre: g.Type, gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      mtree.withDenot(gpre, gsym.toLogical)
    }
    def tryDenot(gpre: g.Type, gsym: g.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      if (gpre == null || gpre == g.NoType) mtree
      else if (gsym == null || gsym == g.NoSymbol) mtree
      else mtree.withDenot(gsym)
    }
    def withDenot(gpre: g.Type, lsym: l.Symbol)(implicit ev: CanHaveDenot[T]): T = {
      require(((lsym == l.None) ==> (mtree.isInstanceOf[m.Name.Anonymous])) && debug(mtree, gpre, lsym))
      val ptree1 = mtree match {
        case mtree: m.Name.Anonymous => mtree.copy(denot = denot(gpre, lsym))
        case mtree: m.Name.Indeterminate => mtree.copy(denot = denot(gpre, lsym))
        case mtree: m.Term.Name => mtree.copy(denot = denot(gpre, lsym), typing = mtree.typing)
        case mtree: m.Type.Name => mtree.copy(denot = denot(gpre, lsym))
        // TODO: some ctor refs don't have corresponding constructor symbols in Scala (namely, ones for traits)
        // in these cases, our lsym is going to be a symbol of the trait in question
        // we need to account for that in `symbolTable.convert` and create a constructor symbol of our own
        case mtree: m.Ctor.Name => mtree.copy(denot = denot(gpre, lsym), typing = mtree.typing)
        case _ => unreachable(debug(mtree, mtree.show[Structure]))
      }
      ptree1.require[T]
    }
    def withDenot(ldenot: l.Denotation)(implicit ev: CanHaveDenot[T]): T = {
      require(ldenot.nonEmpty)
      mtree.withDenot(ldenot.pre, ldenot.sym)
    }
    def tryDenot(ldenot: l.Denotation)(implicit ev: CanHaveDenot[T]): T = {
      if (ldenot.isEmpty) mtree
      else mtree.withDenot(ldenot)
    }
    private def typing(gtpe: g.Type): s.Typing = {
      s.Typing.Known(gtpe.toMtypeArg)
    }
    def withTyping(gtpe: g.Type)(implicit ev: CanHaveTpe[T]): T = {
      // TODO: m.Term doesn't have copy, which is reasonable, because it's a branch trait
      // TODO: m.Term.Param doesn't have copy, which is totally unreasonable
      val ptree1 = mtree match {
      //   case mtree: m.Term.Name => mtree.copy(denot = mtree.denot, typing = typing(gtpe))
      //   case mtree: m.Term => mtree.copy(typing = typing(gtpe))
      //   case mtree: m.Term.Param => mtree.copy(typing = typing(gtpe))
      //   case _ => unreachable(debug(mtree, mtree.show[Raw]))
        case mtree: m.Ctor.Name => mtree.copy(denot = mtree.denot, typing = typing(gtpe))
        case _ => mtree
      }
      ptree1.require[T]
    }
    def tryTyping(gtpe: g.Type)(implicit ev: CanHaveTpe[T]): T = {
      if (gtpe == null || gtpe == g.NoType) mtree
      else mtree.withTyping(gtpe)
    }
  }
}
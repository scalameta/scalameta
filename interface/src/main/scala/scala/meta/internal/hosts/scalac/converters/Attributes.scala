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

  protected implicit class RichAttributesTree[T <: m.Tree : ClassTag](ptree: T) {
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
      require(((lsym == l.None) ==> (ptree.isInstanceOf[m.Name.Anonymous])) && debug(ptree, gpre, lsym))
      val ptree1 = ptree match {
        case ptree: m.Name.Anonymous => ptree.copy(denot = denot(gpre, lsym))
        case ptree: m.Name.Indeterminate => ptree.copy(denot = denot(gpre, lsym))
        case ptree: m.Term.Name => ptree.copy(denot = denot(gpre, lsym), typing = ptree.typing)
        case ptree: m.Type.Name => ptree.copy(denot = denot(gpre, lsym))
        // TODO: some ctor refs don't have corresponding constructor symbols in Scala (namely, ones for traits)
        // in these cases, our lsym is going to be a symbol of the trait in question
        // we need to account for that in `symbolTable.convert` and create a constructor symbol of our own
        case ptree: m.Ctor.Name => ptree.copy(denot = denot(gpre, lsym), typing = ptree.typing)
        case _ => unreachable(debug(ptree, ptree.show[Structure]))
      }
      ptree1.require[T]
    }
    private def typing(gtpe: g.Type): s.Typing = {
      s.Typing.Known(gtpe.toMtypeArg)
    }
    def withTpe(gtpe: g.Type)(implicit ev: CanHaveTpe[T]): T = {
      // TODO: m.Term doesn't have copy, which is reasonable, because it's a branch trait
      // TODO: m.Term.Param doesn't have copy, which is totally unreasonable
      val ptree1 = ptree match {
      //   case ptree: m.Term.Name => ptree.copy(denot = ptree.denot, typing = typing(gtpe))
      //   case ptree: m.Term => ptree.copy(typing = typing(gtpe))
      //   case ptree: m.Term.Param => ptree.copy(typing = typing(gtpe))
      //   case _ => unreachable(debug(ptree, ptree.show[Raw]))
        case ptree: m.Ctor.Name => ptree.copy(denot = ptree.denot, typing = typing(gtpe))
        case _ => ptree
      }
      ptree1.require[T]
    }
  }
}
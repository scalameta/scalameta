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
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}

// This module provides utilities shared between ToEnsugaredScalameta and ToDesugaredScalameta.
// TODO: Ultra-precise type annotations provided here are actually necessary for the crazy macro framework
// that services ToEnsugaredScalameta. We should see about getting rid of that and cutting these swaths of boilerplate.
trait ToM extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToScalametaSymbol(gsym: g.Symbol) {
    type mTermOrTypeName = m.Name{type ThisType >: m.Term.Name with m.Type.Name <: m.Name}
    type mTermOrTypeOrAnonymousName = m.Name{type ThisType >: m.Term.Name with m.Type.Name with m.Name.Anonymous <: m.Name}
    private def dumbcvt(in: g.Tree): m.Name = {
      if (gsym.isTerm) m.Term.Name(in.alias)
      else if (gsym.isType) m.Type.Name(in.alias)
      else unreachable
    }
    def precvt(pre: g.Type, in: g.Tree): mTermOrTypeName = dumbcvt(in).withDenot(pre, gsym).require[mTermOrTypeName]
    def rawcvt(in: g.Tree): mTermOrTypeName = dumbcvt(in).withDenot(gsym).require[mTermOrTypeName]
    def anoncvt(in: g.Tree): mTermOrTypeOrAnonymousName = (if (gsym.isAnonymous) m.Name.Anonymous() else dumbcvt(in)).withDenot(gsym).require[mTermOrTypeOrAnonymousName]
  }

  protected implicit class RichToScalametaTermSymbol(gsym: g.TermSymbol) {
    type mTermOrAnonymousName = m.Name{type ThisType >: m.Term.Name with m.Name.Anonymous <: m.Name}
    def precvt(pre: g.Type, in: g.Tree): m.Term.Name = (gsym: g.Symbol).precvt(pre, in).require[m.Term.Name]
    def rawcvt(in: g.Tree, allowNoSymbol: Boolean = false): m.Term.Name = (gsym: g.Symbol).rawcvt(in).require[m.Term.Name]
    def anoncvt(in: g.ValDef): mTermOrAnonymousName = (gsym: g.Symbol).anoncvt(in).require[mTermOrAnonymousName]
  }

  protected implicit class RichToScalametaTypeSymbol(gsym: g.TypeSymbol) {
    type mTypeOrAnonymousName = m.Name{type ThisType >: m.Type.Name with m.Name.Anonymous <: m.Name}
    def precvt(pre: g.Type, in: g.Tree): m.Type.Name = (gsym: g.Symbol).precvt(pre, in).require[m.Type.Name]
    def rawcvt(in: g.Tree): m.Type.Name = (gsym: g.Symbol).rawcvt(in).require[m.Type.Name]
    def anoncvt(in: g.TypeDef): mTypeOrAnonymousName = (gsym: g.Symbol).anoncvt(in).require[mTypeOrAnonymousName]
  }

  protected implicit class RichToScalametaConstant(gconst: g.Constant) {
    type mConst = m.Lit{type ThisType >: m.Lit.Null with m.Lit.Unit with m.Lit.Bool with m.Lit.Int with m.Lit.Long with m.Lit.Float with m.Lit.Double with m.Lit.String with m.Lit.Char <: m.Lit}
    def rawcvt: mConst = gconst.value match {
      case null => m.Lit.Null()
      case () => m.Lit.Unit()
      case v: Boolean => m.Lit.Bool(v)
      case v: Byte => m.Lit.Int(v)
      case v: Short => m.Lit.Int(v)
      case v: Int => m.Lit.Int(v)
      case v: Long => m.Lit.Long(v)
      case v: Float => m.Lit.Float(v)
      case v: Double => m.Lit.Double(v)
      case v: String => m.Lit.String(v)
      case v: Char => m.Lit.Char(v)
      case v: g.Type => unreachable
      case v: g.Symbol => unreachable
    }
  }
}
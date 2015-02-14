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

// This module provides utilities shared between ToEnsugaredScalameta and ToDesugaredScalameta.
// TODO: Ultra-precise type annotations provided here are actually necessary for the crazy macro framework
// that services ToEnsugaredScalameta. We should see about getting rid of that and cutting these swaths of boilerplate.
trait ToP extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToScalametaSymbol(gsym: g.Symbol) {
    type pTermOrTypeName = p.Name{type ThisType >: p.Term.Name with p.Type.Name <: p.Name}
    type pTermOrTypeOrAnonymousName = p.Name{type ThisType >: p.Term.Name with p.Type.Name with p.Name.Anonymous <: p.Name}
    private def dumbcvt(in: g.Tree): p.Name = {
      if (gsym.isTerm) p.Term.Name(in.alias)
      else if (gsym.isType) p.Type.Name(in.alias)
      else unreachable
    }
    def precvt(pre: g.Type, in: g.Tree): pTermOrTypeName = dumbcvt(in).withDenot(pre, gsym).require[pTermOrTypeName]
    def rawcvt(in: g.Tree): pTermOrTypeName = dumbcvt(in).withDenot(gsym).require[pTermOrTypeName]
    def anoncvt(in: g.Tree): pTermOrTypeOrAnonymousName = (if (gsym.isAnonymous) p.Name.Anonymous() else dumbcvt(in)).withDenot(gsym).require[pTermOrTypeOrAnonymousName]
  }

  protected implicit class RichToScalametaTermSymbol(gsym: g.TermSymbol) {
    type pTermOrAnonymousName = papi.Term.Name with p.Name{type ThisType >: p.Term.Name with p.Name.Anonymous <: p.Name}
    def precvt(pre: g.Type, in: g.Tree): p.Term.Name = (gsym: g.Symbol).precvt(pre, in).require[p.Term.Name]
    def rawcvt(in: g.Tree, allowNoSymbol: Boolean = false): p.Term.Name = (gsym: g.Symbol).rawcvt(in).require[p.Term.Name]
    def anoncvt(in: g.ValDef): pTermOrAnonymousName = (gsym: g.Symbol).anoncvt(in).require[pTermOrAnonymousName]
  }

  protected implicit class RichToScalametaTypeSymbol(gsym: g.TypeSymbol) {
    type pTypeOrAnonymousName = papi.Type.Name with p.Name{type ThisType >: p.Type.Name with p.Name.Anonymous <: p.Name}
    def precvt(pre: g.Type, in: g.Tree): p.Type.Name = (gsym: g.Symbol).precvt(pre, in).require[p.Type.Name]
    def rawcvt(in: g.Tree): p.Type.Name = (gsym: g.Symbol).rawcvt(in).require[p.Type.Name]
    def anoncvt(in: g.TypeDef): pTypeOrAnonymousName = (gsym: g.Symbol).anoncvt(in).require[pTypeOrAnonymousName]
  }

  protected implicit class RichToScalametaConstant(gconst: g.Constant) {
    type pConst = p.Lit{type ThisType >: p.Lit.Null with p.Lit.Unit with p.Lit.Bool with p.Lit.Int with p.Lit.Long with p.Lit.Float with p.Lit.Double with p.Lit.String with p.Lit.Char <: p.Lit}
    def rawcvt: pConst = gconst.value match {
      case null => p.Lit.Null()
      case () => p.Lit.Unit()
      case v: Boolean => p.Lit.Bool(v)
      case v: Byte => ???
      case v: Short => ???
      case v: Int => p.Lit.Int(v)
      case v: Long => p.Lit.Long(v)
      case v: Float => p.Lit.Float(v)
      case v: Double => p.Lit.Double(v)
      case v: String => p.Lit.String(v)
      case v: Char => p.Lit.Char(v)
      case v: g.Type => unreachable
      case v: g.Symbol => unreachable
    }
  }
}
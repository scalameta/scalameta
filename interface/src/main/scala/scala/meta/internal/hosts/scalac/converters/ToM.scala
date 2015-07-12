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

// This module provides utilities shared between scala.reflect -> scala.meta converters
trait ToM extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichToScalametaSymbol(gsym: g.Symbol) {
    private def dumbcvt(in: g.Tree): m.Name = {
      if (gsym.isTerm) m.Term.Name(in.displayName)
      else if (gsym.isType) m.Type.Name(in.displayName)
      else unreachable(debug(gsym, gsym.flags, gsym.getClass, gsym.owner))
    }
    def precvt(pre: g.Type, in: g.Tree): m.Name = dumbcvt(in).withDenot(pre, gsym)
    def rawcvt(in: g.Tree): m.Name = dumbcvt(in).withDenot(gsym)
    def anoncvt(in: g.Tree): m.Name = (if (gsym.isAnonymous) m.Name.Anonymous() else dumbcvt(in)).withDenot(gsym)
  }

  protected implicit class RichToScalametaTermSymbol(gsym: g.TermSymbol) {
    def precvt(pre: g.Type, in: g.Tree): m.Term.Name = (gsym: g.Symbol).precvt(pre, in).require[m.Term.Name]
    def rawcvt(in: g.Tree, allowNoSymbol: Boolean = false): m.Term.Name = (gsym: g.Symbol).rawcvt(in).require[m.Term.Name]
    def anoncvt(in: g.ValDef): m.Name = (gsym: g.Symbol).anoncvt(in)
  }

  protected implicit class RichToScalametaTypeSymbol(gsym: g.TypeSymbol) {
    def precvt(pre: g.Type, in: g.Tree): m.Type.Name = (gsym: g.Symbol).precvt(pre, in).require[m.Type.Name]
    def rawcvt(in: g.Tree): m.Type.Name = (gsym: g.Symbol).rawcvt(in).require[m.Type.Name]
    def anoncvt(in: g.TypeDef): m.Name = (gsym: g.Symbol).anoncvt(in)
  }

  protected implicit class RichToScalametaConstant(gconst: g.Constant) {
    def rawcvt: m.Lit = gconst.value match {
      case null => m.Lit.Null()
      case () => m.Lit.Unit()
      case v: Boolean => m.Lit.Bool(v)
      case v: Byte => m.Lit.Byte(v)
      case v: Short => m.Lit.Short(v)
      case v: Int => m.Lit.Int(v)
      case v: Long => m.Lit.Long(v)
      case v: Float => m.Lit.Float(v)
      case v: Double => m.Lit.Double(v)
      case v: String => m.Lit.String(v)
      case v: Char => m.Lit.Char(v)
      case v: g.Type => unreachable(debug(gconst))
      case v: g.Symbol => unreachable(debug(gconst))
    }
  }
}
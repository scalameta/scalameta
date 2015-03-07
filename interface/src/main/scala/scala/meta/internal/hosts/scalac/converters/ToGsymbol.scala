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
import scala.meta.internal.{ast => m}

// This module exposes methods to convert from scala.meta members to scala.reflect symbols.
// To be more precise, it returns logical symbols instead of scala.reflect ones, but that's almost the same.
trait ToGsymbol extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected implicit class RichNameToGsymbol(mname: m.Name) {
    def toGsymbol: l.Symbol = {
      symbolTable.convert(mname.denot.symbol)
    }
  }

  protected implicit class RichMemberToGsymbol(mmember: m.Member) {
    def toGsymbol: l.Symbol = mmember.name.require[m.Name].toGsymbol
  }
}
package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.collections._
import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.collection.mutable
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{hygiene => h}

// This module explicitly lists caches that are used by the conversions.
// Some of them are here to improve performance, but some of them are necessary for uniqueness and/or correctness.
// TODO: make `hsymToNativeMmemberCache` private
trait Caches extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected lazy val symbolTable = new SymbolTable()
  protected lazy val lsymToMmemberCache = mutable.Map[(g.Type, l.Symbol), m.Member]()
  protected lazy val tpeCache = TwoWayCache[g.Type, m.Type.Arg]()
  lazy val hsymToNativeMmemberCache = mutable.Map[h.Symbol, m.Member]()
}

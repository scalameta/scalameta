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
trait Caches extends GlobalToolkit with MetaToolkit {
  self: Api =>

  protected lazy val symbolTable = new SymbolTable()
  protected lazy val lsymToMmemberCache = mutable.Map[(g.Type, l.Symbol), m.Member]()
  protected lazy val tpeCache = TwoWayCache[g.Type, m.Type.Arg]()
  protected lazy val hsymToNativeMmemberCache = mutable.Map[h.Symbol, m.Member]()

  def cacheAllMembers[T <: m.Tree](x: T): T = {
    def cache(x: m.Member): Unit = {
      val denot = x.name.require[m.Name].denot
      if (x.name.isBinder && !x.isInstanceOf[m.Ctor.Primary] && !x.name.isInstanceOf[m.Name.Anonymous]) {
        require(denot != h.Denotation.Zero && debug(x, x.show[Raw]))
        require(denot.symbol != h.Symbol.Zero && debug(x, x.show[Raw]))
        // TODO: it seems that we can't have this yet
        // require(!hsymToNativeMmemberCache.contains(denot.symbol) && debug(x, x.show[Raw]))
        hsymToNativeMmemberCache(denot.symbol) = x
      }
    }
    def loop(x: Any): Unit = x match {
      case x: m.Tree => cacheAllMembers(x)
      case x: List[_] => x.foreach(loop)
      case x: Some[_] => loop(x.get)
      case x => // do nothing
    }
    x match { case x: m.Member => cache(x); case _ => }
    x.productIterator.foreach(loop)
    x
  }
}

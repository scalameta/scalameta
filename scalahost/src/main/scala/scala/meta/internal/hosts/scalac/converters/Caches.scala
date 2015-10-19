package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.collections._
import org.scalameta.debug._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.collection.mutable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.hosts.scalac.reflect._
import scala.meta.internal.prettyprinters.Attributes

// This module explicitly lists caches that are used by the conversions.
// Some of them are here to improve performance, but some of them are necessary for uniqueness and/or correctness.
trait Caches extends ReflectToolkit with MetaToolkit {
  self: Api =>

  // NOTE: These maps are mutable, but every mapping there is immutable,
  // i.e. won't change regardless of the mutations happening in the underlying compiler.
  //
  // E.g. a signature of a symbol might evolve over time (as we load additional sources),
  // but the symbol itself isn't going to be recreated or something.
  // Also, the meaning of a type might change as well (as the signatures of the underlying symbols change),
  // but the type itself is just a combination of the underlying symbols, so it's going to remain there.
  //
  // Therefore, the maps below are called "caches".
  protected lazy val symbolTable = new SymbolTable() // TwoWayCache[s.Symbol, l.Symbol]
  protected lazy val tpeCache = TwoWayCache[g.Type, m.Type.Arg]()

  // NOTE: These maps are mutable, and all their mapping are mutable.
  // E.g. if we load new sources into the compiler, it is possible that they will take over
  // the previously existing symbols, so we'll need to update the maps.
  // Therefore, the maps below are called "indices".
  private lazy val _ssymToMmemberIndex = mutable.Map[s.Symbol, m.Member]()
  protected def ssymToMmemberIndex = { require(!indicesWriteOnly); _ssymToMmemberIndex }
  private lazy val _ldenotToMmemberIndex = mutable.Map[l.Denotation, m.Member]()
  protected def ldenotToMmemberIndex = { require(!indicesWriteOnly); _ldenotToMmemberIndex }

  // NOTE: Since indices are mutable, sometimes we want to ensure
  // that a given subsystem of scala.meta has exclusive access to them.
  // Otherwise, we could run into problems when we, say, resolve a reference during indexing
  // and the result of the resolution is an object that is reconstructed from a symbol,
  // not loaded directly from a source that hasn't been indexed yet.
  private var indicesWriteOnly = false
  protected def withWriteOnlyIndices[T](body: => T): T = {
    Debug.delayingSensitiveLogs {
      val old = indicesWriteOnly
      indicesWriteOnly = true
      try body
      finally indicesWriteOnly = old
    }
  }

  def indexOne[T <: m.Tree](x: T): T = {
    require(x.isTypechecked)
    x match {
      case x: m.Member =>
        if (x.name.isBinder && !x.isInstanceOf[m.Ctor.Primary] && !x.name.isInstanceOf[m.Name.Anonymous]) {
          x.name.require[m.Name].denot match {
            case s.Denotation.Single(_, ssym) if ssym != s.Symbol.Zero =>
              // TODO: it seems that we can't have this yet
              // require(!_ssymToMmemberIndex.contains(symbol) && debug(x, x.show[Attributes]))
              _ssymToMmemberIndex(ssym) = x
              _ldenotToMmemberIndex.retain((ldenot, _) => ldenot.sym != symbolTable.convert(ssym))
            case _ =>
              abort(debug(x, x.show[Attributes]))
          }
        }
      case _ =>
        x
    }
    x
  }

  def indexAll[T <: m.Tree](x: T): T = {
    require(x.isTypechecked)
    def loop(x: Any): Unit = x match {
      case x: m.Tree => indexAll(x)
      case x: Seq[_] => x.foreach(loop)
      case x: Some[_] => loop(x.get)
      case x => // do nothing
    }
    x match { case x: m.Member => indexOne(x); case _ => }
    x.productIterator.foreach(loop)
    x
  }
}

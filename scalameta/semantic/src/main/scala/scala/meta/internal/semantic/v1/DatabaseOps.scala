package scala.meta
package internal
package semantic
package v1

import scala.collection.mutable
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location
import scala.meta.semantic.v1.Symbol

trait DatabaseOps {
  private[meta] implicit class XtensionInternalDatabase(db1: Database) {
    def append(db2: Database): Database = {
      val symbols2 = mutable.Map[Location, Symbol]()
      symbols2 ++= db1.symbols
      val addrs = db2.symbols.keys.map(_.addr).toSet
      addrs.foreach(addr => symbols2.retain((k, _) => k.addr != addr))
      symbols2 ++= db2.symbols
      Database(symbols2.toMap)
    }
  }
}
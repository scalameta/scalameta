package scala.meta
package internal
package semantic
package v1

import scala.collection.mutable
import scala.meta.semantic.v1.CompilerMessage
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location
import scala.meta.semantic.v1.Symbol

trait DatabaseOps {
  private[meta] implicit class XtensionInternalDatabase(db1: Database) {
    def append(db2: Database): Database = {
      val names2 = mutable.Map[Location, Symbol]()
      names2 ++= db1.names
      val addrs = db2.names.keys.map(_.addr).toSet
      addrs.foreach(addr => names2.retain((k, _) => k.addr != addr))
      names2 ++= db2.names
      val messages = mutable.LinkedHashSet.empty[CompilerMessage]
      messages ++= db1.messages
      messages ++= db2.messages
      Database(names2.toMap, messages.toSeq)
    }
  }
}
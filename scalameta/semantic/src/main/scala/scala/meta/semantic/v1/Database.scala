package scala.meta
package semantic
package v1

import scala.compat.Platform.EOL
import scala.meta.internal.semantic.v1._
import scala.meta.prettyprinters._
import scala.meta.semantic.v1._

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the low-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

case class Database(
  symbols: Map[Location, Symbol]
  // TODO: Additional fields are to be discussed
  // https://github.com/scalameta/scalameta/issues/605
) {
  override def toString: String = {
    val buf = new StringBuilder
    val grouped = symbols.groupBy(_._1.addr)
    grouped.keys.toList.sortBy(_.syntax).foreach(addr => {
      buf ++= (addr + EOL)
      val content = addr.content
      grouped(addr).keys.toList.sortBy(_.start).foreach(k => {
        val snippet = content.substring(k.start, k.end)
        buf ++= (s"[${k.start}..${k.end}): $snippet => ${grouped(addr)(k).syntax}" + EOL)
      })
      buf ++= EOL
    })
    buf.toString.trim + EOL
  }
}

object Database extends DatabaseOps {
  def apply(): Database = {
    Database(Map[Location, Symbol]())
  }
}
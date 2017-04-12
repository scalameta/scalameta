package scala.meta
package semantic
package v1

import scala.compat.Platform.EOL
import scala.meta.internal.semantic.v1._

import org.scalameta.semantic.v1.{proto => p}
import scala.meta.internal.scalahost.v1.ProtoCodec._
import scala.meta.prettyprinters._
import scala.meta.semantic.v1._
import scala.util.Try

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the low-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

case class Database(
  names: Map[Location, Symbol],
  messages: Seq[CompilerMessage]
  // TODO: Additional fields are to be discussed
  // https://github.com/scalameta/scalameta/issues/605
) {
  override def toString: String = {
    val buf = new StringBuilder
    val groupedNames = names.groupBy(_._1.addr)
    val groupedMessages = messages.groupBy(_.location.addr).withDefaultValue(Nil)
    val addrs = groupedNames.keys.toList.sortBy(_.syntax)
    addrs.foreach(addr => {
      val messages = groupedMessages(addr).map(x => x.location -> x.syntax)
      val names = groupedNames(addr).keys.map(x => x -> groupedNames(addr)(x).syntax)
      val combined = (messages ++ names).sortBy(_._1.start)
      buf ++= (addr + EOL)
      val content = addr.content
      combined.foreach { case (Location(_, start, end), syntax) =>
        val snippet = content.substring(start, end)
        buf ++= (s"[$start..$end): $snippet => $syntax" + EOL)
      }
      buf ++= EOL
    })
    buf.toString.trim + EOL
  }
  def toBinary: Array[Byte] =
   this.toProto[p.Database].toByteArray
  def writeDatabaseToFile(file: File): Unit = {
    val fos = new FileOutputStream(file)
    try fos.write(toBinary)
    finally { fos.close() }
  }
}

object Database extends DatabaseOps {
  def apply(): Database = {
    Database(Map[Location, Symbol](), Nil)
  }
  def fromBinary(bytes: Array[Byte]): Try[Database] =
    Try(p.Database.parseFrom(bytes).toMeta[Database])
  def fromFile(file: File): Try[Database] =
    Try(p.Database.parseFrom(new FileInputStream(file)).toMeta[Database])
}
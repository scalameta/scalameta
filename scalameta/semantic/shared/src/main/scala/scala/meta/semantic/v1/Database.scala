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
  messages: Seq[CompilerMessage],
  denotations: Map[Symbol, Denotation]
  // TODO: Additional fields are to be discussed
  // https://github.com/scalameta/scalameta/issues/605
) {
  @deprecated("Use names instead", "1.8.0")
  def symbols: Map[Location, Symbol] = names

  override def toString: String = {
    val buf = new StringBuilder
    val groupedNames = names.groupBy(_._1.path)
    val groupedMessages = messages.groupBy(_.location.path).withDefaultValue(Nil)
    val paths = groupedNames.keys.toList.sortBy(_.absolute)
    paths.foreach(path => {
      val messages = groupedMessages(path).map(x => x.location -> x.syntax)
      val names = groupedNames(path).keys.map(x => x -> groupedNames(path)(x).syntax)
      val combined = (messages ++ names).sortBy(_._1.start)
      buf ++= (path + EOL)
      val content = path.slurp
      combined.foreach { case (Location(_, start, end), syntax) =>
        val snippet = content.substring(start, end)
        buf ++= (s"[$start..$end): $snippet => $syntax" + EOL)
      }
      buf ++= EOL
    })
    buf ++= ("Denotations:" + EOL)
    val symbols = denotations.keys.toList.sortBy(_.syntax)
    symbols.foreach(symbol => {
      val denot = denotations(symbol)
      buf ++= (s"$symbol => $denot" + EOL)
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
    Database(Map[Location, Symbol](), Nil, Map[Symbol, Denotation]())
  }
  def fromBinary(bytes: Array[Byte]): Try[Database] =
    Try(p.Database.parseFrom(bytes).toMeta[Database])
  def fromFile(file: File): Try[Database] =
    Try(p.Database.parseFrom(new FileInputStream(file)).toMeta[Database])
}
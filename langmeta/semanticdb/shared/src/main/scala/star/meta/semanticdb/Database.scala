package lang.meta
package semanticdb

import scala.compat.Platform.EOL
import lang.meta.inputs._
import lang.meta.io._
import lang.meta.internal.io.PathIO
import lang.meta.internal.semanticdb._
import lang.meta.internal.semanticdb.{vfs => v}
import lang.meta.internal.semanticdb.{schema => s}

final case class Database(entries: Seq[Attributes]) {
  lazy val names: Seq[ResolvedName] = entries.flatMap(_.names)
  lazy val messages: Seq[Message] = entries.flatMap(_.messages)
  lazy val symbols: Seq[ResolvedSymbol] = entries.flatMap(_.symbols)
  lazy val synthetics: Seq[Synthetic] = entries.flatMap(_.synthetics)

  def save(targetroot: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    this.toSchema(sourceroot).toVfs(targetroot).save()
  }

  def syntax: String = {
    val s_entries = entries.map { attrs =>
      val s_input = PathIO.toUnix(attrs.input.syntax)
      val separator = EOL + "-" * s_input.toString.length + EOL
      s_input + separator + attrs.syntax
    }
    s_entries.mkString(EOL + EOL)
  }

  def structure: String = {
    val s_entries = entries.map(_.structure).mkString(",")
    s"Database(List($s_entries))"
  }

  override def toString: String = syntax
}

object Database {
  def load(classpath: Classpath, sourcepath: Sourcepath): Database = {
    v.Database.load(classpath).toSchema.toDb(Some(sourcepath))
  }

  def load(classpath: Classpath): Database = {
    v.Database.load(classpath).toSchema.toDb(None)
  }

  def load(bytes: Array[Byte]): Database = {
    val sdb = s.Database.parseFrom(bytes)
    val mdb = sdb.toDb(None)
    mdb
  }
}

package star.meta
package semanticdb

import star.meta.inputs._
import star.meta.io._
import star.meta.internal.semanticdb._
import star.meta.internal.semanticdb.{vfs => v}
import star.meta.internal.semanticdb.{schema => s}

final case class Database(entries: Seq[Attributes]) {
  lazy val names: Map[Position, Symbol] = {
    val builder = Map.newBuilder[Position, Symbol]
    entries.foreach { entry =>
      entry.names.foreach(builder += _)
      entry.sugars.foreach { case (_, sugar) =>
        sugar.names.foreach(builder += _)
      }
    }
    builder.result()
  }
  lazy val messages: Seq[Message] = entries.flatMap(_.messages)
  lazy val denotations: Map[Symbol, Denotation] = entries.flatMap(_.denotations).toMap
  lazy val sugars: Map[Position, Sugar] = entries.flatMap(_.sugars).toMap

  def save(targetroot: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    this.toSchema(sourceroot).toVfs(targetroot).save()
  }

  def syntax: String = star.meta.internal.semanticdb.DatabaseSyntax(this)
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

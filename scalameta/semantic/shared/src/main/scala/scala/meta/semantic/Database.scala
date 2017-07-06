package scala.meta
package semantic

import org.scalameta.data._
import scala.meta.inputs._
import scala.meta.io._
import scala.meta.internal.semantic._
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}
import scala.meta.{semantic => m}

@data class Database(entries: Seq[Attributes]) extends Mirror {
  def database = this

  lazy val names: Map[Position, Symbol] = entries.flatMap(_.names).toMap
  lazy val messages: Seq[Message] = entries.flatMap(_.messages)
  lazy val denotations: Map[Symbol, Denotation] = entries.flatMap(_.denotations).toMap
  lazy val sugars: Map[Position, String] = entries.flatMap(_.sugars).toMap

  def save(targetroot: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    this.toSchema(sourceroot).save(targetroot)
  }

  def syntax: String = scala.meta.internal.semantic.DatabaseSyntax(this)
  def structure: String = {
    val s_entries = entries.map(_.structure).mkString(",")
    s"Database(List($s_entries))"
  }
  override def toString: String = syntax
}

object Database {
  def load(classpath: Classpath, sourcepath: Sourcepath): Database = {
    v.Database.load(classpath).toSchema.toMeta(Some(sourcepath))
  }
  def load(classpath: Classpath): Database = {
    v.Database.load(classpath).toSchema.toMeta(None)
  }
  def load(bytes: Array[Byte]): Database = {
    val sdb = s.Database.parseFrom(bytes)
    val mdb = sdb.toMeta(None)
    mdb
  }
}
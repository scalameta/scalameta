package scala.meta
package semantic

import org.scalameta.data._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.meta.inputs._
import scala.meta.io._
import scala.meta.internal.semantic._
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}
import scala.meta.{semantic => m}

@data class Database(entries: Seq[(Input, Attributes)]) extends Mirror {
  def database = this

  lazy val names: Map[Position, Symbol] = entries.flatMap(_._2.names).toMap
  lazy val messages: Seq[Message] = entries.flatMap(_._2.messages)
  lazy val denotations: Map[Symbol, Denotation] = entries.flatMap(_._2.denotations).toMap
  lazy val sugars: Map[Position, String] = entries.flatMap(_._2.sugars).toMap

  def save(classpath: Classpath, sourcepath: Sourcepath): Unit = {
    this.toSchema(sourcepath).toVfs(classpath).save()
  }

  def syntax: String = scala.meta.internal.semantic.DatabaseSyntax(this)
  def structure: String = {
    val s_entries = entries.map{ case (input, attrs) => s"${input.structure} -> ${attrs.structure}" }.mkString(", ")
    s"Database(Seq($s_entries))"
  }
  override def toString: String = syntax
}

object Database {
  def load(classpath: Classpath, sourcepath: Sourcepath): Database = {
    v.Database.load(classpath).toSchema.toMeta(sourcepath)
  }
}
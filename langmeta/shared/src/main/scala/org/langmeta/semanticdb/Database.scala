package org.langmeta
package semanticdb

import scala.compat.Platform.EOL
import org.langmeta.inputs._
import org.langmeta.io._
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb._
import org.langmeta.internal.semanticdb.{vfs => v}
import org.langmeta.internal.semanticdb.{schema => s}

final case class Database(files: Seq[SourceFile]) {
  lazy val names: Seq[ResolvedName] = files.flatMap(_.names)
  lazy val messages: Seq[Message] = files.flatMap(_.messages)
  lazy val symbols: Seq[ResolvedSymbol] = files.flatMap(_.symbols)
  lazy val synthetics: Seq[Synthetic] = files.flatMap(_.synthetics)

  def save(targetroot: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    this.toSchema(sourceroot).toVfs(targetroot).save()
  }

  def syntax: String = {
    val s_entries = files.map { attrs =>
      val s_input = PathIO.toUnix(attrs.input.syntax)
      val separator = EOL + "-" * s_input.toString.length + EOL
      s_input + separator + attrs.syntax
    }
    s_entries.mkString(EOL + EOL)
  }

  def structure: String = {
    val s_entries = files.map(_.structure).mkString(",")
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

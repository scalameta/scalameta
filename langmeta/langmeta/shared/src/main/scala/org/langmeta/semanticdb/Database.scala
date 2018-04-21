package org.langmeta.semanticdb

import scala.compat.Platform.EOL
import org.langmeta.io._
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb._
import org.langmeta.internal.semanticdb.{vfs => v}
import scala.meta.internal.{semanticdb3 => s}

@deprecated(DeprecationMessage, "3.8.0")
final case class Database(documents: Seq[Document]) {
  lazy val names: Seq[ResolvedName] = documents.flatMap(_.names)
  lazy val messages: Seq[Message] = documents.flatMap(_.messages)
  lazy val symbols: Seq[ResolvedSymbol] = documents.flatMap(_.symbols)
  lazy val synthetics: Seq[Synthetic] = documents.flatMap(_.synthetics)

  def save(targetroot: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    throw new UnsupportedOperationException()
  }
  def append(targetroot: AbsolutePath, sourceroot: AbsolutePath): Unit = {
    throw new UnsupportedOperationException()
  }

  def syntax: String = {
    val s_entries = documents.map { attrs =>
      val s_input = PathIO.toUnix(attrs.input.syntax)
      val separator = EOL + "-" * s_input.toString.length + EOL
      s_input + separator + attrs.syntax
    }
    s_entries.sorted.mkString(EOL + EOL)
  }

  def structure: String = {
    val s_entries = documents.map(_.structure).mkString(",")
    s"Database(List($s_entries))"
  }

  override def toString: String = syntax
}

@deprecated(DeprecationMessage, "3.8.0")
object Database {
  def load(classpath: Classpath, sourcepath: Sourcepath): Database = {
    throw new UnsupportedOperationException()
  }

  def load(classpath: Classpath): Database = {
    throw new UnsupportedOperationException()
  }

  def load(bytes: Array[Byte]): Database = {
    throw new UnsupportedOperationException()
  }
}

package org.langmeta.internal
package semanticdb
package vfs

import java.io.File
import java.io.FileOutputStream
import org.langmeta.io._
import org.langmeta.internal.semanticdb.{vfs => v}
import org.langmeta.internal.semanticdb.{schema => legacy}
import scala.meta.internal.{semanticdb3 => s}
import com.google.protobuf.InvalidProtocolBufferException

object Database {
  def load(classpath: Classpath): v.Database = {
    val fentries = classpath.deep.filter(e => v.SemanticdbPaths.isSemanticdb(e.name))
    val ventries = fentries.map(fentry => v.Entry.OnDisk(fentry))
    v.Database(ventries)
  }
}

final case class Database(entries: List[Entry]) {

  def toSchema3: s.TextDocuments = {
    val sentries = entries.flatMap { ventry =>
      val sdocs = s.TextDocuments.parseFrom(ventry.bytes)
      sdocs.mergeDiagnosticOnlyDocuments.documents
    }
    s.TextDocuments(sentries)
  }

  def toSchema: s.TextDocuments =
    try toSchema3
    catch {
      case _: InvalidProtocolBufferException if isLegacyPayload =>
        val sentries = entries.flatMap { ventry =>
          val db = legacy.Database.parseFrom(ventry.bytes)
          legacy.LegacySemanticdb.toTextDocuments(db).documents
        }
        s.TextDocuments(sentries)
    }

  private def isLegacyPayload: Boolean =
    (for {
      ventry <- entries.headOption
      document <- legacy.LegacyTextDocuments.parseFrom(ventry.bytes).documents
    } yield document.schema == legacy.LegacySchema.LEGACY).getOrElse(false)

  def save(append: Boolean): Unit = {
    entries.foreach(ventry => {
      val file = new File(ventry.uri)
      file.getParentFile.mkdirs()
      val fos = new FileOutputStream(file, append)
      try fos.write(ventry.bytes)
      finally fos.close()
    })
  }
}

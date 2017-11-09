package org.langmeta.internal
package semanticdb
package vfs

import java.io.File
import java.io.FileOutputStream
import org.langmeta.internal.semanticdb.schema.Document
import org.langmeta.io._
import org.langmeta.internal.semanticdb.{vfs => v}
import org.langmeta.internal.semanticdb.{schema => s}

object Database {
  def load(classpath: Classpath): v.Database = {
    val fentries = classpath.deep.filter(e => v.SemanticdbPaths.isSemanticdb(e.name))
    val ventries = fentries.map(fentry => v.Entry.OnDisk(fentry))
    v.Database(ventries)
  }
}

final case class Database(entries: List[Entry]) {
  def toSchema: s.Database = {
    val sentries = entries.flatMap { ventry =>
      val sdb = s.Database.parseFrom(ventry.bytes)
      // returns true if this document contains only messages and nothing else.
      // deprecation messages are reported in refchecks and get persisted
      // as standalone documents that need to be merged with their typer-phase
      // document during loading. It seems there's no way to merge the documents
      // during compilation without introducing a lot of memory pressure.
      def isOnlyMessages(sdocument: s.Document): Boolean =
        sdocument.messages.nonEmpty &&
          sdocument.contents.isEmpty &&
          sdocument.names.isEmpty &&
          sdocument.synthetics.isEmpty &&
          sdocument.symbols.isEmpty
      val sdocsMerged: Seq[Document] = {
        if (sdb.documents.length <= 1) {
          // NOTE(olafur) the most common case is that there is only a single database
          // per document so we short-circuit here if that's the case.
          sdb.documents
        } else {
          sdb.documents match {
            case Seq(doc, messages)
                if doc.filename == messages.filename &&
                  isOnlyMessages(messages) =>
              val x = doc.addMessages(messages.messages: _*)
              x :: Nil
            case _ => sdb.documents
          }
        }
      }
      sdocsMerged
    }
    s.Database(sentries)
  }

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

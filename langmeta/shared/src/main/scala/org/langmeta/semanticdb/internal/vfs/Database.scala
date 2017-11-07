package org.langmeta.internal
package semanticdb
package vfs

import java.io.File
import java.io.FileOutputStream
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
      sdb.documents
    }
    s.Database(sentries)
  }

  def save(): Unit = persist(append = false)

  def append(): Unit = persist(append = true)

  private def persist(append: Boolean): Unit = {
    entries.foreach(ventry => {
      val file = new File(ventry.uri)
      val fos = new FileOutputStream(file, append)
      try fos.write(ventry.bytes)
      finally fos.close()
    })
  }
}

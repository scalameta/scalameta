package org.langmeta.internal
package semanticdb
package vfs

import java.io.File
import java.io.FileOutputStream
import org.langmeta.io._
import org.langmeta.internal.semanticdb.{vfs => v}
import scala.meta.internal.{semanticdb3 => s}

object Database {
  def load(classpath: Classpath): v.Database = {
    val fentries = classpath.deep.filter(e => v.SemanticdbPaths.isSemanticdb(e.name))
    val ventries = fentries.map(fentry => v.Entry.OnDisk(fentry))
    v.Database(ventries)
  }
}

final case class Database(entries: List[Entry]) {
  @deprecated(DeprecationMessage, "3.8.0")
  def toSchema: s.TextDocuments = {
    throw new UnsupportedOperationException()
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

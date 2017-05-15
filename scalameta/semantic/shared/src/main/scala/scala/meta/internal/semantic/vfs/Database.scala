package scala.meta.internal.semantic.vfs

import java.io._
import java.net._
import org.scalameta.adt._
import org.scalameta.data._
import org.scalameta.invariants._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.io._
import scala.meta.internal.io.InputStreamIO
import scala.meta.internal.io.PathIO.fileSeparator
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}

object Database {
  def load(classpath: Classpath): v.Database = {
    val fentries = classpath.deep.filter(e => v.Paths.isSemanticdb(e.name))
    val ventries = fentries.map(fentry => v.Entry.OnDisk(fentry))
    v.Database(ventries)
  }
}

@data class Database(entries: Seq[Entry]) {
  def toSchema: s.Database = {
    val sentries = entries.map(ventry => {
      val scalaName = v.Paths.semanticdbToScala(ventry.name)
      scalaName -> s.Attributes.parseFrom(ventry.bytes)
    })
    s.Database(sentries)
  }

  def save(): Unit = {
    entries.foreach(ventry => {
      val file = new File(ventry.uri)
      file.getParentFile.mkdirs()
      val fos = new FileOutputStream(file)
      try fos.write(ventry.bytes)
      finally fos.close()
    })
  }
}

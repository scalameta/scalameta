package scala.meta.internal
package scalahost
package v1

import java.io._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location

class OfflineMirror(classpath: String, sourcepath: String)
    extends Mirror
    with DialectOps
    with MirrorOps
    with PathOps {

  if (classpath == "") sys.error("Classpath must be non-empty")
  if (sourcepath == "") sys.error("Sourcepath must be non-empty")

  override def toString: String = {
    s"online mirror for $classpath and $sourcepath"
  }

  lazy val sources: Seq[Source] = {
    sourcepath.files.map(uri => dialect(new File(uri)).parse[Source].get)
  }

  lazy val database: Database = {
    val databaseFiles = classpath.paths
      .flatMap(uri => {
        val subfiles = new File(uri).listFiles
        if (subfiles != null) subfiles.filter(_.getName == "semanticdb").toList
        else Nil
      })
      .sortBy(_.getName)
    val databases = databaseFiles.map(Database.readFile)
    databases.foldLeft(Database(Map()))(_ append _)
  }
}

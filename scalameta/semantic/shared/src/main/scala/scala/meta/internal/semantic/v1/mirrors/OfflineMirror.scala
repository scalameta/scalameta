package scala.meta
package internal
package semantic
package v1
package mirrors

import java.io._
import org.scalameta.data._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.parsers._
import scala.meta.semantic.v1._

@data class OfflineMirror(classpath: String, sourcepath: String) extends CommonMirror with PathOps {
  private def failEmpty(what: String) =
    sys.error(
      s"$what must be non-empty. " +
        s"This may indicate that Mirror is badly configured. " +
        s"If you use sbt-scalahost, make sure your project defines " +
        s"`dependsOn(<projectname> % Scalameta)` for at least one <projectname>.")
  if (classpath == null || classpath == "") failEmpty("classpath")
  if (sourcepath == null || sourcepath == "") failEmpty("sourcepath")

  lazy val sources: Seq[Source] = {
    sourcepath
      .split(File.pathSeparator)
      .flatMap(_.files.map(file => dialect(file).parse[Source].get))
      .to[Seq]
  }

  lazy val database: Database = {
    val databaseFiles = classpath.paths
      .flatMap(uri => {
        val subfiles = new File(uri).listFiles
        if (subfiles != null) subfiles.filter(_.getName == "semanticdb").toList
        else Nil
      })
      .sortBy(_.getName)
    val databases = databaseFiles.map(f => Database.fromFile(f).get)
    databases.foldLeft(Database())(_ append _)
  }
}

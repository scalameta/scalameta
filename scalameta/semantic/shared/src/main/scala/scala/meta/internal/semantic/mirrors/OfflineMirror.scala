package scala.meta
package internal
package semantic
package mirrors

import java.io._
import org.scalameta.data._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.io.PlatformIO
import scala.meta.parsers._
import scala.meta.semantic._

@data class OfflineMirror(classpath: String, sourcepath: String) extends CommonMirror {
  private def failEmpty(what: String) =
    sys.error(
      s"$what must be non-empty. " +
        s"This may indicate that Mirror is badly configured. " +
        s"If you use sbt-scalahost, make sure your project defines " +
        s"`dependsOn(<projectname> % Scalameta)` for at least one <projectname>.")
  if (classpath == null || classpath == "") failEmpty("classpath")
  if (sourcepath == null || sourcepath == "") failEmpty("sourcepath")

  lazy val dialect: Dialect = {
    // TODO: This is only going to work well if we embed file contents.
    // The corresponding commit should land pretty soon.
    ???
  }

  lazy val sources: Seq[Source] = {
    // TODO: This is only going to work well if we embed file contents.
    // The corresponding commit should land pretty soon.
    ???
  }

  lazy val database: Database = {
    Database.fromClasspath(classpath)
  }
}

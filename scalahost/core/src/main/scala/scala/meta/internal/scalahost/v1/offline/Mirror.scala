package scala.meta.internal
package scalahost
package v1
package offline

import java.io._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.tools.cmd.CommandLineParser
import scala.tools.nsc.{Global, CompilerCommand, Settings}
import scala.tools.nsc.reporters.StoreReporter
import scala.meta._
import scala.meta.semantic.v1.{Mirror => MirrorApi}
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location
import scala.meta.internal.scalahost.v1.{Mirror => BaseMirror}
import scala.meta.internal.scalahost.v1.online.{Mirror => OnlineMirror}

class Mirror(classpath: String, sourcepath: String)
    extends MirrorApi
    with BaseMirror
    with PathOps {

  private def fail(what: String) =
    sys.error(
      s"$what must be non-empty. " +
        s"This may indicate that Mirror is badly configured. " +
        s"If you use sbt-scalahost, make sure your project defines " +
        s"`dependsOn(<projectname> % Scalameta)` for at least one <projectname>.")
  if (classpath == null || classpath == "") fail("classpath")
  if (sourcepath == null || sourcepath == "") fail("sourcepath")

  override def toString: String = {
    s"online mirror for $classpath and $sourcepath"
  }

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

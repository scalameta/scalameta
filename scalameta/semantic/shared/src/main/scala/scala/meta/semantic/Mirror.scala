package scala.meta
package semantic

import scala.meta.io._

trait Mirror {
  def database: Database
}

object Mirror {
  def apply(classpath: Classpath, sourceroot: AbsolutePath): Mirror = {
    Database.load(classpath, sourceroot)
  }

  def apply(): Mirror = {
    def failEmpty(what: String) =
      sys.error(
        s"$what must be non-empty. " +
          s"This indicates that your environment is misconfigured. " +
          s"If you use sbt-scalahost, make sure your project defines " +
          s"`dependsOn(<projectname> % Scalameta)` for at least one <projectname>.")
    val classpath = {
      val classpath = sys.props("scalameta.classpath")
      if (classpath == null || classpath == "") failEmpty("-Dscalameta.classpath")
      classpath
    }
    val sourceroot = {
      val sourcepath = sys.props("scalameta.sourceroot")
      if (sourcepath == null || sourcepath == "") failEmpty("-Dscalameta.sourceroot")
      sourcepath
    }
    apply(Classpath(classpath), AbsolutePath(sourceroot))
  }
}
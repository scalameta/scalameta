package scala.meta.tests.metacp

import scala.meta.io.Classpath

case class Library(name: String, classpath: () => Classpath)
object Library {

  /** The scala-library of the latest 2.13 row, which built the classes the suites read. */
  val scalaLibrary: Library =
    Library("scala-library", () => Classpath(sys.props("sbt.paths.scala-library.jars")))

}

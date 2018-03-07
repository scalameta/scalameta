package scala.meta.tests.cli

import scala.meta._
import scala.meta.testkit.DiffAssertions
import org.scalatest.FunSuite

abstract class BaseCliSuite extends FunSuite with DiffAssertions  {
  val scalaLibraryJar = sys.props("sbt.paths.scalalibrary.classes")
  if (scalaLibraryJar == null) sys.error("sbt.paths.scalalibrary.classes not set. broken build?")
  val scalaLibraryClasspath = Classpath(AbsolutePath(scalaLibraryJar))
}

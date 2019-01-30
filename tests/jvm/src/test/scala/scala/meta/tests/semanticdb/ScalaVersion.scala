package scala.meta.tests.semanticdb

import scala.util.Properties

object ScalaVersion {
  // both the compiler and stdlib are different between Scala versions.
  // For the sake of simplicity, we only run the expect test against the
  // output of 2.12. It's possible to add another expect file for 2.11
  // later down the road if that turns out to be useful.
  def is212: Boolean = {
    Properties.versionNumberString.startsWith("2.12")
  }

  def doIf212[T](what: String)(thunk: => T): Unit = {
    if (is212) {
      thunk
    } else {
      println(s"Skipping $what because scalaVersion is ${Properties.versionNumberString}")
    }
  }

}

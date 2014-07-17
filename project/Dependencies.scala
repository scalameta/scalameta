import sbt._

object Dependencies {
  import Settings.metaVersion

  def reflect(sv: String) = "org.scala-lang" % "scala-reflect" % sv
  def compiler(sv: String) = "org.scala-lang" % "scala-compiler" % sv

  def meta = "org.scalameta" %% "scalameta" % "0.1.0-SNAPSHOT"

  lazy val paradise ="org.scalamacros" % "paradise" % "2.1.0-M1" cross CrossVersion.full

  lazy val scalaMetaFoundation = "org.scalameta" %% "scalameta-foundation" % metaVersion

  lazy val interpreter = "org.scalameta" %% "interpreter" % metaVersion

  lazy val scalatest = "org.scalatest" %% "scalatest" % "2.1.3" % "test"
  lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
}
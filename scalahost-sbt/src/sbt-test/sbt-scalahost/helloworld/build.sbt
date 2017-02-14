lazy val root = project
  .in(file("."))
  .aggregate(
    p1,
    p2,
    p3
  )

lazy val p1 = project.settings(scalaVersion := "2.10.5")
lazy val p2 = project.settings(scalaVersion := "2.11.8")
lazy val p3 = project.settings(scalaVersion := "2.12.1")

TaskKey[Unit]("check") := {
  val contents = IO.read(file("semanticdb"))
  assert(!contents.contains("p1"))
  // TODO(olafur) enable once https://github.com/scalameta/scalameta/pull/686 is merged.
//  assert(contents.contains("p2"))
//  assert(contents.contains("p3"))
}

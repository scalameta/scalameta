// This project should only test cross-versions. For more intricate tests use "semantic-example"
lazy val root = project
  .in(file("."))
  .aggregate(
    p1,
    p2,
    p3
  )

lazy val p1 = project.settings(scalaVersion := "2.10.5")
lazy val p2 = project.settings(scalaVersion := "2.11.11")
lazy val p3 = project.settings(scalaVersion := "2.12.2")

scalaVersion in ThisBuild := "2.11.8"
lazy val library1 = project
lazy val library2 = project
lazy val app = project.dependsOn(
  library1 % Scalahost,
  library2 % Scalahost
)

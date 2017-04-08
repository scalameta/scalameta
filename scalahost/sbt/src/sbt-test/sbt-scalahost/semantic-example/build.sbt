scalaVersion in ThisBuild := "2.11.10"
scalacOptions in ThisBuild += "-deprecation"
scalacOptions in ThisBuild += "-Ywarn-unused-import"
lazy val library1 = project
lazy val library2 = project
lazy val app = project.dependsOn(
  library1 % Scalameta,
  library2 % Scalameta
)

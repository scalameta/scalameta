scalaVersion in ThisBuild := "2.11.11"
scalacOptions in ThisBuild += "-deprecation"
scalacOptions in ThisBuild += "-Ywarn-unused-import"
// resolving scalameta:-SNAPSHOT is unbearably slow without this setting.
updateOptions in ThisBuild := updateOptions.value.withLatestSnapshots(false)
lazy val library1 = project
lazy val library2 = project
lazy val app = project
  .settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1",
    fork.in(Test, test) := true,
    fork.in(Test, testOnly) := true
  )
  .dependsOn(
    library1 % Scalameta,
    library2 % Scalameta
  )

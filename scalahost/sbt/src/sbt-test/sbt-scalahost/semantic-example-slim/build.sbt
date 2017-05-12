scalaVersion in ThisBuild := "2.11.11"
scalacOptions in ThisBuild += "-deprecation"
scalacOptions in ThisBuild += "-Ywarn-unused-import"
compile in Compile <<= (compile in Compile).dependsOn(Def.task {
  sys.props("scalameta.semanticdb") = "slim"
})
lazy val library1 = project
lazy val library2 = project
lazy val app = project.dependsOn(
  library1 % Scalameta,
  library2 % Scalameta
)

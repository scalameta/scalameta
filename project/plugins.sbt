// exclude is a workaround for https://github.com/sbt/sbt-assembly/issues/236#issuecomment-294452474
addSbtPlugin(
  "com.eed3si9n" % "sbt-assembly" % "0.14.10" exclude ("org.apache.maven", "maven-plugin-api")
)

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.27")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.31")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin-shaded" % "0.10.3"

libraryDependencies += "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.3")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.6.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.1")

addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")

addSbtPlugin("org.scalameta" % "sbt-munit" % "0.7.5")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.1.5")

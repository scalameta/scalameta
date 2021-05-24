addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.4")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.3"

libraryDependencies += "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.7")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.8.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.0.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.5.1")

addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")

addSbtPlugin("org.scalameta" % "sbt-munit" % "0.7.25")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.20")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.0")

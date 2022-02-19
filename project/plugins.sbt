addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.1.0")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.3")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.4"

libraryDependencies += "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.10")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.0.1")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.1.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.6.0")

addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")

addSbtPlugin("org.scalameta" % "sbt-munit" % "0.7.29")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.24")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.2")

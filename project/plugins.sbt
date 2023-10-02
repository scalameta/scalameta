resolvers ++= Resolver.sonatypeOssRepos("snapshots")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.3")

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.6")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.13"

libraryDependencies += "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.3")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")

addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.12.0")

addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools" % "1.1.1")

addSbtPlugin("org.scalameta" % "sbt-munit" % "0.7.29")

addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.7")

addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.14")

addSbtPlugin("io.chrisdavenport" %% "sbt-npm-package" % "0.1.2")

addSbtPlugin("io.get-coursier" % "sbt-shading" % "2.1.3")

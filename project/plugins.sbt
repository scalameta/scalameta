// scalafmt: { maxColumn = 100 }

resolvers += Resolver.sonatypeCentralSnapshots

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.4.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.1")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.12.0")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.1.0-RC2")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.6")

addSbtPlugin("io.get-coursier" % "sbt-shading" % "2.1.8")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.9.2")
addSbtPlugin("org.scalameta" % "sbt-munit" % "1.3.6")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

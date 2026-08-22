// scalafmt: { maxColumn = 100 }

val crossProjectV = "1.3.2"

resolvers += Resolver.sonatypeCentralSnapshots

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.4.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.6.1")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.12.0")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.8")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.6")

addSbtPlugin("io.get-coursier" % "sbt-shading" % "2.1.7")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % crossProjectV)
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % crossProjectV)

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.12")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.9.1")
addSbtPlugin("org.scalameta" % "sbt-munit" % "1.3.5")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.8")

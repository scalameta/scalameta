addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.16")

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")

addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.3.3")

// addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-M15") // see "Updating"

addSbtPlugin("com.lihaoyi" % "scalatex-sbt-plugin" % "0.3.7")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.15")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

addSbtPlugin("com.eed3si9n" % "sbt-doge" % "0.1.5")

addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.6" exclude ("com.trueaccord.scalapb", "protoc-bridge_2.10"))
libraryDependencies += "com.trueaccord.scalapb" %% "compilerplugin-shaded" % "0.6.0-pre2"

libraryDependencies += "org.scala-sbt" % "scripted-plugin" % sbtVersion.value

addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.6.0")

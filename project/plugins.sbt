// scalafmt: { maxColumn = 100, align.preset = more, align.allowOverflow = true }

val crossProjectV = "1.3.2"

resolvers ++= Resolver.sonatypeOssRepos("snapshots")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin"  % "0.11.17"
libraryDependencies += "org.scala-sbt"        %% "scripted-plugin" % sbtVersion.value

addSbtPlugin("com.eed3si9n" % "sbt-assembly"  % "2.3.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.13.1")

addSbtPlugin("com.github.sbt" % "sbt-unidoc"     % "0.5.0")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.11.1")

addSbtPlugin("com.thesamet" % "sbt-protoc"      % "1.0.7")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.4")

addSbtPlugin("io.chrisdavenport" %% "sbt-npm-package" % "0.2.0")
addSbtPlugin("io.get-coursier"    % "sbt-shading"     % "2.1.5")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % crossProjectV)
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % crossProjectV)

addSbtPlugin("org.scala-debugger" % "sbt-jdi-tools"    % "1.1.1")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"      % "1.18.2")
addSbtPlugin("org.scala-native"   % "sbt-scala-native" % "0.5.6")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"         % "2.7.1")
addSbtPlugin("org.scalameta"      % "sbt-munit"        % "1.1.0")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")

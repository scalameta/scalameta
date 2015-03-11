import sbt._
import Keys._

object ScalaHostBuild extends Build {
  import Dependencies._
  import Settings._

  lazy val commonDependencies = Seq(
    libraryDependencies <++= (scalaVersion)(sv => Seq(
      reflect(sv) % "provided",
      compiler(sv) % "provided",
      meta
    )),
    addCompilerPlugin(paradise)
  )

  lazy val root = Project(
    id = "root",
    base = file("root"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      dontPackage,
      usePlugin(plugin),
      replIntegration
    )
  ) aggregate (plugin, tests) dependsOn (plugin, foundation, interface)

  lazy val foundation = Project(
    id   = "scalahost-foundation",
    base = file("foundation"),
    settings = publishableSettings ++ commonDependencies ++ Seq(
      libraryDependencies += metafoundation,
      dontPackage
    )
  )

  lazy val interface = Project(
    id   = "scalahost-interface",
    base = file("interface"),
    settings = publishableSettings ++ commonDependencies ++ Seq(
      dontPackage
    )
  ) dependsOn (foundation)

  lazy val plugin = Project(
    id   = "scalahost",
    base = file("plugin"),
    settings = publishableSettings ++ commonDependencies ++ mergeDependencies ++ Seq(
      libraryDependencies += interpreter
    )
  ) dependsOn (foundation % "optional", interface % "optional") // not really optional, used for fatjar

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      usePlugin(plugin)
    )
  ) dependsOn(plugin)

  lazy val tests = Project(
    id   = "tests",
    base = file("tests"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      usePlugin(plugin),
      libraryDependencies ++= Seq(scalatest, scalacheck),
      dontPackage
    ) ++ exposeClasspaths("tests")
  ) dependsOn (plugin, foundation, interface)
}

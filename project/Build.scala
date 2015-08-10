import sbt._
import Keys._

object ScalahostBuild extends Build {
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
      usePlugin(scalahost),
      replIntegration
    )
  ) aggregate (scalahost, tests)

  lazy val scalahost = Project(
    id   = "scalahost",
    base = file("scalahost"),
    settings = publishableSettings ++ commonDependencies ++ Seq(
      libraryDependencies += ivy
    ) ++ mergeDependencies
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      usePlugin(scalahost)
    )
  ) dependsOn(scalahost)

  lazy val tests = Project(
    id   = "tests",
    base = file("tests"),
    settings = sharedSettings ++ commonDependencies ++ Seq(
      libraryDependencies ++= Seq(scalatest, scalacheck),
      dontPackage
    ) ++ exposeClasspaths("tests")
  ) dependsOn (scalahost)
}

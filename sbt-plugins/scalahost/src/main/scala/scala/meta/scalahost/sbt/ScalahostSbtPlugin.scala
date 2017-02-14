package scala.meta.scalahost.sbt

import sbt._
import Keys._
import org.scalameta.BuildInfo
import sbt.plugins.JvmPlugin

object ScalahostSbtPlugin extends AutoPlugin {
  override def requires = JvmPlugin
  override def trigger: PluginTrigger = AllRequirements
  // Defaults to builds version.value, but with possibility to override.
  val scalahostVersion: String = sys.props.getOrElse("scalahost.version", BuildInfo.version)
  // If set to output of scalahost/package, uses -Xplugin:/path/to/scalahost.jar
  // instead of addCompilerPlugin. When testing scalahost locally, this provides a
  // much faster iteration speed compared to publishLocal.
  // NOTE. Only possible with one scalaVersion, not cross-build projects.
  val scalahostPackageJar: Option[String] = sys.props.get("scalahost.packagejar")
  override def projectSettings = Seq(
    commands += Command.command("dumpsemanticdb")(s => "test:compile" :: s),
    resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
    scalacOptions ++= {
      if (scalaVersion.value.startsWith("2.10")) Nil
      else {
        scalahostPackageJar.map(jar => s"Xplugin:$jar").toList ++ List(
          "-Yrangepos",
          "-Xplugin-require:scalahost"
        )
      }
    },
    libraryDependencies ++= {
      if (scalaVersion.value.startsWith("2.10")) Nil
      else if (scalahostPackageJar.isDefined) Nil
      else {
        List(
          compilerPlugin(
            ("org.scalameta" %% "scalahost" % scalahostVersion).cross(CrossVersion.full))
        )
      }
    }
  )
}

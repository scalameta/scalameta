package scala.meta.scalahost.sbt

import sbt._
import Keys.{version => _, _}
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
  // Scalahost only supports the latest patch version of scala minor version, for example
  // only 2.11.8 and not 2.11.7. If a build is using 2.11.7, we upgrade the dependency to
  // scalahost 2.11.8. See https://github.com/scalameta/scalameta/issues/681
  private object SupportedScalaVersion {
    private val MajorMinor = "(\\d+\\.\\d+)..*".r
    def unapply(arg: String): Option[String] = arg match {
      case MajorMinor(v) =>
        BuildInfo.supportedScalaVersions.find(_.startsWith(v))
    }
  }
  private val MetaConfig = config("scalameta").hide

  override def projectSettings = Seq(
    ivyConfigurations += MetaConfig,
    resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
    scalacOptions ++= {
      scalaVersion.value match {
        case SupportedScalaVersion(_) =>
          scalahostPackageJar.map(jar => s"Xplugin:$jar").toList ++ List(
            "-Yrangepos",
            "-Xplugin-require:scalahost"
          )
        case _ => Nil
      }
    },
    libraryDependencies ++= {
      scalaVersion.value match {
        case _ if scalahostPackageJar.isDefined => Nil
        case SupportedScalaVersion(version) =>
          List(
            compilerPlugin("org.scalameta" % s"scalahost_$version" % scalahostVersion)
          )
        case _ => Nil
      }
    }
  )
}

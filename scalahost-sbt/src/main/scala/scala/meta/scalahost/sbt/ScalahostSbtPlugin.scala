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
        case SupportedScalaVersion(version) =>
          val jarRegex = s".*scalahost_$version(-$scalahostVersion)?.jar$$"
          val allJars = update.value.filter(configurationFilter(MetaConfig.name)).allFiles
          val scalahostJar = allJars
            .collectFirst {
              case file if file.getAbsolutePath.matches(jarRegex) => file.getAbsolutePath
            }
            .getOrElse {
              throw new IllegalStateException(
                s"""Unable to find scalahost compiler plugin jar.
                   |Please report the output below at https://github.com/scalameta/scalameta/issues
                   |Scala version: ${scalaVersion.value}
                   |Cross version: ${crossScalaVersions.value}
                   |Jar regex: $jarRegex
                   |All jars: $allJars
                   |""".stripMargin
              )
            }
          List(
            s"-Xplugin:${scalahostJar}",
            "-Yrangepos",
            "-Xplugin-require:scalahost"
          )
        case _ => Nil
      }
    },
    libraryDependencies ++= {
      scalaVersion.value match {
        case SupportedScalaVersion(version) =>
          List(
            "org.scalameta" % s"scalahost_$version" % scalahostVersion % MetaConfig
          )
        case _ => Nil
      }
    }
  )
}

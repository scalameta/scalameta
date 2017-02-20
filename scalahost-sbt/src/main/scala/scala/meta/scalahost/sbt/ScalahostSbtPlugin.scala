package scala.meta.scalahost.sbt

import sbt._
import Keys.{version => _, _}
import org.scalameta.BuildInfo
import sbt.ScopeFilter.ScopeFilter
import sbt.plugins.JvmPlugin

object ScalahostSbtPlugin extends AutoPlugin {
  object autoImport {
    // use dependsOn(foo % Scalahost) to automatically include the semantic database
    // of foo in the scala.meta.Mirror() constructor.
    val Scalahost: Configuration = config("scalahost")
  }
  import autoImport._
  override def requires = JvmPlugin
  override def trigger: PluginTrigger = AllRequirements
  override def projectSettings: Seq[Def.Setting[_]] = scalahostAllSettings

  lazy val scalahostAllSettings =
    scalahostBaseSettings ++
      scalahostInjectCompilerPluginSettings ++
      scalahostHasMirrorSettings

  lazy val scalahostBaseSettings: Seq[Def.Setting[_]] = Def.settings(
    ivyConfigurations += Scalahost,
    resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")
  )

  lazy val scalahostInjectCompilerPluginSettings: Seq[Def.Setting[_]] = Def.settings(
    libraryDependencies ++= {
      scalaVersion.value match {
        case SupportedScalaVersion(version) =>
          List(
            "org.scalameta" % s"scalahost_$version" % scalahostVersion % Scalahost
          )
        case _ => Nil
      }
    },
    // sets -Xplugin:/scalahost.jar and other necessary compiler options.
    scalacOptions ++= {
      scalaVersion.value match {
        case SupportedScalaVersion(version) =>
          val jarRegex = s".*scalahost_$version(-$scalahostVersion)?.jar$$"
          val allJars = update.value.filter(configurationFilter(Scalahost.name)).allFiles
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
            s"-Xplugin:$scalahostJar",
            "-Yrangepos",
            "-Xplugin-require:scalahost"
          )
        case _ => Nil
      }
    }
  )

  lazy val scalahostHasMirrorSettings: Seq[Def.Setting[_]] = Def.settings(
    // Automatically set scalahostAggregate to projects listed in dependsOn(x % Scalahost)
    scalahostAggregate := thisProject.value.dependencies.collect {
      case x if x.configuration.exists(_ == Scalahost.name) => x.project
    },
    javaOptions ++= {
      if (scalahostAggregate.value.isEmpty) Nil
      else {
        val sourcepath =
          scalahostSourcepath.value
            .map(_.getAbsolutePath)
            .mkString(",")
        val classpath =
          scalahostClasspath.value
            .flatMap(_.files.map(_.getAbsolutePath))
            .mkString(java.io.File.pathSeparator)
        List(
          s"-Dscalameta.mirror.sourcepath=$sourcepath",
          s"-Dscalameta.mirror.classpath=$classpath"
        )
      }
    },
    // fork := true is required to pass along -Dscalameta.mirror.{classpath,sourcepath}
    fork in run := {
      if (scalahostAggregate.value.isEmpty) fork.in(run).value
      else true
    },
    // automatically depend on scalahost if this project dependsOn(otherProject % Scalahost)
    libraryDependencies ++= {
      if (scalahostAggregate.value.isEmpty) Nil
      else
        List(
          ("org.scalameta" % "scalahost" % scalahostVersion).cross(CrossVersion.full)
        )
    }
  )

  // Scalahost only supports the latest patch version of scala minor version, for example
  // 2.11.8 and not 2.11.7. If a build is using 2.11.7, we upgrade the dependency to
  // scalahost 2.11.8. See https://github.com/scalameta/scalameta/issues/681
  private object SupportedScalaVersion {
    private val MajorMinor = "(\\d+\\.\\d+)..*".r
    def unapply(arg: String): Option[String] = arg match {
      case MajorMinor(version) =>
        BuildInfo.supportedScalaVersions.find(_.startsWith(version))
      case _ => None
    }
  }
  // Defaults to version.value of in scala.meta's build.sbt
  private val scalahostVersion: String =
    sys.props.getOrElse("scalahost.version", BuildInfo.version)
  private val scalahostAggregate: SettingKey[Seq[ProjectRef]] =
    settingKey[Seq[ProjectRef]](
      "Projects to analyze with scala.meta, automatically set by dependsOn(foo % Scalahost).")
  private def scalahostAggregateFilter: Def.Initialize[ScopeFilter] = Def.setting {
    ScopeFilter(inProjects(scalahostAggregate.value.map(x => LocalProject(x.project)): _*),
                inConfigurations(Compile, Test, IntegrationTest))
  }
  private val scalahostSourcepath: Def.Initialize[Seq[File]] =
    Def.settingDyn(sourceDirectory.all(scalahostAggregateFilter.value))
  private val scalahostClasspath: Def.Initialize[Task[Seq[Classpath]]] =
    Def.taskDyn(fullClasspath.all(scalahostAggregateFilter.value))
}

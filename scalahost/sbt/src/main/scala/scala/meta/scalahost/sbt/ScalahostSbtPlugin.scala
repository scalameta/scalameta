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
    val Scalameta: Configuration = config("scalameta")
    val scalametaDependencies: SettingKey[Seq[ProjectRef]] =
      settingKey[Seq[ProjectRef]](
        "Projects to analyze with scala.meta, automatically set by dependsOn(foo % Scalahost).")
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
    ivyConfigurations += Scalameta,
    resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")
  )

  lazy val scalahostInjectCompilerPluginSettings: Seq[Def.Setting[_]] = Def.settings(
    libraryDependencies ++= {
      scalaVersion.value match {
        case SupportedScalaVersion(version) =>
          List(
            "org.scalameta" % s"scalahost-nsc_$version" % scalahostVersion % Scalameta
          )
        case _ => Nil
      }
    },
    // sets -Xplugin:/scalahost.jar and other necessary compiler options.
    scalacOptions ++= {
      scalahostJarPath.value.fold(List.empty[String]) { path =>
        List(
          s"-Xplugin:$path",
          "-Yrangepos",
          "-Xplugin-require:scalahost"
        )
      }
    }
  )

  lazy val scalahostHasMirrorSettings: Seq[Def.Setting[_]] = Def.settings(
    // Automatically set scalametaDependencies to projects listed in dependsOn(x % Scalahost)
    scalametaDependencies := thisProject.value.dependencies.collect {
      case x if x.configuration.exists(_ == Scalameta.name) => x.project
    },
    javaOptions ++= {
      if (scalametaDependencies.value.isEmpty) Nil
      else {
        val sourcepath =
          scalahostSourcepath.value
            .flatMap(_.map(_.getAbsolutePath))
            .mkString(java.io.File.pathSeparator)
        val classpath =
          scalahostClasspath.value
            .flatMap(_.files.map(_.getAbsolutePath))
            .mkString(java.io.File.pathSeparator)
        val projectName = name.value
        scalahostJarPath.value.map(path => s"-Dscalahost.jar=$path").toList ++
          List(
            s"-D$projectName.scalameta.sourcepath=$sourcepath",
            s"-D$projectName.scalameta.classpath=$classpath",
            s"-Dscalameta.sourcepath=$sourcepath",
            s"-Dscalameta.classpath=$classpath"
          )
      }
    },
    // fork := true is required to pass along -Dscalameta.mirror.{classpath,sourcepath}
    fork in run := {
      if (scalametaDependencies.value.isEmpty) fork.in(run).value
      else true
    },
    // automatically depend on scalahost if this project dependsOn(otherProject % Scalahost)
    libraryDependencies ++= {
      if (scalametaDependencies.value.isEmpty) Nil
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

  private lazy val scalahostJarPath: Def.Initialize[Task[Option[String]]] = Def.task {
    scalaVersion.value match {
      case SupportedScalaVersion(version) =>
        val jarRegex = s".*scalahost-nsc_$version(-$scalahostVersion)?.jar$$"
        val allJars = update.value.filter(configurationFilter(Scalameta.name)).allFiles
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
        Some(scalahostJar)
      case _ => None
    }
  }
  // Defaults to version.value of in scala.meta's build.sbt
  private val scalahostVersion: String =
    sys.props.getOrElse("scalahost.version", BuildInfo.version)
  private def scalahostAggregateFilter: Def.Initialize[ScopeFilter] = Def.setting {
    ScopeFilter(inProjects(scalametaDependencies.value.map(x => LocalProject(x.project)): _*),
                inConfigurations(Compile, Test, IntegrationTest))
  }
  private val scalahostSourcepath: Def.Initialize[Seq[Seq[File]]] =
    Def.settingDyn(sourceDirectories.all(scalahostAggregateFilter.value))
  private val scalahostClasspath: Def.Initialize[Task[Seq[Classpath]]] =
    Def.taskDyn(fullClasspath.all(scalahostAggregateFilter.value))
}

package scala.meta.scalahost.sbt

import scala.collection.mutable.ListBuffer

import sbt._
import Keys.{version => _, _}
import org.scalameta.BuildInfo
import sbt.ScopeFilter.ScopeFilter
import sbt.plugins.JvmPlugin

object ScalahostSbtPlugin extends AutoPlugin {
  object autoImport {

    // NOTE: This is duplicated in ConfigOps.scala in the scalahost project.
    // If we end up adding more complicated options like opting out of sections
    // we should consider sharing the same sources.
    sealed trait ScalametaSemanticdb
    object ScalametaSemanticdb {
      case object Disabled extends ScalametaSemanticdb
      case object Slim extends ScalametaSemanticdb
      case object Fat extends ScalametaSemanticdb
    }

    // use dependsOn(foo % Scalahost) to automatically include the semantic database
    // of foo in the scala.meta.Mirror() constructor.
    val Scalameta: Configuration = config("scalameta")
    val scalametaSourcepath: SettingKey[File] =
      settingKey[File]("What is the the base directory for all source files in this build?")
    val scalametaSemanticdb: SettingKey[ScalametaSemanticdb] =
      settingKey[ScalametaSemanticdb](
        "What kind of semanticdb to persist? See ScalametaSemanticdb for options.")
    val scalametaDependencies: SettingKey[Seq[ProjectRef]] =
      settingKey[Seq[ProjectRef]](
        "Projects to analyze with scalameta, automatically set by dependsOn(foo % Scalameta).")
  }
  import autoImport._
  override def requires = JvmPlugin
  override def trigger: PluginTrigger = AllRequirements
  override def projectSettings: Seq[Def.Setting[_]] = scalahostAllSettings

  lazy val scalahostAllSettings =
    scalahostBaseSettings ++
      inTestAndCompile(scalahostConfigSettings) ++
      scalahostInjectScalacOptions ++
      scalahostInjectDependency ++
      scalahostHasMirrorSettings

  lazy val scalahostBaseSettings: Seq[Def.Setting[_]] = Def.settings(
    ivyConfigurations += Scalameta,
    scalametaSourcepath := baseDirectory.in(ThisBuild).value,
    resolvers += Resolver.bintrayRepo("scalameta", "maven")
  )
  lazy val scalahostConfigSettings: Seq[Def.Setting[_]] = Def.settings(
    scalametaSemanticdb := ScalametaSemanticdb.Fat
  )

  lazy val scalahostInjectDependency: Seq[Def.Setting[_]] = Def.settings(
    libraryDependencies ++= {
      scalaVersion.value match {
        case SupportedScalaVersion(version) =>
          List(
            "org.scalameta" % s"scalahost_$version" % scalahostVersion % Scalameta
          )
        case _ => Nil
      }
    }
  )
  lazy val scalahostInjectScalacOptions: Seq[Def.Setting[_]] = Def.settings(
    // sets -Xplugin:/scalahost.jar and other necessary compiler options.
    scalacOptions ++= {
      scalahostJarPath.value.fold(List.empty[String]) { path =>
        val pluginName = "scalahost"
        val semanticdb = scalametaSemanticdb.value.toString.toLowerCase
        val sourcepath = scalametaSourcepath.value.getAbsolutePath
        List(
          s"-Xplugin:$path",
          s"-P:$pluginName:semanticdb:$semanticdb",
          s"-P:$pluginName:sourcepath:$sourcepath",
          "-Yrangepos",
          s"-Xplugin-require:$pluginName"
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
        val sourcepath = sys.props("user.dir")
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

  private def inTestAndCompile(settings: Seq[Def.Setting[_]]) =
    List(Compile, Test).flatMap(inConfig(_)(settings)) ++ settings

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
        val jarRegex = s".*scalahost_$version(-$scalahostVersion)?.jar$$"
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
  private val scalahostClasspath: Def.Initialize[Task[Seq[Classpath]]] =
    Def.taskDyn(fullClasspath.all(scalahostAggregateFilter.value))
}

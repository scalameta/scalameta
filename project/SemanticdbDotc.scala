package org.scalameta
package build

import sbt.Keys._
import sbt._

import scala.language.implicitConversions

import sbtbuildinfo.BuildInfoPlugin
import sbtbuildinfo.BuildInfoPlugin.autoImport.{_, given}
import sbtprotoc.ProtocPlugin.autoImport.PB
import ScalaPB._

/** The build code of semanticdb-dotc, apart from its project definition in build.sbt. */
object SemanticdbDotc {

  /** The configuration in which sbt-protoc runs ScalaPB. sbt does not compile its output. */
  val DotcGen = config("dotcgen").extend(Compile).hide

  private val dotcGenDir = Def.setting(target.value / "dotcgen")

  /** Copies the schema, with the module's own ScalaPB options in place of the schema's. */
  private val dotcSchema = Def.task {
    val schema = IO.read(SemanticdbProtoDir / "semanticdb.proto")
    val block = """(?s)option \(scalapb\.options\) = \{.*?\};""".r
    require(
      """\(scalapb\.""".r.findAllIn(schema).size == 1 && block.findFirstIn(schema).nonEmpty,
      "semanticdb.proto must have one ScalaPB option: its top-level options block",
    )
    val options =
      """|option (scalapb.options) = {
         |  package_name: "dotty.tools.dotc.semanticdb"
         |  scala3_sources: true
         |  lenses: false
         |  preserve_unknown_fields: false
         |};""".stripMargin
    val copy = block.replaceFirstIn(schema, scala.util.matching.Regex.quoteReplacement(options))
    val out = dotcGenDir.value / "proto" / "semanticdb.proto"
    // write only a changed copy, so that protoc does not run again
    if (!out.exists || IO.read(out) != copy) IO.write(out, copy)
  }

  /**
   * The scalac options of Scala 3's build for its compiler, by the minor version of Scala 3. Each
   * entry comes from project/Build.scala of Scala 3, at the tag in its comment. We compile the
   * module with the newest entry at or below the minor version of `scalaVersion`, and with -Werror.
   * sharedBaseSettings already has -feature and -unchecked.
   */
  private val ScalacOptionsByLine: Seq[(Int, Seq[String])] = {
    val common = Seq("-deprecation", "-encoding", "UTF8", "-language:implicitConversions")
    // 3.3.9: thisBuildSettings, commonDottyCompilerSettings, bootstrappedDottyCompilerSettings
    val line33 = common ++ Seq(
      "-Wconf:cat=deprecation&origin=scala\\.collection\\.mutable\\.AnyRefMap.*:s",
      "-Yexplicit-nulls",
      "-Ysafe-init",
    )
    // 3.8.4: thisBuildSettings
    val line38 = common ++ Seq("--java-output-version:17", "-Yexplicit-nulls", "-Wsafe-init")
    // 3.9.0: thisBuildSettings
    val line39 = line38 ++ Seq(
      "-Wconf:msg=package scala contains object and package with same name:i",
      "-Wconf:src=scaladoc-testcases/.*:s",
      "-Wconf:src=presentation-compiler-testcases/.*:s",
      "-Wconf:msg=The RHS of reassignment must be transitively initialized:i",
      "-Wconf:msg=Could not verify that the method argument is transitively initialized:i",
      "-Wconf:msg=may cause initialization errors:i",
      "-Wconf:cat=deprecation&origin=scala\\.collection\\.Iterable\\.stringPrefix:s",
    )
    Seq(3 -> line33, 8 -> line38, 9 -> line39)
  }

  val settings = Def.settings(
    moduleName := "semanticdb-dotc",
    description := "SemanticDB data structures for the Scala 3 compiler",
    Compile / doc / sources := Seq.empty,
    Compile / scalacOptions ++= {
      val minor = CrossVersion.partialVersion(scalaVersion.value).fold(0L)(_._2)
      "-Werror" +: ScalacOptionsByLine.findLast(_._1 <= minor).fold(Seq.empty[String])(_._2)
    },
    // compile the tests without the options of Scala 3's build
    Test / scalacOptions := scalacOptions.value,
    Compile / unmanagedSourceDirectories += (Compile / sourceDirectory).value / "generated",
    libraryDependencies ++= Seq(
      "com.thesamet.scalapb" %% "scalapb-runtime" % scalapbVersion.value % "protobuf",
      "org.scalameta" %% "munit" % munit.sbtmunit.BuildInfo.munitVersion % Test,
    ),
    inConfig(DotcGen)(Def.settings(
      sbtprotoc.ProtocPlugin.protobufConfigSettings,
      PB.protoSources := Seq(dotcGenDir.value / "proto"),
      // leave out the schema directory of semanticdb-shared, a test dependency
      PB.includePaths := PB.protoSources.value :+ PB.externalIncludePath.value,
      PB.targets := Seq(scalapbTarget(Def.setting(dotcGenDir.value / "scala")).value),
    )),
    inConfig(Test)(BuildInfoPlugin.buildInfoDefaultSettings),
    BuildInfoPlugin.buildInfoScopedSettings(Test),
    Test / buildInfoPackage := "scala.meta.tests.semanticdb.dotc",
    Test / buildInfoObject := "DotcBuildInfo",
    Test / buildInfoKeys := Seq[BuildInfoKey](
      "raw" -> dotcGenDir.value / "scala",
      "committed" -> (Compile / sourceDirectory).value / "generated",
      "classes" -> (Compile / classDirectory).value,
    ),
    Test / fullClasspath := Def.uncached {
      // copy the schema before sbt-protoc lists the schema directory
      val _ = Def.sequential(dotcSchema, DotcGen / PB.generate).value
      (Test / fullClasspath).value
    },
    // generate for save-dotc too: its runMain reads this classpath, not fullClasspath
    Test / fullClasspathAsJars := Def.uncached {
      val _ = (Test / fullClasspath).value
      (Test / fullClasspathAsJars).value
    },
  )

}

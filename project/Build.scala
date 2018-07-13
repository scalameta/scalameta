package org.scalameta
package build

import java.lang.ProcessBuilder._
import java.nio.file._
import java.nio.file.Files._
import scala.collection.JavaConverters._
import sbt._
import sbt.Keys._
import sbt.plugins._

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements

  import autoImport._
  object autoImport {
    trait BenchSuite {
      def initCommands: List[String] = List(
        "bench/clean",
        "wow " + Versions.LatestScala212
      )

      def metacpBenches: List[String]
      def metacpCommands: List[String] = {
        if (metacpBenches.isEmpty) Nil
        else List("bench/jmh:run " + metacpBenches.mkString(" "))
      }

      def scalacBenches: List[String]
      def scalacCommands: List[String] = {
        if (scalacBenches.isEmpty) Nil
        else List("bench/jmh:run " + scalacBenches.mkString(" "))
      }

      def scalametaBenches: List[String]
      def scalametaCommands: List[String] = {
        if (scalametaBenches.isEmpty) Nil
        else List("bench/jmh:run " + scalametaBenches.mkString(" "))
      }

      final def command: String = {
        val benchCommands = metacpCommands ++ scalacCommands ++ scalametaCommands
        (initCommands ++ benchCommands).map(c => s";$c ").mkString("")
      }
    }

    object benchLSP extends BenchSuite {
      def metacpBenches = List("Metacp")
      def scalacBenches = List("ScalacBaseline")
      def scalametaBenches = List("ScalametaBaseline")
    }

    object benchAll extends BenchSuite {
      def metacpBenches = List("Metacp")
      def scalacBenches = List("Scalac")
      def scalametaBenches = List("Scalameta")
    }

    object benchQuick extends BenchSuite {
      def metacpBenches = List("Metacp")
      def scalacBenches = Nil
      def scalametaBenches = List("ScalametaBaseline")
    }

    // https://stackoverflow.com/questions/41229451/how-to-disable-slow-tagged-scalatests-by-default-allow-execution-with-option
    lazy val Fast = config("fast").extend(Test)
    lazy val Slow = config("slow").extend(Test)
    lazy val All = config("all").extend(Test)

    val javacSemanticdbDirectory =
      SettingKey[File](
        "javacSemanticdbDirectory",
        "location of semanticdb produced by semanticdb-javac")
  }
}

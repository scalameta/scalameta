package org.scalameta
package build

import sbt._
import sbt.plugins._

object Build extends AutoPlugin {
  override def requires: Plugins = JvmPlugin
  override def trigger: PluginTrigger = allRequirements

  import autoImport._
  object autoImport {
    trait BenchSemanticdbSuite {
      def initCommands: List[String] = List("benchSemanticdb2_13/clean")

      private def toCommands(benches: Seq[String]): List[String] =
        if (benches.isEmpty) Nil else List(benches.mkString("benchSemanticdb2_13/Jmh/run ", " ", ""))

      def metacpBenches: List[String]
      def metacpCommands: List[String] = toCommands(metacpBenches)

      def scalacBenches: List[String]
      def scalacCommands: List[String] = toCommands(scalacBenches)

      def scalametaBenches: List[String]
      def scalametaCommands: List[String] = toCommands(scalametaBenches)

      final def command: String = {
        val benchCommands = metacpCommands ++ scalacCommands ++ scalametaCommands
        (initCommands ++ benchCommands).mkString("; ", "; ", "")
      }
    }

    object benchLSP extends BenchSemanticdbSuite {
      def metacpBenches = List("Metacp")
      def scalacBenches = List("ScalacBaseline")
      def scalametaBenches = List("ScalametaBaseline")
    }

    object benchAll extends BenchSemanticdbSuite {
      def metacpBenches = List("Metacp")
      def scalacBenches = List("Scalac")
      def scalametaBenches = List("Scalameta")
    }

    object benchQuick extends BenchSemanticdbSuite {
      def metacpBenches = List("Metacp")
      def scalacBenches = Nil
      def scalametaBenches = List("ScalametaBaseline")
    }
  }
}

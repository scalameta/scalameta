package org.scalameta
package build

import sbt.Keys._
import sbt._

/** How the build runs ScalaPB on the SemanticDB schema. */
object ScalaPB {

  // for SIP-51, the newest ScalaPB built against the earliest Scala 2.13.x we support
  def scalapbVersion = Def.setting(if (scalaVersion.value == "2.13.15") "0.11.17" else "0.11.20")

  def scalapbTarget(outputPath: Def.Initialize[File]) = Def.setting {
    /* sbt-protoc loads the generator in a classloader of its own, so the meta-build needs no
     * scalapb compilerplugin, whose Scala 3 build wants another protoc-bridge than sbt-protoc */
    val artifact = protocbridge
      .Artifact("com.thesamet.scalapb", "compilerplugin_2.13", scalapbVersion.value)
    protocbridge.Target(
      generator = protocbridge.SandboxedJvmGenerator
        .forModule("scala", artifact, "scalapb.ScalaPbCodeGenerator$", Nil),
      outputPath = outputPath.value,
      options = Seq("flat_package"), // what scalapb.gen(flatPackage = true) passes to protoc
    )
  }

  val SemanticdbProtoDir = file("semanticdb/semanticdb/shared/src/main/proto")

}

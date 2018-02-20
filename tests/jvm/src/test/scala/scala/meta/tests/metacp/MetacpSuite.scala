package scala.meta.tests.metacp

import scala.meta.internal.metacp.Main
import scala.tools.asm.ClassReader
import scala.tools.asm.tree.ClassNode
import org.langmeta.io.AbsolutePath

class MetacpSuite extends BaseMetacpSuite {

  check("scala-library", () => scalaLibraryJar)
//  checkLibrary("org.scalameta", "scalameta_2.12", "3.2.0")
//  checkLibrary("com.typesafe.akka", "akka-testkit_2.12", "2.5.9")
//  checkLibrary("org.apache.spark", "spark-sql_2.11", "2.2.1")

}

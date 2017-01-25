package scala.meta.tests
package scalahost

import scala.io.Source

// To manually test the converter against an individual file
class ManualSuite extends ConverterSuite {
  override val parseAsCompilationUnit: Boolean = true

  def checkUrl(url: String): Unit = {
    val code = Source.fromURL(url)("UTF-8").getLines().mkString("\n")
    check(code)
  }
  def check(code: String): Unit = {
    test(code.lines.filter(_.nonEmpty).take(1).mkString) {
      getConvertedMetaTree(code)
    }
  }
  // For example github raw url
  //  checkUrl(
  //    "https://raw.githubusercontent.com/ornicar/lila/e5b897ada8f7212ed69de886fcbbc26ea52f3b82/modules/relation/src/main/RelationActor.scala")
  // or code
  // check("""
  //     |package object a {
  //     |  println(1)
  //     |}
  //   """.stripMargin)
}

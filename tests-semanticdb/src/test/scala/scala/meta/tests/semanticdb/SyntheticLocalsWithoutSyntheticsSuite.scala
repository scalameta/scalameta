package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac._

class SyntheticLocalsWithoutSyntheticsSuite extends SemanticdbSuite {

  override def customizeConfig(config: SemanticdbConfig): SemanticdbConfig = config
    .copy(synthetics = BinaryMode.Off)

  checkSection(
    "local implicit class: symbols",
    """|object w1 {
       |  def m = { implicit class R(val x: Int) { def d = x }; 1.d }
       |}
       |""".stripMargin,
    """|_empty_/w1. => final object w1 extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/w1.m(). => method m: Int
       |  Int => scala/Int#
       |local0 => implicit class R extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => val param x: Int
       |  Int => scala/Int#
       |local2 => primary ctor <init>(val x: Int)
       |  x => local1
       |  Int => scala/Int#
       |local3 => method d: Int
       |  Int => scala/Int#
       |local4 => synthetic implicit method R(val x: Int): R
       |  x => local1
       |  Int => scala/Int#
       |  R => local0
       |""".stripMargin,
    "Symbols",
  )

}

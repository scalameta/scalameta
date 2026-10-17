package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1636
class Issue1636Suite extends SemanticdbSuite {

  if (ScalaVersion.is212) synthetics(
    """|object m1 {
       |  def m[CC[_]](implicit cbf: collection.generic.CanBuildFrom[Nothing, Int, CC[Int]]): CC[Int] =
       |    List.empty[Int].to[CC]
       |}
       |""".stripMargin,
    """|[2:4..2:26): List.empty[Int].to[CC] => *(cbf)
       |  cbf => _empty_/m1.m().(cbf)
       |""".stripMargin,
  )

  synthetics(
    """|object m2 {
       |  def m(xs: List[Int])(implicit ord: Ordering[Int]): List[Int] = xs.sorted
       |}
       |""".stripMargin,
    """|[1:65..1:74): xs.sorted => *(ord)
       |  ord => _empty_/m2.m().(ord)
       |[1:65..1:74): xs.sorted => *[Int]
       |  Int => scala/Int#
       |""".stripMargin,
  )

}

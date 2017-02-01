package scala.meta.tests
package scalahost
package v1

class Integration extends OnlineMirrorSuite {
  database(
    """
    |object Test {
    |  def main(args: Array[String]): Unit = {
    |    val list = List(1, 2, 3)
    |    println(list)
    |  }
    |}
  """.trim.stripMargin,
    """
    |[7..11): Test => _empty_.Test.
    |[20..24): main => _empty_.Test.main([Ljava/lang/String;)V.
    |[25..29): args => _empty_.Test.main([Ljava/lang/String;)V.(args)
    |[31..36): Array => _root_.scala.Array#
    |[37..43): String => _root_.scala.Predef.String#
    |[47..51): Unit => _root_.scala.Unit#
    |[64..68): list => file:<...>@64
    |[71..75): List => _root_.scala.collection.immutable.List.apply(Lscala/collection/Seq;)Lscala/collection/immutable/List;.
    |[89..96): println => _root_.scala.Predef.println(Ljava/lang/Object;)V.
    |[97..101): list => file:<...>@64
  """.trim.stripMargin)
}

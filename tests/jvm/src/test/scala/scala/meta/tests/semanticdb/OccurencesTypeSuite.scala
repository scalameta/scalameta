package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac.OccurrenceMode

class OccurencesTypeSuite extends SemanticdbSuite(occurences = OccurrenceMode.SymbolAndType) {


  occurrences(
    """object L2 {
	    |  val list = List(1, 2, 3)
	    |  list
			|  list.head
	    |}""".stripMargin
	  ,
    """[0:7..0:9): L2 <= _empty_.L2.
			|[1:6..1:10): list <= _empty_.L2.list().
			|[1:13..1:17): List => scala.collection.immutable.List. ((Int*) => List[Int])
			|[2:2..2:6): list => _empty_.L2.list(). (List[Int])
			|[3:2..3:6): list => _empty_.L2.list(). (List[Int])
			|[3:7..3:11): head => scala.collection.IterableLike#head(). (Int)""".stripMargin
  )

	synthetics(
		"""object L2 {
			|  val head = Array(1, 2, 3)(0)
			|  def ala(a: Int*): Int = 1
			|}""".stripMargin
		,
		"""[1:18..1:18):  => *.apply
			|  [0:0..0:1): * => _star_.
			|  [0:2..0:7): apply => scala.Array.apply(Int,Int*). ((Int, Int*) => Array[Int])
			|[1:27..1:27):  => *.apply
			|  [0:0..0:1): * => _star_.
			|  [0:2..0:7): apply => scala.Array#apply(Int). ((Int) => Int)
			|""".stripMargin
	)

	occurrences(
		"""object L3 { def param(a: Seq[Int]) = a.head } """
	,
		"""[0:7..0:9): L3 <= _empty_.L3.
			|[0:16..0:21): param <= _empty_.L3.param(Seq).
			|[0:22..0:23): a <= _empty_.L3.param(Seq).(a)
			|[0:25..0:28): Seq => scala.package.Seq# (Seq)
			|[0:29..0:32): Int => scala.Int# (Int)
			|[0:37..0:38): a => _empty_.L3.param(Seq).(a) (Seq[Int])
			|[0:39..0:43): head => scala.collection.IterableLike#head(). (Int)""".stripMargin
	)
}

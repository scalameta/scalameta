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

	def varCode(name: String) =
							s"""object $name {
								 |  var foo = List(1,2)
								 |  foo = List(1,2,3)
								 |  foo_=(Nil)
								 |  foo ::= 3
 								 |}""".stripMargin

	occurrences(varCode("VarOccurrences"),
		"""[0:7..0:21): VarOccurrences <= _empty_.VarOccurrences.
			|[1:6..1:9): foo <= _empty_.VarOccurrences.foo().
			|[1:12..1:16): List => scala.collection.immutable.List. ((Int*) => List[Int])
			|[2:2..2:5): foo => _empty_.VarOccurrences.`foo_=`(List). ((List[Int]) => Unit)
			|[2:8..2:12): List => scala.collection.immutable.List. ((Int*) => List[Int])
			|[3:2..3:7): foo_= => _empty_.VarOccurrences.`foo_=`(List). ((List[Int]) => Unit)
			|[3:8..3:11): Nil => scala.collection.immutable.Nil. (Nil)
			|[4:2..4:5): foo => _empty_.VarOccurrences.foo(). (List[Int])
			|[4:6..4:9): ::= => scala.collection.immutable.List#`::`(B). ((Int) => List[Int])""".stripMargin
	)

	synthetics(varCode("VarSynthetics"),
		"""[1:16..1:16):  => *.apply[Int]
			|  [0:0..0:1): * => _star_.
			|  [0:2..0:7): apply => scala.collection.immutable.List.apply(A*). ((Int*) => List[Int])
			|  [0:8..0:11): Int => scala.Int# ((Int*) => List[Int])
			|[2:12..2:12):  => *.apply[Int]
			|  [0:0..0:1): * => _star_.
			|  [0:2..0:7): apply => scala.collection.immutable.List.apply(A*). ((Int*) => List[Int])
			|  [0:8..0:11): Int => scala.Int# ((Int*) => List[Int])
			|[4:2..4:11): foo ::= 3 => VarSynthetics.this.`foo_=`(*)
			|  [0:19..0:26): `foo_=` => _empty_.VarSynthetics.`foo_=`(List). (Unit)
			|  [0:27..0:28): * => _star_.
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

	occurrences(
		"""object L4 { val a = List.empty[Int].map(_ + 1) } """
	,
		"""[0:7..0:9): L4 <= _empty_.L4.
			|[0:16..0:17): a <= _empty_.L4.a().
			|[0:20..0:24): List => scala.collection.immutable.List. (List)
			|[0:25..0:30): empty => scala.collection.immutable.List.empty(). (List[Int])
			|[0:31..0:34): Int => scala.Int# (Int)
			|[0:36..0:39): map => scala.collection.immutable.List#map(Function1,CanBuildFrom). ((Function1[Int, Int])(CanBuildFrom[List[Int], Int, List[Int]]) => List[Int])
			|[0:40..0:45): _ + 1 => _.lambda. (Function1[Int, Int])
			|[0:42..0:43): + => scala.Int#`+`(Int). ((Int) => Int)""".stripMargin
	)

	occurrences(
		"""import scala.concurrent.Future
			|import scala.concurrent.ExecutionContext.Implicits.global
			|
			|class Futures {
			|  val unitFuture = Future(println("ala"))
			|  val nonUnitFuture = Future("ala")
			|}""".stripMargin
		,
		"""[0:7..0:12): scala => scala. (scala)
			|[0:13..0:23): concurrent => scala.concurrent. (concurrent)
			|[0:24..0:30): Future => scala.concurrent.Future#
			|[0:24..0:30): Future => scala.concurrent.Future.
			|[1:7..1:12): scala => scala. (scala)
			|[1:13..1:23): concurrent => scala.concurrent. (concurrent)
			|[1:24..1:40): ExecutionContext => scala.concurrent.ExecutionContext. (ExecutionContext)
			|[1:41..1:50): Implicits => scala.concurrent.ExecutionContext.Implicits. (Implicits)
			|[1:51..1:57): global => scala.concurrent.ExecutionContext.Implicits.global().
			|[3:6..3:13): Futures <= _empty_.Futures#
			|[3:14..3:14):  <= _empty_.Futures#`<init>`(). (Futures)
			|[4:6..4:16): unitFuture <= _empty_.Futures#unitFuture().
			|[4:19..4:25): Future => scala.concurrent.Future. ((=> Unit)(ExecutionContext) => Future[Unit])
			|[4:26..4:33): println => scala.Predef.println(Any). ((Any) => Unit)
			|[5:6..5:19): nonUnitFuture <= _empty_.Futures#nonUnitFuture().
			|[5:22..5:28): Future => scala.concurrent.Future. ((=> String)(ExecutionContext) => Future[String])""".stripMargin
	)
}

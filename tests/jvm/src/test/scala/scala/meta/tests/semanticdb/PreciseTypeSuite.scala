package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac.SymbolMode
import scala.meta.internal.semanticdb3.Print
import scala.meta.internal.{semanticdb3 => s}

class PreciseTypeSuite extends SemanticdbSuite(symbols = SymbolMode.AllWithMaterializedTypes) {

  private def testActualTypes(code: String)(occurencesTypes: String) = test(code){
    val db = computeDatabaseFromSnippet(code)
    val allOccurences = db.occurrences ++ db.synthetics.flatMap(_.text).flatMap(_.occurrences)

    def printOccurence(occ: s.SymbolOccurrence) = occ.materializedTpe.map( tpe =>
          s"${Print.range(occ.range.get)} ${Print.tpe(db, tpe)}"
      )

    val occurencesStrings = for {
      syn <- db.synthetics
      text = syn.text.get
      occ <- text.occurrences
    } yield printOccurence(occ).map(t => s"${Print.range(syn.range.get)} ~> $t")

    val typeLines = db.occurrences.map(printOccurence) ++ occurencesStrings
    val typeString = typeLines.flatten.sorted.mkString("\n")
    assert(typeString == occurencesTypes)
  }

  testActualTypes(
    "object L2 { val head = Array(1, 2, 3)(0) }"
  )(
    """[0:23..0:28) scala.Array.
      |[0:28..0:28) ~> [0:2..0:7) scala.Array.apply(Int,Int*).(x)scala.Array.apply(Int,Int*).(xs)scala.Array#scala.Int#
      |[0:37..0:37) ~> [0:2..0:7) scala.Array#apply(Int).(i)scala.Int#""".stripMargin
  )

	testActualTypes(
		"""object L2 {
			|  var foo = List(1,2)
			|  foo = List(1,2,3)
			|  foo ::= 3
			|}
		""".stripMargin
	)(
		"""[1:12..1:16) scala.collection.immutable.List.apply(A*).(xs)scala.collection.immutable.List#scala.Int#
			|[1:16..1:16) ~> [0:2..0:7) scala.collection.immutable.List.apply(A*).(xs)scala.collection.immutable.List#scala.Int#
			|[1:16..1:16) ~> [0:8..0:11) scala.collection.immutable.List.apply(A*).(xs)scala.collection.immutable.List#scala.Int#
			|[2:12..2:12) ~> [0:2..0:7) scala.collection.immutable.List.apply(A*).(xs)scala.collection.immutable.List#scala.Int#
			|[2:12..2:12) ~> [0:8..0:11) scala.collection.immutable.List.apply(A*).(xs)scala.collection.immutable.List#scala.Int#
			|[2:2..2:5) _empty_.L2.`foo_=`(List).(x$1)scala.collection.immutable.List#scala.Int#scala.Unit#
			|[2:8..2:12) scala.collection.immutable.List.apply(A*).(xs)scala.collection.immutable.List#scala.Int#
			|[3:2..3:5) _empty_.L2.`foo_=`(List).(x$1)scala.collection.immutable.List#scala.Int#scala.Unit#
			|[3:6..3:9) scala.collection.immutable.List#`::`(B).(x)scala.collection.immutable.List#scala.Int#""".stripMargin
	)

	testActualTypes(
		"""object L3 { def param(a: Seq[Int]) = a.head } """
	)(
		"""[0:25..0:28) scala.package.Seq#
			|[0:29..0:32) scala.Int#
			|[0:37..0:38) scala.package.Seq#scala.Int#
			|[0:39..0:43) scala.Int#""".stripMargin
	)

	testActualTypes(
		"""object L4 { val a = List.empty[Int].map(_ + 1) } """
	)(
		"""[0:20..0:24) scala.collection.immutable.List.
			|[0:25..0:30) scala.collection.immutable.List#scala.Int#
			|[0:31..0:34) scala.Int#
			|[0:36..0:39) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
			|[0:39..0:39) ~> [0:12..0:15) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
			|[0:39..0:39) ~> [0:2..0:5) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
			|[0:39..0:39) ~> [0:7..0:11) scala.collection.immutable.List#map(Function1,CanBuildFrom).(f)scala.collection.immutable.List#map(Function1,CanBuildFrom).(bf)scala.collection.immutable.List#scala.Int#
			|[0:42..0:43) scala.Int#`+`(Int).(x)scala.Int#
			|[0:46..0:46) ~> [0:34..0:46) scala.collection.immutable.List#scala.Int#
			|[0:46..0:46) ~> [0:47..0:50) scala.collection.immutable.List#scala.Int#""".stripMargin
	)
}

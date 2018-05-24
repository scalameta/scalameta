package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac.OccurrenceMode

class OccurencesTypeSuite extends SemanticdbSuite(occurences = OccurrenceMode.SymbolAndType) {
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

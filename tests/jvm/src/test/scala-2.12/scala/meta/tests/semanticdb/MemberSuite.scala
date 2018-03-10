package scala.meta.tests
package semanticdb

import scala.meta.internal.semanticdb.scalac._

class MemberSuite extends DatabaseSuite(SemanticdbMode.Slim, members = MemberMode.All) {
  members(
    """
      |object a {
      |  trait A {
      |    def a = 2
      |    def a(k: Int) = 2
      |    val b = 3
      |    var c = 3
      |    lazy val d = 3
      |    type E
      |    class F
      |    trait G
      |  }
      |  class AA {
      |    def aa = 3
      |  }
      |  trait B extends AA with A
      |  class C extends AA with A
      |}
    """.stripMargin,
    """
      |_empty_.a.A#{
      |  G#
      |  F#
      |  d.
      |  `c_=`.
      |  c.
      |  b.
      |  a.
      |  a.
      |  E#
      |}
      |_empty_.a.AA#{
      |  aa.
      |}
    """.stripMargin
  )

  symbols(
    """
      |import scala.util._
    """.stripMargin,
    """
      |scala. => package scala
      |scala.util. => package util.{+72 members}
    """.stripMargin
  )

  members(
    """
      |import scala.concurrent.Future._
    """.stripMargin,
    """
      |scala.concurrent.Future.{
      |  InternalCallbackExecutor.
      |  traverse.
      |  reduceLeft.
      |  reduce.
      |  fold.
      |  foldNext.
      |  foldLeft.
      |  find.
      |  find.
      |  firstCompletedOf.
      |  sequence.
      |  apply.
      |  fromTry.
      |  successful.
      |  failed.
      |  `unit `.
      |  unit.
      |  never.
      |  `toBoxed `.
      |  toBoxed.
      |}
    """.stripMargin
  )
  members(
    """
      |object b {
      |  val c = util.Success("")
      |  import c._
      |}
    """.stripMargin,
    """
      |_empty_.b.c().{
      |  canEqual.
      |  productIterator.
      |  productElement.
      |  productArity.
      |  productPrefix.
      |  copy$default$1.
      |  copy.
      |  fold.
      |  toEither.
      |  toOption.
      |  failed.
      |  recoverWith.
      |  recover.
      |  filter.
      |  collect.
      |  map.
      |  transform.
      |  foreach.
      |  flatten.
      |  flatMap.
      |  orElse.
      |  getOrElse.
      |  get.
      |  isSuccess.
      |  isFailure.
      |  `value `.
      |  value.
      |  WithFilter#
      |  withFilter.
      |}
    """.stripMargin
  )

  members(
    """
      |package scala.util.matching
    """.stripMargin,
    """
      |scala.util.matching.{
      |  Regex$MatchData.
      |  Regex$MatchData#
      |  Regex$Groups$.
      |  Regex$Groups$#
      |  Regex$MatchIterator$$anon$1.
      |  Regex$MatchIterator$$anon$1#
      |  Regex$.
      |  Regex$#
      |  Regex$$anon$4.
      |  Regex$$anon$4#
      |  Regex.
      |  Regex#
      |  Regex$$anon$2.
      |  Regex$$anon$2#
      |  Regex$MatchIterator$$anon$3.
      |  Regex$MatchIterator$$anon$3#
      |  Regex$Match$.
      |  Regex$Match$#
      |  UnanchoredRegex.
      |  UnanchoredRegex#
      |  Regex$MatchIterator.
      |  Regex$MatchIterator#
      |  Regex$Match.
      |  Regex$Match#
      |  Regex$Replacement.
      |  Regex$Replacement#
      |}
    """.stripMargin
  )

  members(
    """
      |object a {
      |  new java.lang.Runnable {
      |    def run(): Unit = ()
      |  }
      |}""".stripMargin,
    """
      |java.lang.Runnable#{
      |  run.
      |}
      |""".stripMargin
  )

  members(
    """
      |object a {
      |  new java.util.TimerTask {
      |    def run(): Unit = ()
      |  }
      |}""".stripMargin,
    """
      |java.util.TimerTask#{
      |  scheduledExecutionTime.
      |  cancel.
      |  period.
      |  nextExecutionTime.
      |  state.
      |  lock.
      |  run.
      |}
      |""".stripMargin
  )

  members(
    """
      |object a {
      |  new java.lang.AutoCloseable with java.lang.Iterable[Int] {
      |    def close(): Unit = ???
      |    def iterator(): java.util.Iterator[Int] = ???
      |  }
      |}""".stripMargin,
    """
      |java.lang.Iterable#{
      |  spliterator.
      |  forEach.
      |  iterator.
      |}
      |java.lang.AutoCloseable#{
      |  close.
      |}
      |""".stripMargin
  )

  members(
    """
      |object a {
      |  class Bar()
      |  trait Foo { def bar: Int }
      |}""".stripMargin,
    ""
  )
}

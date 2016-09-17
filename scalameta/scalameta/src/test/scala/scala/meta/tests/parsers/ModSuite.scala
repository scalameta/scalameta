package scala.meta.tests
package parsers

import org.scalatest.exceptions.TestFailedException

import scala.meta._

class ModSuite extends ParseSuite {
  def interceptParseErrors(stats: String*) = {
    stats.foreach { stat =>
      try {
        intercept[parsers.ParseException] {
          templStat(stat)
        }
      } catch {
        case t: TestFailedException =>
          val msg = "no exception was thrown"
          val richFeedback = t.message.map(_.replace(msg, s"$msg for '$stat'"))
          throw new TestFailedException(richFeedback.get,
                                        t.failedCodeStackDepth)
      }
    }
  }

  test("implicit") {
    val Defn.Object(Seq(Mod.Implicit()), _, _) = templStat("implicit object A")
    val Defn.Class(Seq(Mod.Implicit()), _, _, _, _) = templStat("implicit class A")
    val Defn.Object(Seq(Mod.Implicit(), Mod.Case()), _, _) = templStat("implicit case object A")

    val Defn.Def(_, _, _, Seq(Seq(Term.Param(Seq(Mod.Implicit()), _, _, _))), _, _) =
      templStat("def foo(implicit a: Int): Int = a")

    val Defn.Def(Seq(Mod.Implicit()), _, _, _, _, _) = templStat("implicit def foo(a: Int): Int = a")

    val Defn.Val(Seq(Mod.Implicit()), _, _, _) = templStat("implicit val a: Int = 1")
    val Decl.Val(Seq(Mod.Implicit()), _, _) = templStat("implicit val a: Int")

    val Defn.Var(Seq(Mod.Implicit()), _, _, _) = templStat("implicit var a: Int = 1")
    val Decl.Var(Seq(Mod.Implicit()), _, _) = templStat("implicit var a: Int")

    interceptParseErrors(
      "implicit implicit var a: Int",
      "implicit implicit val a: Int",
      "implicit implicit class A",
      "implicit implicit object A",
      "implicit implicit trait A",
      "implicit implicit case class A",
      "implicit trait A",
      "implicit case class A(a: Int)"
    )
  }

  test("final") {
    val Defn.Object(Seq(Mod.Final()), _, _) = templStat("final object A")
    val Defn.Class(Seq(Mod.Final()), _, _, _, _) = templStat("final class A")
    val Defn.Class(Seq(Mod.Final(), Mod.Case()), _, _, _, _) = templStat("final case class A(a: Int)")
    val Defn.Object(Seq(Mod.Final(), Mod.Case()), _, _) = templStat("final case object A")

    val Defn.Def(Seq(Mod.Final()), _, _, _, _, _) = templStat("final def foo(a: Int): Int = a")
    val Defn.Val(Seq(Mod.Final()), _, _, _) = templStat("final val a: Int = 1")
    val Decl.Val(Seq(Mod.Final()), _, _) = templStat("final val a: Int")

    val Defn.Var(Seq(Mod.Final()), _, _, _) = templStat("final var a: Int = 1")
    val Decl.Var(Seq(Mod.Final()), _, _) = templStat("final var a: Int")

    interceptParseErrors(
      "final final var a: Int",
      "final final val a: Int",
      "final final class A",
      "final final object A",
      "final final trait A",
      "final final case class A",
      "final trait A",
      "def foo(final val a: Int): Int = a"
    )
  }

  // TODO: sealed
  // TODO: override
  // TODO: case
  // TODO: abstract
  // TODO: covariant
  // TODO: contravariant
  // TODO: lazy
  // TODO: abstract override
  // TODO: macro
  // TODO: val param
  // TODO: var param
}

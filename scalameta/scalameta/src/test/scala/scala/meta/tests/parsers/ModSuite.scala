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
      "implicit implicit var a: Int = 1",
      "implicit implicit val a: Int = 1",
      "implicit implicit class A",
      "implicit implicit object A",
      "implicit implicit trait A",
      "implicit implicit case class A",
      "implicit implicit type A",
      "implicit implicit type A = Int",
      "implicit trait A",
      "implicit type A",
      "implicit type A = Int",
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
    val Defn.Type(Seq(Mod.Final()), _, _, _) = templStat("final type A = Int")

    interceptParseErrors(
      "final final var a: Int",
      "final final val a: Int",
      "final final var a: Int = 1",
      "final final val a: Int = 1",
      "final final class A",
      "final final object A",
      "final final trait A",
      "final final case class A",
      "final final type A",
      "final trait A",
      "def foo(final val a: Int): Int = a"
    )
  }

  test("sealed") {
    val Defn.Trait(Seq(Mod.Sealed()), _, _, _, _) = templStat("sealed trait A")
    val Defn.Class(Seq(Mod.Sealed()), _, _, _, _) = templStat("sealed class A")
    val Defn.Class(Seq(Mod.Sealed(), Mod.Abstract()), _, _, _, _) = templStat("sealed abstract class A")
    val Defn.Class(Seq(Mod.Sealed(), Mod.Case()), _, _, _, _) = templStat("sealed case class A(a: Int)")

    interceptParseErrors(
      "sealed sealed var a: Int",
      "sealed sealed val a: Int",
      "sealed sealed var a: Int = 1",
      "sealed sealed val a: Int = 1",
      "sealed sealed class A",
      "sealed sealed object A",
      "sealed sealed trait A",
      "sealed sealed case class A",
      "sealed sealed type A",
      "sealed object A",
      "sealed case object A",
      "sealed def foo(a: Int): Int = a",
      "sealed val a: Int = 1",
      "sealed val a: Int",
      "sealed var a: Int = 1",
      "sealed var a: Int",
      "sealed type A",
      "sealed type A = Int",
      "def foo(sealed val a: Int): Int = a"
    )
  }

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

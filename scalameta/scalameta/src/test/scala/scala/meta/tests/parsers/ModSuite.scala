package scala.meta.tests
package parsers

import scala.meta._

class ModSuite extends ParseSuite {
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

    intercept[parsers.ParseException] {
      templStat("implicit implicit var a: Int")
    }

    intercept[parsers.ParseException] {
      templStat("implicit implicit val a: Int")
    }

    intercept[parsers.ParseException] {
      templStat("implicit implicit class A")
    }

    intercept[parsers.ParseException] {
      templStat("implicit implicit object A")
    }

    intercept[parsers.ParseException] {
      templStat("implicit implicit trait A")
    }

    intercept[parsers.ParseException] {
      templStat("implicit implicit case class A")
    }

    intercept[parsers.ParseException] {
      templStat("implicit trait A")
    }

    intercept[parsers.ParseException] {
      templStat("implicit case class A(a: Int)")
    }

    intercept[parsers.ParseException] {
      templStat("implicit package A")
    }

    intercept[parsers.ParseException] {
      templStat("implicit package object A")
    }
  }

  // TODO: final
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

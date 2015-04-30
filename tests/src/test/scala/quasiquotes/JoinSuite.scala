import org.scalatest._
import scala.meta.internal.ast._
import scala.meta.dialects.Scala211

class JoinSuite extends FunSuite {
  test("idents") {
    val Term.Name("xtemp") = {
      import scala.meta._
      q"xtemp"
    }
    val Term.Name("ytemp") = {
      import scala.meta._
      q"ytemp"
    }
  }

  test("vals") {
    val fields = List(Pat.Var.Term(Term.Name("x")) -> Term.Name("xtemp"), Pat.Var.Term(Term.Name("y")) -> Term.Name("ytemp"))
    val vals = fields.map{ case (f, ref) =>
      import scala.meta._
      q"val $f = $ref.${f.name}"
    }
    assert(vals.length === 2)
    val Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), None, Term.Select(Term.Name("xtemp"), Term.Name("x"))) = vals(0)
    val Defn.Val(Nil, List(Pat.Var.Term(Term.Name("y"))), None, Term.Select(Term.Name("ytemp"), Term.Name("y"))) = vals(1)
  }

  test("result") {
    val x = Term.Name("x")
    val y = Term.Name("y")
    val valsin = List(
      Defn.Val(Nil, List(Pat.Var.Term(Term.Name("x"))), None, Term.Select(Term.Name("xtemp"), Term.Name("x"))),
      Defn.Val(Nil, List(Pat.Var.Term(Term.Name("y"))), None, Term.Select(Term.Name("ytemp"), Term.Name("y"))))
    val result = {
      import scala.meta._
      q"""
        val xtemp = $x
        val ytemp = $y
        new { ..$valsin }
      """
    }
    val Term.Block(stats) = result
    assert(stats.length === 3)
    val Defn.Val(Nil, List(Pat.Var.Term(Term.Name("xtemp"))), None, Term.Name("x")) = stats(0)
    val Defn.Val(Nil, List(Pat.Var.Term(Term.Name("ytemp"))), None, Term.Name("y")) = stats(1)
    val Term.New(Template(Nil, Nil, Term.Param(Nil, Name.Anonymous(), None, None), Some(valsout))) = stats(2)
    assert(valsout.length === 2)
    assert(valsout(0).toString === valsin(0).toString)
    assert(valsout(1).toString === valsin(1).toString)
  }
}
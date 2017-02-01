package scala.meta.tests
package ast

import org.scalatest._
import scala.meta._
import scala.meta.internal.ast._
import scala.meta.dialects.{Scala211, QuasiquoteTerm}

class InfrastructureSuite extends FunSuite {
  test("become for Quasi-0") {
    val dialect = QuasiquoteTerm(Scala211, multiline = false)
    val q = dialect("$hello").parse[Term].get.asInstanceOf[Term.Quasi]
    assert(q.become[Type.Quasi].show[Structure] === """Type.Quasi(0, Term.Name("hello"))""")
    assert(q.become[Type.Quasi].pos.toString === """[0..6) in Input.String("$hello")""")
  }

  test("become for Quasi-1") {
    val dialect = QuasiquoteTerm(Scala211, multiline = false)
    val Term.Block(Seq(q: Stat.Quasi)) = dialect("..$hello").parse[Stat].get
    assert(q.become[Type.Quasi].show[Structure] === """Type.Quasi(1, Type.Quasi(0, Term.Name("hello")))""")
    assert(q.become[Type.Quasi].pos.toString === """[0..8) in Input.String("..$hello")""")
  }

  test("copy parent") {
    val Term.Select(x1: Term.Name, _) = "foo.bar".parse[Term].get
    val x2 = x1.copy()
    assert(x1.parent.nonEmpty === true)
    assert(x2.parent.nonEmpty === false)
  }

  test("copy pos") {
    val x1 = "foo".parse[Term].get.asInstanceOf[Term.Name]
    val x2 = x1.copy()
    assert(x1.pos.nonEmpty === true)
    assert(x2.pos.nonEmpty === false)
  }

  test("copy tokens") {
    val x1 = "foo".parse[Term].get.asInstanceOf[Term.Name]
    val x2 = x1.copy()
    assert(x1.tokens.nonEmpty === true)
    assert(x2.tokens.nonEmpty === true)
  }
}
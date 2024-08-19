package scala.meta.tests
package trees

import scala.meta._

class InfrastructureSuite extends TreeSuiteBase {

  implicit val dialect: Dialect = dialects.Scala211

  test("become for Quasi-0") {
    val dialect = this.dialect.unquoteTerm(multiline = false)
    val q = dialect("$hello").parse[Term].get.asInstanceOf[Term.Quasi]
    assertTree(q.become[Type])(Type.Quasi(0, tname("hello")))
    assertEquals(q.become[Type].pos.toString, """[0..6) in Input.String("$hello")""")
  }

  test("become for Quasi-1") {
    val dialect = this.dialect.unquoteTerm(multiline = false)
    val Term.Block(List(q: Stat.Quasi)) = dialect("..$hello").parse[Stat].get
    assertTree(q.become[Type])(Type.Quasi(1, Type.Quasi(0, tname("hello"))))
    assertEquals(q.become[Type].pos.toString, """[0..8) in Input.String("..$hello")""")
  }

  test("copy parent") {
    val Term.Select(x1: Term.Name, _) = "foo.bar".parse[Term].get
    val x2 = x1.copy()
    assert(x1.parent.nonEmpty)
    assert(x2.parent.isEmpty)
  }

  test("copy pos") {
    val x1 = "foo".parse[Term].get.asInstanceOf[Term.Name]
    val x2 = x1.copy()
    assert(x1.pos != Position.None)
    assertEquals(x2.pos, Position.None)
  }

  test("copy tokens") {
    val x1 = "foo".parse[Term].get.asInstanceOf[Term.Name]
    val x2 = x1.copy()
    assertEquals(x1.tokens.nonEmpty, true)
    assertEquals(x2.tokenizeFor(implicitly[Dialect]).nonEmpty, true)
  }
}

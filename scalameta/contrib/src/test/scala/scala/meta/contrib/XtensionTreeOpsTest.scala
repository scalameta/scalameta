package scala.meta
package contrib

import scala.meta.testkit.SyntaxAnalysis

import org.scalatest.FunSuite

class XtensionTreeOpsTest extends FunSuite {
  val a: Defn.Val = q"val x = 2"

  test("testForeach") {
    var obtained = List.empty[String]
    a.foreach(x => obtained = x.structure :: obtained)
    assert(
      obtained ==
        List(
          "Lit(2)",
          "Term.Name(\"x\")",
          "Pat.Var.Term(Term.Name(\"x\"))",
          "Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name(\"x\"))), None, Lit(2))"
        ))
  }

  test("find property") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      val names = ast.collect { case t: Defn.Val => t.copy(pats = t.pats) }
      names.filterNot(name => ast.contains[Structural](name))
    }
    assert(errors.isEmpty)
  }

  test("parents") {
    val lit: Tree = q"val x = { 2 + 3 }".find(_.equal[Structural](q"3")).get
    assert(lit.ancestors.length == 3)
  }

}

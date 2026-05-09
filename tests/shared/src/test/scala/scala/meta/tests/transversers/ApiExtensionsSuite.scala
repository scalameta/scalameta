package scala.meta.tests.transversers

import scala.meta._
import scala.meta.tests.TreeSuiteBase

import scala.collection.mutable.ListBuffer

class ApiExtensionsSuite extends TreeSuiteBase {
  val a: Defn.Val = q"val x = 2"

  test("test dfs") {
    val expected = List(
      """|Defn.Val(
         |  Nil,
         |  List(
         |    Pat.Var(Term.Name("x"))
         |  ),
         |  None,
         |  Lit.Int(2)
         |)""".stripMargin.lf2nl,
      """Pat.Var(Term.Name("x"))""",
      """Term.Name("x")""",
      """Lit.Int(2)""",
    )
    def dfsTraverse = {
      val buf = new ListBuffer[String]
      a.dfs(x => buf.append(x.structure))
      buf.result()
    }
    def dfsCollect = a.dfsCollect(x => Some(x.structure))
    def dfsCollectEach = a.dfsCollectEach(x => x.structure :: Nil)
    assertEquals(dfsTraverse, expected)
    assertEquals(dfsCollect, expected)
    assertEquals(dfsCollectEach, expected)
  }

  test("test bfs") {
    val expected = List(
      """|Defn.Val(
         |  Nil,
         |  List(
         |    Pat.Var(Term.Name("x"))
         |  ),
         |  None,
         |  Lit.Int(2)
         |)""".stripMargin.lf2nl,
      """Pat.Var(Term.Name("x"))""",
      """Lit.Int(2)""",
      """Term.Name("x")""",
    )
    def bfsTraverse = {
      val buf = new ListBuffer[String]
      a.bfs(x => buf.append(x.structure))
      buf.result()
    }
    def bfsCollect = a.bfsCollect(x => Some(x.structure))
    def bfsCollectEach = a.bfsCollectEach(x => x.structure :: Nil)
    assertEquals(bfsTraverse, expected)
    assertEquals(bfsCollect, expected)
    assertEquals(bfsCollectEach, expected)
  }

  test("dfsForall") {
    assert(a.dfsForall(_.is[Tree]))
    assert(!a.dfsForall(_.is[Term]))
    assert(!a.dfsForall(_.is[Type]))
    assert(q"3".dfsForall(_.syntax == "3"))
  }

  test("bfsForall") {
    assert(a.bfsForall(_.is[Tree]))
    assert(!a.bfsForall(_.is[Term]))
    assert(!a.bfsForall(_.is[Type]))
    assert(q"3".bfsForall(_.syntax == "3"))
  }

  test("dfsExists") {
    assert(a.dfsExists(_.is[Defn.Val]))
    assert(a.dfsExists(_.is[Lit]))
    assert(a.dfsExists(_.is[Pat]))
    assert(!a.dfsExists(_.is[Type.Name]))
    assert(q"val x = { 2 + 3 }".dfsExists(_.syntax == "3"))
  }

  test("bfsExists") {
    assert(a.bfsExists(_.is[Defn.Val]))
    assert(a.bfsExists(_.is[Lit]))
    assert(a.bfsExists(_.is[Pat]))
    assert(!a.bfsExists(_.is[Type.Name]))
    assert(q"val x = { 2 + 3 }".bfsExists(_.syntax == "3"))
  }

  test("dfsFind") {
    a.dfs(t => assert(a.dfsFind(_ eq t).contains(t)))
    val b: Defn.Object = q"object Foo { def bar: Any = ??? }"
    b.dfs(t => assert(a.dfsFind(_ eq t).isEmpty))
  }

  test("bfsFind") {
    a.bfs(t => assert(a.bfsFind(_ eq t).contains(t)))
    val b: Defn.Object = q"object Foo { def bar: Any = ??? }"
    b.bfs(t => assert(a.bfsFind(_ eq t).isEmpty))
  }

  test("dfsCollectFirst") {
    assert(a.dfsCollectFirst { case t: Tree => t }.contains(a))
    assert(a.dfsCollectFirst { case t: Defn.Val => t }.nonEmpty)
    assert(a.dfsCollectFirst { case t: Lit => t }.nonEmpty)
    assert(a.dfsCollectFirst { case t: Type => t }.nonEmpty)
    assert(a.dfsCollectFirst { case t: Defn.Trait => t }.isEmpty)
    assert(a.dfsCollectFirst { case t: Defn.Def => t }.isEmpty)
  }

  test("bfsCollectFirst") {
    assert(a.bfsCollectFirst { case t: Tree => t }.contains(a))
    assert(a.bfsCollectFirst { case t: Defn.Val => t }.nonEmpty)
    assert(a.bfsCollectFirst { case t: Lit => t }.nonEmpty)
    assert(a.bfsCollectFirst { case t: Type => t }.nonEmpty)
    assert(a.bfsCollectFirst { case t: Defn.Trait => t }.isEmpty)
    assert(a.bfsCollectFirst { case t: Defn.Def => t }.isEmpty)
  }
}

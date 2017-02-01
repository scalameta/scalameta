package scala.meta
package contrib
import scala.meta.testkit._

import org.scalatest.FunSuite

class EqualSuite extends FunSuite {
  val a: Stat       = "val x = 2 // foo".parse[Stat].get
  val b: Defn.Val   = q"val x = 2"
  val c: Defn.Val   = q"val x = 2"
  val d: Defn.Def   = q"def x = 2"
  val e: Defn.Class = q"class Foo { val x = 2 }"

  test("syntactic") {
    assert(!a.equal[Syntactically](b))
    assert(b.equal[Syntactically](c))
    assert(!e.contains[Syntactically](a))
    assert(e.contains[Syntactically](b))
    assert(!e.contains[Structurally](d))
    assert(Set[Syntactically[Tree]](a, b, c, d).size == 3)
  }

  test("structural") {
    assert(a.equal[Structurally](b))
    assert(b.equal[Structurally](c))
    assert(e.contains[Structurally](a))
    assert(e.contains[Structurally](b))
    assert(!e.contains[Structurally](d))
    assert(Set[Structurally[Tree]](a, b, c, d).size == 2)
  }

  test("structural property") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      // empty transformation preserve structural equality
      val a               = ast.transform { case Term.Name(x) => Term.Name(x) }
      val b               = ast.transform { case Type.Name(x) => Type.Name(x) }
      val refEqual        = a.equals(b) // should be false
      val structuralEqual = a.equal[Structurally](b) // should be true
      if (refEqual || !structuralEqual) Seq(a)
      else Nil
    }
    assert(errors.isEmpty)
  }
}

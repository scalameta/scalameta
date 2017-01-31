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
    assert(!a.equal[Syntactic](b))
    assert(b.equal[Syntactic](c))
    assert(!e.contains[Syntactic](a))
    assert(e.contains[Syntactic](b))
    assert(!e.contains[Structural](d))
    assert(Set[Syntactic[Tree]](a, b, c, d).size == 3)
  }

  test("structural") {
    assert(a.equal[Structural](b))
    assert(b.equal[Structural](c))
    assert(e.contains[Structural](a))
    assert(e.contains[Structural](b))
    assert(!e.contains[Structural](d))
    assert(Set[Structural[Tree]](a, b, c, d).size == 2)
  }

  test("structural property") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      // empty transformation preserve structural equality
      val a               = ast.transform { case Term.Name(x) => Term.Name(x) }
      val b               = ast.transform { case Type.Name(x) => Type.Name(x) }
      val refEqual        = a.equals(b) // should be false
      val structuralEqual = a.equal[Structural](b) // should be true
      if (refEqual || !structuralEqual) Seq(a)
      else Nil
    }
    assert(errors.isEmpty)
  }
}

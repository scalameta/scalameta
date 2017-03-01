package scala.meta
package contrib
import scala.meta.testkit._
import org.scalatest.FunSuite

class EqualSuite extends FunSuite {
  val a: Stat = "val x = 2 // foo".parse[Stat].get
  val b: Defn.Val = q"val x = 2"
  val c: Defn.Val = q"val x = 2"
  val d: Defn.Def = q"def x = 2"
  val e: Defn.Class = q"class Foo { val x = 2 }"

  test("syntactic") {
    assert(!a.isEqual[Syntactically](b))
    assert(b.isEqual[Syntactically](c))
    assert(!e.contains[Syntactically](a))
    assert(e.contains[Syntactically](b))
    assert(!e.contains[Structurally](d))
    assert(Set[Syntactically[Tree]](a, b, c, d).size == 3)
  }

  test("structural") {
    assertTypeError("""c.isEqual(d)""") // Defn.Val cannot be Defn.Def
    assert(a.isEqual(b))
    assert(b.isEqual(c))
    assert(e.contains(a))
    assert(e.contains(b))
    assert(!e.contains(d))
    assert(Set[Structurally[Tree]](a, b, c, d).size == 2)
  }

  test("structural property") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      // empty transformation preserve structural equality
      val termNameTransformer: PartialFunction[Tree, Tree] = { case Term.Name(x) => Term.Name(x) }
      val typeNameTransformer: PartialFunction[Tree, Tree] = { case Type.Name(x) => Type.Name(x) }

      // We need to make sure that the tree will actually be transformed. or ref equality will be true
      val transformWillChangeRef = termNameTransformer.isDefinedAt(ast) || typeNameTransformer
          .isDefinedAt(ast)

      val a = ast.transform(termNameTransformer)
      val b = ast.transform(typeNameTransformer)

      val refEqual = a.equals(b) && transformWillChangeRef // should be false
      val structuralEqual = a.isEqual(b) // should be true

      if (refEqual || !structuralEqual) Seq(a)
      else Nil
    }
    assert(errors.isEmpty)
  }
}

import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.semantic.quasiquotes._
import scala.meta.ui._

class HygieneSuite extends FunSuite {
  test("q\"List\"") {
    assert(q"List".show[Semantics] === """
      |Term.Name("List")[1]
      |[1] scala.package.List
    """.stripMargin.trim)
  }
  test("t\"List.type\"") {
    assert(t"List.type".show[Semantics] === """
      |Type.Singleton(Term.Name("List")[1])
      |[1] scala.package.List
    """.stripMargin.trim)
  }
  test("t\"List[Int]\"") {
    assert(t"List[Int]".show[Semantics] === """
      |Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
      |[1] scala.package#List
      |[2] scala#Int
    """.stripMargin.trim)
  }
  test("not yet supported: t\"List[X]\"") {
    assert(t"List[X]".show[Semantics] === """
      |Type.Apply(Type.Name("List")[0], List(Type.Name("X")[0]))
    """.stripMargin.trim)
  }
  test("not yet supported: t\"List[Int] { def head: Int }\"") {
    assert(t"List[Int] { def head: Int }".show[Semantics] === """
      |Type.Compound(List(Type.Apply(Type.Name("List")[0], List(Type.Name("Int")[0]))), List(Decl.Def(Nil, Term.Name("head")[0], Nil, Nil, Type.Name("Int")[0])))
    """.stripMargin.trim)
  }
}
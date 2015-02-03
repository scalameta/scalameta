import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.semantic.quasiquotes._
import scala.meta.ui._

class HygieneSuite extends FunSuite {
  test("q\"List\"") {
    assert(q"List".show[Semantics] === """
      |Term.Name("List")[1]
      |[1] 0::scala.package.List
    """.stripMargin.trim)
  }
  test("t\"List.type\"") {
    assert(t"List.type".show[Semantics] === """
      |Type.Singleton(Term.Name("List")[1])
      |[1] 0::scala.package.List
    """.stripMargin.trim)
  }
  test("t\"List[Int]\"") {
    assert(t"List[Int]".show[Semantics] === """
      |Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
      |[1] 0::scala.package#List
      |[2] 0::scala#Int
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
  test("equality - 1") {
    val unqualified = {
      import scala.collection.mutable.Map
      t"Map"
    }
    val qualified = t"scala.collection.mutable.Map"
    assert(unqualified == qualified)
    assert(unqualified.hashCode == qualified.hashCode)
  }
  test("equality - 2") {
    val unqualified = {
      import scala.collection.mutable.Map
      q"Map"
    }
    val qualified1 = t"scala.collection.mutable.Map"
    val qualified2 = t"scala.collection.mutable.Map.type"
    assert(unqualified != qualified1)
    assert(unqualified != qualified2)
  }
}
import org.scalatest._
import scala.meta._
import scala.meta.dialects.Scala211

class HygieneSuite extends FunSuite {
  test("q\"List\"") {
    assert(q"List".show[Semantics] === """
      |Term.Name("List")[1]
      |[1] Type.Singleton(Term.Name("package")[2])::scala.package.List
      |[2] Type.Singleton(Term.Name("scala")[3])::scala.package
      |[3] Type.Singleton(Term.Name("_root_")[4])::scala
      |[4] 0::_root_
    """.stripMargin.trim)
  }
  test("q\"scala.collection.immutable.List\"") {
    assert(q"scala.collection.immutable.List".show[Semantics] === """
      |Term.Select(Term.Select(Term.Select(Term.Name("scala")[1], Term.Name("collection")[2]), Term.Name("immutable")[3]), Term.Name("List")[4])
      |[1] Type.Singleton(Term.Name("_root_")[5])::scala
      |[2] Type.Singleton(Term.Name("scala")[1])::scala.collection
      |[3] Type.Singleton(Term.Name("collection")[2])::scala.collection.immutable
      |[4] Type.Singleton(Term.Name("immutable")[3])::scala.collection.immutable.List
      |[5] 0::_root_
    """.stripMargin.trim)
  }
  test("t\"List.type\"") {
    assert(t"List.type".show[Semantics] === """
      |Type.Singleton(Term.Name("List")[1])
      |[1] Type.Singleton(Term.Name("package")[2])::scala.package.List
      |[2] Type.Singleton(Term.Name("scala")[3])::scala.package
      |[3] Type.Singleton(Term.Name("_root_")[4])::scala
      |[4] 0::_root_
    """.stripMargin.trim)
  }
  test("t\"List[Int]\"") {
    assert(t"List[Int]".show[Semantics] === """
      |Type.Apply(Type.Name("List")[1], List(Type.Name("Int")[2]))
      |[1] Type.Singleton(Term.Name("package")[3])::scala.package#List
      |[2] Type.Singleton(Term.Name("scala")[4])::scala#Int
      |[3] Type.Singleton(Term.Name("scala")[4])::scala.package
      |[4] Type.Singleton(Term.Name("_root_")[5])::scala
      |[5] 0::_root_
    """.stripMargin.trim)
  }
  test("t\"HygieneSuite\"") {
    assert(t"HygieneSuite".show[Semantics] === """
      |Type.Name("HygieneSuite")[1]
      |[1] Type.Singleton(Term.Name("_empty_")[2])::_empty_#HygieneSuite
      |[2] Type.Singleton(Term.Name("_root_")[3])::_empty_
      |[3] 0::_root_
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
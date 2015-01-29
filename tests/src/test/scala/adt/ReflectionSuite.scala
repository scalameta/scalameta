import scala.meta.Tree
import scala.meta.internal.ast.Term.{If, Name}

class ReflectionSuite extends AdtSuite {
  import AdtReflection._

  test("root") {
    assert(symbolOf[Tree].isRoot)
  }

  test("If") {
    val iff = symbolOf[If].asLeaf
    val List(f1, f2, f3) = iff.fields
    assert(f1.toString === "field cond: scala.meta.internal.ast.Term")
    assert(f2.toString === "field thenp: scala.meta.internal.ast.Term")
    assert(f3.toString === "field elsep: scala.meta.internal.ast.Term")
    val List(a1, a2, a3) = iff.allFields
    assert(a1.toString === "field cond: scala.meta.internal.ast.Term")
    assert(a2.toString === "field thenp: scala.meta.internal.ast.Term")
    assert(a3.toString === "field elsep: scala.meta.internal.ast.Term")
  }

  test("Term.Name") {
    val iff = symbolOf[Name].asLeaf
    val List(f1) = iff.fields
    assert(f1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
    val List(a1) = iff.allFields
    assert(a1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
  }
}
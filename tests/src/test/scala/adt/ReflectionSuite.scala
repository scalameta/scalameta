import scala.meta.Tree
import scala.meta.internal.ast.Term.{If, Name}

class ReflectionSuite extends AdtSuite {
  import AdtReflection._

  // NOTE: these counts are important
  // because our AdtReflection infrastructure for @ast classes is very fragile
  // therefore we do need additional safeguards in place to prevent silent failures
  // I understand that it's inconvenient to update these numbers every time something changes
  // but please deal with that (or come up with a more effective way of testing AdtReflection)
  test("root") {
    assert(symbolOf[Tree].isRoot)
    assert(symbolOf[Tree].asRoot.allBranches.length === 65)
    assert(symbolOf[Tree].asRoot.allLeafs.length === 130)
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
    val List(a1, a2, a3) = iff.allFields
    assert(a1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
    assert(a2.toString === "field denot: scala.meta.internal.hygiene.Denotation (auxiliary)")
    assert(a3.toString === "field sigma: scala.meta.internal.hygiene.Sigma (auxiliary)")
  }
}
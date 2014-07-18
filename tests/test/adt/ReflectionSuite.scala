import scala.meta._

class ReflectionSuite extends AdtSuite {
  import AdtReflection._

  test("root") {
    assert(symbolOf[Tree].isRoot)
    assert(symbolOf[Tree].asRoot.branches.length === 19)
    assert(symbolOf[Tree].asRoot.allBranches.length === 54)
    assert(symbolOf[Tree].asRoot.leafs.length === 6)
    assert(symbolOf[Tree].asRoot.allLeafs.length === 119)
  }

  test("If") {
    val iff = symbolOf[Term.If].asLeaf
    val List(f1, f2, f3) = iff.fields
    assert(f1.toString === "field cond: scala.meta.Term")
    assert(f2.toString === "field thenp: scala.meta.Term")
    assert(f3.toString === "field elsep: scala.meta.Term")
    val List(p1, p2, p3) = iff.nontriviaFields
    assert(p1.toString === "field cond: scala.meta.Term")
    assert(p2.toString === "field thenp: scala.meta.Term")
    assert(p3.toString === "field elsep: scala.meta.Term")
    val List(a1, a2, a3, a4) = iff.allFields
    assert(a1.toString === "field cond: scala.meta.Term")
    assert(a2.toString === "field thenp: scala.meta.Term")
    assert(a3.toString === "field elsep: scala.meta.Term")
    assert(a4.toString === "field hasElsep: scala.Boolean (auto trivia)")
  }

  test("Term.Name") {
    val iff = symbolOf[Term.Name].asLeaf
    val List(f1, f2) = iff.fields
    assert(f1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
    assert(f2.toString === "field isBackquoted: scala.Boolean (manual trivia)")
    val List(p1) = iff.nontriviaFields
    assert(p1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
    val List(a1, a2) = iff.allFields
    assert(a1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
    assert(a2.toString === "field isBackquoted: scala.Boolean (manual trivia)")
  }
}
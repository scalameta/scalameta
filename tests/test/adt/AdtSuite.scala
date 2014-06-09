import org.scalatest._
import org.scalareflect.adt._
import scala.reflect.core._
import scala.reflect.runtime.universe.symbolOf

class AdtSuite extends FunSuite {
  test("root") {
    assert(symbolOf[Tree].isRoot)
    assert(symbolOf[Tree].asRoot.branches.length === 18)
    assert(symbolOf[Tree].asRoot.allBranches.length === 53)
    assert(symbolOf[Tree].asRoot.leafs.length === 6)
    assert(symbolOf[Tree].asRoot.allLeafs.length === 119)
  }

  test("If") {
    val iff = symbolOf[Term.If].asLeaf
    val List(f1, f2, f3) = iff.fields
    assert(f1.toString === "field cond: scala.reflect.core.Term")
    assert(f2.toString === "field thenp: scala.reflect.core.Term")
    assert(f3.toString === "field elsep: scala.reflect.core.Term")
    val List(p1, p2, p3) = iff.nontriviaFields
    assert(p1.toString === "field cond: scala.reflect.core.Term")
    assert(p2.toString === "field thenp: scala.reflect.core.Term")
    assert(p3.toString === "field elsep: scala.reflect.core.Term")
    val List(a1, a2, a3, a4) = iff.allFields
    assert(a1.toString === "field cond: scala.reflect.core.Term")
    assert(a2.toString === "field thenp: scala.reflect.core.Term")
    assert(a3.toString === "field elsep: scala.reflect.core.Term")
    assert(a4.toString === "field hasElsep: scala.Boolean (auto trivia)")
  }

  test("Term.Name") {
    val iff = symbolOf[Term.Name].asLeaf
    val List(f1, f2) = iff.fields
    assert(f1.toString === "field value: String @org.scalareflect.invariants.nonEmpty")
    assert(f2.toString === "field isBackquoted: scala.Boolean (manual trivia)")
    val List(p1) = iff.nontriviaFields
    assert(p1.toString === "field value: String @org.scalareflect.invariants.nonEmpty")
    val List(a1, a2) = iff.allFields
    assert(a1.toString === "field value: String @org.scalareflect.invariants.nonEmpty")
    assert(a2.toString === "field isBackquoted: scala.Boolean (manual trivia)")
  }
}
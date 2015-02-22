package scala.meta

import org.scalatest._
import org.scalameta.invariants._
import org.scalameta.{unreachable, UnreachableError}

class InvariantSuite extends FunSuite {
  test("more informative error messages") {
    val x = 2
    try require(x > 3)
    catch {
      case ex: InvariantFailedException =>
        assert(ex.getMessage === """
          |invariant failed:
          |when verifying x.>(3)
          |found that x.>(3) is false
          |where x = 2
        """.trim.stripMargin)
    }
  }

  test("secondary constructors in templates") {
    import scala.meta.internal.{ast => impl}
    val primaryCtor = impl.Ctor.Primary(Nil, impl.Ctor.Name("this"), Nil)
    val secondaryCtor = impl.Ctor.Secondary(Nil, impl.Ctor.Name("this"), List(List()), impl.Ctor.Name("this"))
    val stats = List(secondaryCtor)
    val template = impl.Template(Nil, Nil, impl.Term.Param(Nil, impl.Name.Anonymous(), None, None), Some(stats))
    impl.Defn.Class(Nil, impl.Type.Name("test"), Nil, primaryCtor, template)
    intercept[InvariantFailedException] { impl.Defn.Trait(Nil, impl.Type.Name("test"), Nil, primaryCtor, template) }
    intercept[InvariantFailedException] { impl.Defn.Object(Nil, impl.Term.Name("test"), primaryCtor, template) }
    intercept[InvariantFailedException] { impl.Pkg(impl.Term.Name("test"), stats) }
    intercept[InvariantFailedException] { impl.Pkg.Object(Nil, impl.Term.Name("test"), primaryCtor, template) }
  }

  test("even more informative error messages") {
    val y = 2
    try {
      case class C(x: Int) { require(x != 3 && debug(x, y)) }
      new C(3)
    } catch {
      case ex: InvariantFailedException =>
        assert(ex.getMessage === """
          |invariant failed:
          |when verifying C.this.x.!=(3).&&(org.scalameta.invariants.`package`.debug(C.this.x, y))
          |found that C.this.x is equal to 3
          |where C = C(3)
          |where C.this.x = 3
          |where y = 2
        """.trim.stripMargin)
    }
  }

  test("unreachable - 1") {
    try {
      unreachable
    } catch {
      case ex: UnreachableError =>
        println(ex.getMessage)
        assert(ex.getMessage === """
          |this code path should've been unreachable
        """.trim.stripMargin)
    }
  }

  test("unreachable - 2") {
    val y = 2
    try {
      case class C(x: Int) { unreachable(debug(x, y)) }
      new C(3)
    } catch {
      case ex: UnreachableError =>
        println(ex.getMessage)
        assert(ex.getMessage === """
          |this code path should've been unreachable
          |where C.this.x = 3
          |where y = 2
        """.trim.stripMargin)
    }
  }
}

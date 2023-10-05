package scala.meta.tests
package trees

import org.scalameta.invariants._
import scala.meta._
import scala.meta.dialects.Scala211

class InvariantSuite extends TreeSuiteBase {
  test("secondary constructors in templates") {
    val primaryCtor = EmptyCtor()
    val secondaryCtor = Ctor.Secondary(
      Nil,
      anon,
      List(List()),
      Init(Type.Singleton(Term.This(anon)), anon, emptyArgClause),
      Nil
    )
    val template = tpl(secondaryCtor)
    Defn.Class(Nil, pname("test"), Nil, primaryCtor, template)
    intercept[InvariantFailedException] {
      Defn.Trait(Nil, pname("test"), Nil, primaryCtor, template)
    }
    intercept[InvariantFailedException](Defn.Object(Nil, tname("test"), template))
    intercept[InvariantFailedException](Pkg.Object(Nil, tname("test"), template))
  }

  test("Lit.Float/Double") {
    intercept[NumberFormatException](Lit.Float("a"))
    intercept[NumberFormatException](Lit.Double("a"))
  }

  test("Term.Repeated") {
    import scala.meta._
    val xs = q"xs: _*"
    intercept[InvariantFailedException](q"$xs + $xs")
  }

  test("Pat.Var") {
    import scala.meta._
    val x = p"X"
    intercept[InvariantFailedException](p"case $x =>")
  }

  test("Type.ByName") {
    import scala.meta._
    val t = t"=> T"
    intercept[InvariantFailedException](t"List[$t]")
  }

  test("Type.Repeated") {
    import scala.meta._
    val t = t"T*"
    intercept[InvariantFailedException](t"List[$t]")
  }

  test("Pat.SeqWildcard") {
    import scala.meta._
    val p = p"_*"
    intercept[InvariantFailedException](p"case $p =>")
  }

  test("Type.Var") {
    import scala.meta._
    val p"$_: List[$tvar]" = p"xs: List[t]"
    assert(tvar.is[Type.Var])
    intercept[InvariantFailedException](p"x: $tvar")
    val okay1 = t"List[$tvar]"
    val okay2 = q"List[$tvar]"
    val okay3 = p"$okay2(x, y)"
  }

  test("Init") {
    val init = init"this()"
    intercept[InvariantFailedException](q"new $init")
  }

  test("Mod.Private/Protected") {
    val ref = q"foo.bar"
    intercept[InvariantFailedException](mod"private[$ref]")
    intercept[InvariantFailedException](mod"protected[$ref]")
  }

  test("empty Term.Tuple") {
    def tuple = Term.Tuple(Nil)
    interceptMessage[InvariantFailedException](
      """|invariant failed (args should be non-empty):
         |when verifying args.!=(null).&&(args.isInstanceOf[scala.meta.internal.trees.Quasi].||(args.nonEmpty))
         |found that args.isInstanceOf[scala.meta.internal.trees.Quasi] is false
         |and also args.nonEmpty is false
         |where args = List()
         |""".stripMargin.lf2nl
    )(tuple)
  }
  test("nested Term.Tuple") {
    def tuple = Term.Tuple(Term.Tuple(Lit.Unit() :: Nil) :: Nil)
    interceptMessage[InvariantFailedException](
      """|invariant failed:
         |when verifying scala.meta.internal.trees.ParentChecks.MemberTuple(args)
         |found that scala.meta.internal.trees.ParentChecks.MemberTuple(args) is false
         |where args = List((()))
         |""".stripMargin.lf2nl
    )(tuple)
  }

  test("empty Pat.Tuple") {
    def tuple = Pat.Tuple(Nil)
    interceptMessage[InvariantFailedException](
      """|invariant failed (args should be non-empty):
         |when verifying args.!=(null).&&(args.isInstanceOf[scala.meta.internal.trees.Quasi].||(args.nonEmpty))
         |found that args.isInstanceOf[scala.meta.internal.trees.Quasi] is false
         |and also args.nonEmpty is false
         |where args = List()
         |""".stripMargin.lf2nl
    )(tuple)
  }
  test("nested Pat.Tuple") {
    def tuple = Pat.Tuple(Pat.Tuple(Lit.Unit() :: Nil) :: Nil)
    interceptMessage[InvariantFailedException](
      """|invariant failed:
         |when verifying scala.meta.internal.trees.ParentChecks.MemberTuple(args)
         |found that scala.meta.internal.trees.ParentChecks.MemberTuple(args) is false
         |where args = List((()))
         |""".stripMargin.lf2nl
    )(tuple)
  }

  test("empty Type.Tuple") {
    def tuple = Type.Tuple(Nil)
    interceptMessage[InvariantFailedException](
      """|invariant failed (args should be non-empty):
         |when verifying args.!=(null).&&(args.isInstanceOf[scala.meta.internal.trees.Quasi].||(args.nonEmpty))
         |found that args.isInstanceOf[scala.meta.internal.trees.Quasi] is false
         |and also args.nonEmpty is false
         |where args = List()
         |""".stripMargin.lf2nl
    )(tuple)
  }
  test("nested Type.Tuple") {
    def tuple = Type.Tuple(Type.Tuple(Lit.Unit() :: Nil) :: Nil)
    interceptMessage[InvariantFailedException](
      """|invariant failed:
         |when verifying scala.meta.internal.trees.ParentChecks.MemberTuple(args)
         |found that scala.meta.internal.trees.ParentChecks.MemberTuple(args) is false
         |where args = List((()))
         |""".stripMargin.lf2nl
    )(tuple)
  }

}

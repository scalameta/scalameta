package scala.meta.tests.parsers.dotty

import scala.meta._

class LanguageExtensionsSuite extends BaseDottySuite {

  override protected implicit val dialect: Dialect = dialects.Scala3.withAllowTrackedParameters(true)

  test("tracked modifier for val param of class") {
    val code = """|class Vec(tracked val size: Int)""".stripMargin
    val tree = Defn.Class(
      Nil,
      Type.Name("Vec"),
      Type.ParamClause(Nil),
      Ctor.Primary(
        Nil,
        Name.Anonymous(),
        List(Term.ParamClause(List(Term.Param(
          List(Mod.Tracked(), Mod.ValParam()),
          Term.Name("size"),
          Some(Type.Name("Int")),
          None
        ))))
      ),
      Template(None, Nil, Template.Body(None, Nil), Nil)
    )
    runTestAssert[Stat](code)(tree)
  }

  test("tracked modifier for non-val param of class") {
    val code = """|class Vec(tracked size: Int)""".stripMargin
    runTestError[Stat](code, "tracked")
  }

  test("tracked modifier for a def parameter") {
    val code = """|def foo(tracked x: Int) = x""".stripMargin
    runTestError[Stat](code, "tracked")
  }

  test("tracked modifier for a def") {
    val code = """|tracked def foo(x: Int) = x""".stripMargin
    runTestError[Stat](code, "tracked")
  }
}

package scala.meta
package tests
package parsers

// Tests from https://github.com/scala/scala/pull/5245/files
class TrailingCommaSuite extends ParseSuite {
  implicit val Scala2122: Dialect = scala.meta.dialects.Scala212.copy(allowTrailingCommas = true)

  checkErrors(
    """trait ValDcl { val foo, bar, = 23 }""",
    """trait VarDcl { var foo, bar, = 23 }""",
    """trait VarDef { var foo, bar, = _ }""",
    """trait PatDef { val Foo(foo), Bar(bar), = bippy }"""
  )

  // Negative tests
  checkOKs(
    """trait ArgumentExprs1 { f(23, "bar", )(Ev0, Ev1) }""",
    """trait ArgumentExprs2 { f(23, "bar")(Ev0, Ev1, ) }""",
    """trait ArgumentExprs3 { new C(23, "bar", )(Ev0, Ev1) }""",
    """trait ArgumentExprs4 { new C(23, "bar")(Ev0, Ev1, ) }""",
    """trait SimpleExpr { (23, "bar", ) }""",
    """trait TypeArgs { def f: C[Int, String, ] }""",
    """trait TypeParamClause { type C[A, B, ] }""",
    """trait FunTypeParamClause { def f[A, B, ] }""",
    """trait SimpleType { def f: (Int, String, ) }""",
    """trait FunctionArgTypes { def f: (Int, String, ) => Boolean }""",
    """trait SimplePattern { val (foo, bar, ) = null: Any }""",
    """trait ImportSelectors { import foo.{ Ev0, Ev1, } }""",
    """trait Import { import foo.Ev0, foo.Ev1, }""",
    """trait SimpleExpr2 { (23, ) }"""
  )
  checkOKsWithSyntax(
    """trait SimpleType2 { def f: (Int, ) }""" -> "trait SimpleType2 { def f: Int }",
    """|trait Params1 {
       |  def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1
       |}""".stripMargin ->
      "trait Params1 { def f(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1) = 1 }",
    """|trait Params2 {
       |  def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1
       |}""".stripMargin ->
      "trait Params2 { def f(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1) = 1 }",
    """|trait ClassParams1 {
       |  final class C(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1)
       |  }""".stripMargin ->
      "trait ClassParams1 { final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1) }",
    """|trait ClassParams2 {
       |  final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, )
       |}""".stripMargin ->
      "trait ClassParams2 { final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1) }"
  )
  // Positive tests
  checkOKs(
    """|trait ArgumentExprs1 {
       |  def f(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1) = 1
       |  f(
       |    23,
       |    "bar",
       |  )(
       |    Ev0,
       |    Ev1,
       |  )
       |
       |  // test arg exprs in the presence of varargs
       |  def g(x: Int, y: Int*) = 1
       |  g(1,2,
       |  )
       |  g(1,List(2, 3): _*,
       |  )
       |}
       |""".stripMargin,
    """|trait ArgumentExprs2 {
       |  class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1)
       |  new C(
       |    23,
       |    "bar",
       |  )(
       |    Ev0,
       |    Ev1,
       |  )
       |}
       |""".stripMargin,
    """|trait Params {
       |  def f(
       |    foo: Int,
       |    bar: String,
       |  )(implicit
       |    ev0: Ev0,
       |    ev1: Ev1,
       |  )
       |}
       |""".stripMargin,
    """|trait ClassParams {
       |  class C(
       |    foo: Int,
       |    bar: String,
       |  )(implicit
       |    ev0: Ev0,
       |    ev1: Ev1,
       |  )
       |
       |  // test class params in the precense of varargs
       |  case class D(i: Int*,
       |  )
       |}
       |""".stripMargin,
    """|trait SimpleExpr1 {
       |  def f: (Int, String) = (
       |    23,
       |    "bar",
       |  )
       |
       |  // the Tuple1 value case, the trailing comma is ignored so the type
       |  // is Int and the value 23
       |  def g: Int = (
       |    23,
       |  )
       |}
       |""".stripMargin,
    """|trait TypeArgs {
       |  class C[A, B]
       |  def f: C[
       |    Int,
       |    String,
       |  ]
       |}
       |""".stripMargin,
    """|trait TypeParamClause {
       |  class C[
       |    A,
       |    B,
       |  ]
       |}
       |""".stripMargin,
    """|trait FunTypeParamClause {
       |  def f[
       |    A,
       |    B,
       |  ]
       |}
       |""".stripMargin,
    """|trait SimpleType {
       |  def f: (
       |    Int,
       |    String,
       |  )
       |
       |  // the Tuple1 type case, the trailing comma is ignored so the type is
       |  // Int and the value 23
       |  def g: (
       |    Int,
       |  ) = 23
       |}
       |""".stripMargin,
    """|trait FunctionArgTypes {
       |  def f: (
       |    Int,
       |    String,
       |  ) => Boolean
       |}
       |""".stripMargin,
    """|trait SimplePattern {
       |  val (
       |    foo,
       |    bar,
       |  ) = null: Any
       |
       |  // test '@' syntax in patterns
       |  Some(1) match {
       |    case Some(x @ 1,
       |    ) => x
       |  }
       |
       |  // test ': _*' syntax in patterns
       |  List(1, 2, 3) match {
       |    case List(1, 2, _ @ _*,
       |    ) => 1
       |  }
       |
       |  // test varargs in patterns
       |  val List(x, y, _*,
       |  ) = 42 :: 17 :: Nil
       |}
       |""".stripMargin,
    """|trait ImportSelectors {
       |  import foo.{
       |    Ev0,
       |    Ev1,
       |  }
       |}
       |""".stripMargin,
    """|trait Bindings {
       |  def g(f: (Int, String) => Boolean)
       |
       |  g((
       |    foo,
       |    bar,
       |  ) => true)
       |}
       |""".stripMargin,
    """|trait Comments {
       |  def f(
       |    a: String,
       |    b: String, // a comment
       |  )
       |}
       |""".stripMargin
  )
}

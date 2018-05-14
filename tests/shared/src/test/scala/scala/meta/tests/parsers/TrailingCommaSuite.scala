package scala.meta
package tests
package parsers

// Tests from https://github.com/scala/scala/pull/5245/files
class TrailingCommaSuite extends ParseSuite {
  implicit val Scala2122 = scala.meta.dialects.Scala212.copy(allowTrailingCommas = true)

  // Negative tests
  checkError("""trait ArgumentExprs1 { f(23, "bar", )(Ev0, Ev1) }""")
  checkError("""trait ArgumentExprs2 { f(23, "bar")(Ev0, Ev1, ) }""")
  checkError("""trait ArgumentExprs3 { new C(23, "bar", )(Ev0, Ev1) }""")
  checkError("""trait ArgumentExprs4 { new C(23, "bar")(Ev0, Ev1, ) }""")
  checkError("""trait SimpleExpr { (23, "bar", ) }""")
  checkError("""trait TypeArgs { def f: C[Int, String, ] }""")
  checkError("""trait TypeParamClause { type C[A, B, ] }""")
  checkError("""trait FunTypeParamClause { def f[A, B, ] }""")
  checkError("""trait SimpleType { def f: (Int, String, ) }""")
  checkError("""trait FunctionArgTypes { def f: (Int, String, ) => Boolean }""")
  checkError("""trait SimplePattern { val (foo, bar, ) = null: Any }""")
  checkError("""trait ImportSelectors { import foo.{ Ev0, Ev1, } }""")
  checkError("""trait Import { import foo.Ev0, foo.Ev1, }""")
  checkError("""trait ValDcl { val foo, bar, = 23 }""")
  checkError("""trait VarDcl { var foo, bar, = 23 }""")
  checkError("""trait VarDef { var foo, bar, = _ }""")
  checkError("""trait PatDef { val Foo(foo), Bar(bar), = bippy }""")
  checkError("""trait SimpleExpr2 { (23, ) }""")
  checkError("""trait SimpleType2 { def f: (Int, ) }""")
  checkError(
    """|trait Params1 {
       |  def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1
       |}""".stripMargin
  )
  checkError(
    """|trait Params2 {
       |  def f(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1, ) = 1
       ||}""".stripMargin
  )
  checkError(
    """|trait ClassParams1 {
       |  final class C(foo: Int, bar: String, )(implicit ev0: Ev0, ev1: Ev1)
       |  }""".stripMargin
  )
  checkError(
    """|trait ClassParams2 {
       |  final class C(foo: Int, bar: String)(implicit ev0: Ev0, ev1: Ev1, )
       |}""".stripMargin
  )
  // Positive tests
  checkOK(
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
       |""".stripMargin
  )
  checkOK(
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
       |""".stripMargin
  )
  checkOK(
    """|trait Params {
       |  def f(
       |    foo: Int,
       |    bar: String,
       |  )(implicit
       |    ev0: Ev0,
       |    ev1: Ev1,
       |  )
       |}
       |""".stripMargin
  )
  checkOK(
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
       |""".stripMargin
  )
  checkOK(
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
       |""".stripMargin
  )
  checkOK(
    """|trait TypeArgs {
       |  class C[A, B]
       |  def f: C[
       |    Int,
       |    String,
       |  ]
       |}
       |""".stripMargin
  )
  checkOK(
    """|trait TypeParamClause {
       |  class C[
       |    A,
       |    B,
       |  ]
       |}
       |""".stripMargin
  )
  checkOK(
    """|trait FunTypeParamClause {
       |  def f[
       |    A,
       |    B,
       |  ]
       |}
       |""".stripMargin
  )
  checkOK(
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
       |""".stripMargin
  )
  checkOK(
    """|trait FunctionArgTypes {
       |  def f: (
       |    Int,
       |    String,
       |  ) => Boolean
       |}
       |""".stripMargin
  )
  checkOK(
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
       |""".stripMargin
  )
  checkOK(
    """|trait ImportSelectors {
       |  import foo.{
       |    Ev0,
       |    Ev1,
       |  }
       |}
       |""".stripMargin
  )
  checkOK(
    """|trait Bindings {
       |  def g(f: (Int, String) => Boolean)
       |
       |  g((
       |    foo,
       |    bar,
       |  ) => true)
       |}
       |""".stripMargin
  )
  checkOK(
    """|trait Comments {
       |  def f(
       |    a: String,
       |    b: String, // a comment
       |  )
       |}
       |""".stripMargin
  )
}

package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

trait BaseDottySuite extends ParseSuite {

  // implicit val dialect: Dialect = scala.meta.dialects.Dotty

  final val anon = meta.Name.Anonymous()
  final val ctor = Ctor.Primary(Nil, anon, Nil)
  final def ctorp(lp: List[Term.Param] = Nil) = Ctor.Primary(Nil, anon, List(lp))
  final val slf = meta.Self(anon, None)

  final def tname(name: String): Term.Name = Term.Name(name)
  final def tpl(stats: List[Stat]): Template = Template(Nil, Nil, slf, stats)
  final def tparam(name: String, tpe: String) =
    Term.Param(Nil, Term.Name(name), Some(pname(tpe)), None)
  final def tparamUsing(name: String, tpe: String) =
    if (name.nonEmpty) Term.Param(List(Mod.Using()), Term.Name(name), Some(pname(tpe)), None)
    else Term.Param(List(Mod.Using()), anon, Some(pname(tpe)), None)

  final def pname(name: String): Type.Name = Type.Name(name)
  final def pparam(s: String): Type.Param =
    Type.Param(Nil, Type.Name(s), Nil, Type.Bounds(None, None), Nil, Nil)

  final def int(i: Int) = Lit.Int(i)
  final def init(name: String): Init = Init(pname(name), anon, Nil)

  protected def runTestAssert[T <: Tree](code: String, assertLayout: Boolean = true)(expected: T)(implicit parser: String => T) {
    import scala.meta.dialects.Dotty
    val obtained: T = parser(code)
    try {
      assertEquals(obtained, expected)

      // check bijection
      val r = scala.meta.internal.prettyprinters.TreeSyntax.reprint[T](obtained)(dialects.Dotty)
      assertEquals(parser(r.toString), expected)
      if (assertLayout) {
        assertEquals(r.toString, code)
      }
    } catch {
      case e: Throwable =>
        println(s"Generated stat: \n ${obtained.structure}")
        throw e
    }
  }

  protected def runTestError[T <: Tree](code: String, expected: String)(
      implicit parser: String => T
  ) {
    val error = intercept[ParseException] {
      val result = parser(code)
      println(s"Statement ${code} should not parse! Got result ${result.structure}")
    }
    if (!error.getMessage().contains(expected)) {
      println(s"Expected [${error.getMessage}] to contain [${expected}].")
    }
    assert(error.getMessage.contains(expected))
  }
}

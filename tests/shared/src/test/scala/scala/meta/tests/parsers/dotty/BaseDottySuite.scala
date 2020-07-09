package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

trait BaseDottySuite extends ParseSuite {

  final val anon = meta.Name.Anonymous()
  final val ctor = Ctor.Primary(Nil, anon, Nil)
  final def ctorp(lp: List[Term.Param] = Nil) = Ctor.Primary(Nil, anon, List(lp))
  final val slf = meta.Self(anon, None)

  final def tname(name: String): Term.Name = Term.Name(name)
  final def tpl(stats: List[Stat]): Template = Template(Nil, Nil, slf, stats)
  final def tparamval(name: String, tpe: String) =
    Term.Param(List(Mod.ValParam()), Term.Name(name), Some(pname(tpe)), None)
  final def tparam(name: String, tpe: String) =
    Term.Param(Nil, Term.Name(name), Some(pname(tpe)), None)
  final def tparamInline(name: String, tpe: String) =
    Term.Param(List(Mod.Inline()), Term.Name(name), Some(pname(tpe)), None)
  final def tparamUsing(name: String, tpe: String) =
    if (name.nonEmpty) Term.Param(List(Mod.Using()), Term.Name(name), Some(pname(tpe)), None)
    else Term.Param(List(Mod.Using()), anon, Some(pname(tpe)), None)

  final def pname(name: String): Type.Name = Type.Name(name)
  final def pparam(s: String): Type.Param =
    Type.Param(Nil, Type.Name(s), Nil, Type.Bounds(None, None), Nil, Nil)

  final def int(i: Int) = Lit.Int(i)
  final def init(name: String): Init = Init(pname(name), anon, Nil)

  /**
   * Check if code can be parsed to expected syntax tree.
   * @see runTestAssert(code, assertLayout)(expected)
   */
  protected def runTestAssert[T <: Tree](
      code: String
  )(expected: T)(implicit parser: String => T): Unit =
    runTestAssert(code, Some(code))(expected)(parser)

  /**
   * General method used to assert a given 'code' parses to expected tree structure and back.
   * We cannot assert trees by equality(==) that's why we check if they are identical
   * by asserting their structure representation and optionally syntax.
   * If expectedLayout is provided then we print back generated tree structure and assert
   * generated text is equal to expectedLayout (in most cases it should be the same as 'code' param
   * but sometimes formatting is a little different or for safety () are added).
   * If you are not interested in asserting layout just provide None.
   * After printing generated tree to text representation we parse it again.
   * This ensures that invariant holds: parse(code) = parse(print(parse(code)))
   * Reprint cannot be handled by `tree.syntax` because syntax is cached by default and would
   * not be reprinted but only input code would be returned.
   *
   * @param code valid scala code
   * @param assertLayout string representation of code to be printed
   * @param expected provided 'code' should parse to this tree structure
   * @param parser Function used to convert code into structured tree
   */
  protected def runTestAssert[T <: Tree](code: String, assertLayout: Option[String])(
      expected: T
  )(implicit parser: String => T): Unit = {
    import scala.meta.dialects.Dotty
    val obtained: T = parser(code)
    try {
      assertNoDiff(obtained.structure, expected.structure)

      // check bijection
      val reprintedCode =
        scala.meta.internal.prettyprinters.TreeSyntax.reprint[T](obtained)(dialects.Dotty).toString
      val obtainedAgain: T = parser(reprintedCode)
      assertNoDiff(obtainedAgain.structure, expected.structure)
      assertLayout.foreach(expectedLayout => assertNoDiff(reprintedCode, expectedLayout))
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

package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

trait BaseDottySuite extends ParseSuite {

  protected implicit val dialect: Dialect = dialects.Scala3

  implicit def parseStat(code: String, dialect: Dialect): Stat = templStat(code)(dialect)
  implicit def parseSource(code: String, dialect: Dialect): Source = source(code)(dialect)
  implicit def parseType(code: String, dialect: Dialect): Type = tpe(code)(dialect)

  final val anon = meta.Name.Anonymous()
  final val phName = meta.Name.Placeholder()
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

  final def tparamUsing(name: String, tpe: String) = {
    val nameTree = name match {
      case "" => anon
      case "_" => phName
      case _ => Term.Name(name)
    }
    Term.Param(List(Mod.Using()), nameTree, Some(pname(tpe)), None)
  }

  final def pname(name: String): Type.Name = Type.Name(name)
  final def pparam(s: String): Type.Param =
    Type.Param(Nil, Type.Name(s), Nil, noBounds, Nil, Nil)

  final val noBounds = Type.Bounds(None, None)
  final def lowBound(bound: Type) = Type.Bounds(Some(bound), None)

  final def bool(v: Boolean) = Lit.Boolean(v)
  final def int(v: Int) = Lit.Int(v)
  final def str(v: String) = Lit.String(v)
  final def init(name: String): Init = Init(pname(name), anon, Nil)

  /**
   * Check if code can be parsed to expected syntax tree.
   * @see
   *   runTestAssert(code, assertLayout)(expected)
   */
  protected def runTestAssert[T <: Tree](
      code: String
  )(expected: T)(implicit parser: (String, Dialect) => T, dialect: Dialect): Unit =
    runTestAssert(code, Some(code))(expected)(parser, dialect)

  /**
   * General method used to assert a given 'code' parses to expected tree structure and back. We
   * cannot assert trees by equality(==) that's why we check if they are identical by asserting
   * their structure representation and optionally syntax. If expectedLayout is provided then we
   * print back generated tree structure and assert generated text is equal to expectedLayout (in
   * most cases it should be the same as 'code' param but sometimes formatting is a little different
   * or for safety () are added). If you are not interested in asserting layout just provide None.
   * After printing generated tree to text representation we parse it again. This ensures that
   * invariant holds: parse(code) = parse(print(parse(code))) Reprint cannot be handled by
   * `tree.syntax` because syntax is cached by default and would not be reprinted but only input
   * code would be returned.
   *
   * @param code
   *   valid scala code
   * @param assertLayout
   *   string representation of code to be printed
   * @param expected
   *   provided 'code' should parse to this tree structure
   * @param parser
   *   Function used to convert code into structured tree
   */
  protected def runTestAssert[T <: Tree](code: String, assertLayout: Option[String])(
      expected: T
  )(implicit parser: (String, Dialect) => T, dialect: Dialect): Unit = {
    val expectedStructure = expected.structure
    val obtained: T = parser(code, dialect)
    MoreHelpers.requireNonEmptyOrigin(obtained)

    // check bijection
    val reprintedCode =
      scala.meta.internal.prettyprinters.TreeSyntax.reprint[T](obtained)(dialect).toString
    assertLayout.foreach(assertNoDiff(reprintedCode, _, s"Reprinted syntax:\n $expectedStructure"))

    assertNoDiff(obtained.structure, expectedStructure, "Generated stat")
    val obtainedAgain: T = parser(reprintedCode, dialect)
    assertNoDiff(obtainedAgain.structure, expectedStructure, s"Reprinted stat: \n${reprintedCode}")
  }

}

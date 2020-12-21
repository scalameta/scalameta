package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.tests.parsers.BasePositionSuite

class Scala3PositionSuite extends BasePositionSuite(dialects.Scala3) {

  checkPositions[Type]("A & B")
  checkPositions[Type]("A | B")
  checkPositions[Type](
    "[X] =>> (X, X)",
    """|Type.Bounds [X@@] =>> (X, X)
       |Type.Tuple (X, X)
       |""".stripMargin
  )
  checkPositions[Stat]("inline def f = 1")
  checkPositions[Stat](
    "open trait a",
    """|Ctor.Primary open trait a@@
       |Template open trait a@@
       |Self open trait a@@
       |""".stripMargin
  )

  checkPositions[Stat](
    "extension [A, B](i: A)(using a: F[A], G[B]) def isZero = i == 0",
    """|Type.Bounds extension [A@@, B](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Type.Bounds extension [A, B@@](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Term.Param a: F[A]
       |Mod.Using extension [A, B](i: A)(using @@a: F[A], G[B]) def isZero = i == 0
       |Type.Apply F[A]
       |Term.Param G[B]
       |Mod.Using extension [A, B](i: A)(using a: F[A], @@G[B]) def isZero = i == 0
       |Name.Anonymous G
       |Type.Apply G[B]
       |Defn.Def def isZero = i == 0
       |Term.ApplyInfix i == 0
       |""".stripMargin
  )

  // This tests exists to document the symmetry between positions for
  // `Mod.Implicit` (test below) and `Mod.Using` (test above).
  checkPositions[Stat](
    "def foo(implicit a: A, b: B): Unit",
    """|Term.Param a: A
       |Mod.Implicit def foo(implicit @@a: A, b: B): Unit
       |Term.Param b: B
       |Mod.Implicit def foo(implicit a: A, @@b: B): Unit
       |""".stripMargin
  )

  checkPositions[Stat](
    "enum Day[T](e: T) extends A with B { case Monday, Tuesday }",
    """|Type.Bounds enum Day[T@@](e: T) extends A with B { case Monday, Tuesday }
       |Ctor.Primary (e: T)
       |Template A with B { case Monday, Tuesday }
       |Self enum Day[T](e: T) extends A with B { @@case Monday, Tuesday }
       |Defn.RepeatedEnumCase case Monday, Tuesday
       |""".stripMargin
  )
  checkPositions[Stat](
    "class Day[T](e: T) extends A with B { val Monday = 42 }",
    """|Type.Bounds class Day[T@@](e: T) extends A with B { val Monday = 42 }
       |Ctor.Primary (e: T)
       |Template A with B { val Monday = 42 }
       |Self class Day[T](e: T) extends A with B { @@val Monday = 42 }
       |Defn.Val val Monday = 42
       |""".stripMargin
  )
  checkPositions[Stat](
    "inline given intOrd as Ord[Int] { def f(): Int = 1 }",
    """|Type.Apply Ord[Int]
       |Template { def f(): Int = 1 }
       |Self inline given intOrd as Ord[Int] { @@def f(): Int = 1 }
       |Defn.Def def f(): Int = 1
       |""".stripMargin
  )
  checkPositions[Stat](
    "export a.b",
    """|Importer a.b
       |""".stripMargin
  )
  checkPositions[Stat](
    "export A.{ b, c, d, _ }",
    """|Importer A.{ b, c, d, _ }
       |""".stripMargin
  )
  checkPositions[Stat](
    "export given a.b",
    """|Importer a.b
       |""".stripMargin
  )
  checkPositions[Stat](
    "import Instances.{ im, given Ordering[?] }",
    """|Importer Instances.{ im, given Ordering[?] }
       |Importee.Given given Ordering[?]
       |Type.Apply Ordering[?]
       |Type.Placeholder ?
       |Type.Bounds import Instances.{ im, given Ordering[?@@] }
       |""".stripMargin
  )
  checkPositions[Stat](
    "import File.given",
    """|Importer File.given
       |Importee.GivenAll given
       |""".stripMargin
  )
  checkPositions[Stat](
    "export given A.{ b, c, d, _ }",
    """|Importer A.{ b, c, d, _ }
       |""".stripMargin
  )
  checkPositions[Type]("A & B")
  checkPositions[Type]("A | B")
  checkPositions[Stat](
    """|type T = A match {
       |  case Char => String
       |  case Array[t] => t
       |}""".stripMargin,
    """|Type.Match A match {
       |  case Char => String
       |  case Array[t] => t
       |}
       |TypeCase case Char => String
       |TypeCase case Array[t] => t
       |Type.Apply Array[t]
       |""".stripMargin
  )
  checkPositions[Stat](
    """|for case a: TP <- iter if cnd do
       |  echo""".stripMargin,
    """|Enumerator.CaseGenerator case a: TP <- iter
       |Pat.Typed a: TP
       |Enumerator.Guard if cnd
       |""".stripMargin
  )
  checkPositions[Stat](
    "infix def a(param: Int) = param"
  )
  checkPositions[Stat](
    "infix type or[X, Y]",
    """|Type.Bounds infix type or[X@@, Y]
       |Type.Bounds infix type or[X, Y@@]
       |Type.Bounds infix type or[X, Y]@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "def fn: Unit = inline if cond then truep",
    """|Term.If inline if cond then truep
       |Lit.Unit def fn: Unit = inline if cond then truep@@
       |""".stripMargin
  )
  checkPositions[Stat](
    """|x match {
       |  case '{ a } => 1
       |}""".stripMargin,
    """|Case case '{ a } => 1
       |Pat.Macro '{ a }
       |Term.QuotedMacroExpr '{ a }
       |Term.Block { a }
       |""".stripMargin
  )
  checkPositions[Stat](
    "val extractor: (e: Entry, f: Other) => e.Key = extractKey",
    """|Type.Function (e: Entry, f: Other) => e.Key
       |Type.TypedParam e: Entry
       |Type.TypedParam f: Other
       |Type.Select e.Key
       |""".stripMargin
  )
  checkPositions[Stat](
    "type F0 = [T] => List[T] ?=> Option[T]",
    """|Type.PolyFunction [T] => List[T] ?=> Option[T]
       |Type.Bounds type F0 = [T@@] => List[T] ?=> Option[T]
       |Type.ContextFunction List[T] ?=> Option[T]
       |Type.Apply List[T]
       |Type.Apply Option[T]
       |""".stripMargin
  )
}

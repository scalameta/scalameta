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
    "inline given intOrd: Ord[Int] with Eq[Int] with { def f(): Int = 1 }",
    """|Template Ord[Int] with Eq[Int] with { def f(): Int = 1 }
       |Init Ord[Int]
       |Type.Apply Ord[Int]
       |Init Eq[Int]
       |Type.Apply Eq[Int]
       |Self inline given intOrd: Ord[Int] with Eq[Int] with { @@def f(): Int = 1 }
       |Defn.Def def f(): Int = 1
       |""".stripMargin
  )
  checkPositions[Stat](
    """|object A{
       |  inline given intOrd: Ord[Int]
       |}""".stripMargin,
    """|Template {
       |  inline given intOrd: Ord[Int]
       |}
       |Self   @@inline given intOrd: Ord[Int]
       |Decl.Given given intOrd: Ord[Int]
       |Type.Apply Ord[Int]
       |""".stripMargin
  )
  checkPositions[Stat](
    """|object A{
       |  given intOrd: Ord[Int] = intOrd
       |}""".stripMargin,
    """|Template {
       |  given intOrd: Ord[Int] = intOrd
       |}
       |Self   @@given intOrd: Ord[Int] = intOrd
       |Defn.GivenAlias given intOrd: Ord[Int] = intOrd
       |Type.Apply Ord[Int]
       |""".stripMargin
  )
  checkPositions[Stat](
    """|object A {
       |  export a.b
       |}""".stripMargin,
    """|Template {
       |  export a.b
       |}
       |Self   @@export a.b
       |Export export a.b
       |Importer a.b
       |""".stripMargin
  )
  checkPositions[Stat](
    "export A.{ b, c, d, _ }",
    """|Importer A.{ b, c, d, _ }
       |""".stripMargin
  )
  checkPositions[Stat](
    "export a.{given Int}",
    """|Importer a.{given Int}
       |Importee.Given given Int
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
       |Type.Bounds type T = @@A match {
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
    """|x match {
       |  case List(xs*) => 1
       |}""".stripMargin,
    """|Case case List(xs*) => 1
       |Pat.Extract List(xs*)
       |Pat.Repeated xs*
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
       |Type.Bounds type F0 = @@[T] => List[T] ?=> Option[T]
       |""".stripMargin
  )
  checkPositions[Stat](
    """|inline def g: Any = inline x match {
       |  case x: String => (x, x) 
       |  case x: Double => x
       |}""".stripMargin,
    """|Term.Match inline x match {
       |  case x: String => (x, x) 
       |  case x: Double => x
       |}
       |Case case x: String => (x, x) 
       |
       |Pat.Typed x: String
       |Term.Tuple (x, x)
       |Case case x: Double => x
       |Pat.Typed x: Double
       |""".stripMargin
  )
  checkPositions[Stat](
    "class Alpha[T] derives Gamma[T], Beta[T]",
    """|Type.Bounds class Alpha[T@@] derives Gamma[T], Beta[T]
       |Ctor.Primary class Alpha[T] @@derives Gamma[T], Beta[T]
       |Template derives Gamma[T], Beta[T]
       |Self class Alpha[T] derives Gamma[T], Beta[T]@@
       |Type.Apply Gamma[T]
       |Type.Apply Beta[T]
       |""".stripMargin
  )
  checkPositions[Stat](
    """|object O {
       |  import scala as s
       |  import a.b.C as D
       |  import a.*
       |  import a.{no as _, *}
       |  import A.b.`*`
       |  import a.b.C as _
       |}""".stripMargin,
    """|Template {
       |  import scala as s
       |  import a.b.C as D
       |  import a.*
       |  import a.{no as _, *}
       |  import A.b.`*`
       |  import a.b.C as _
       |}
       |Self   @@import scala as s
       |Import import scala as s
       |Importer scala as s
       |Importee.Rename scala as s
       |Import import a.b.C as D
       |Importer a.b.C as D
       |Term.Select a.b
       |Importee.Rename C as D
       |Import import a.*
       |Importer a.*
       |Importee.Wildcard *
       |Import import a.{no as _, *}
       |Importer a.{no as _, *}
       |Importee.Unimport no as _
       |Importee.Wildcard *
       |Import import A.b.`*`
       |Importer A.b.`*`
       |Term.Select A.b
       |Importee.Name `*`
       |Name.Indeterminate `*`
       |Import import a.b.C as _
       |Importer a.b.C as _
       |Term.Select a.b
       |Importee.Unimport C as _
       |""".stripMargin
  )

  checkPositions[Stat](
    """|object X:
       |  def a: Int =
       |    42
       |  def b: String =
       |    "b"
       |""".stripMargin,
    """|Template :
       |  def a: Int =
       |    42
       |  def b: String =
       |    "b"
       |Self   @@def a: Int =
       |Defn.Def def a: Int =
       |    42
       |Defn.Def def b: String =
       |    "b"
       |Lit.String "b"
       |""".stripMargin
  )

  checkPositions[Stat](
    """|object a:
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |       case b =>
       |         stmt1
       |         stmt2
       |       case c =>
       |     finally bar
       |""".stripMargin,
    """|Template :
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |       case b =>
       |         stmt1
       |         stmt2
       |       case c =>
       |     finally bar
       |Self    @@def foo =
       |Defn.Def def foo =
       |     try foo
       |     catch
       |       case a =>
       |       case b =>
       |         stmt1
       |         stmt2
       |       case c =>
       |     finally bar
       |Term.Try try foo
       |     catch
       |       case a =>
       |       case b =>
       |         stmt1
       |         stmt2
       |       case c =>
       |     finally bar
       |Case case a =>
       |Term.Block    @@def foo =
       |Case case b =>
       |         stmt1
       |         stmt2
       |Term.Block stmt1
       |         stmt2
       |Case case c =>
       |Term.Block    @@def foo =
       |""".stripMargin
  )
}

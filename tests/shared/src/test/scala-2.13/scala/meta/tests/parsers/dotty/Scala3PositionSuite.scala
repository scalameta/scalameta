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

}

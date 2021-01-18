package scala.meta.tests.parsers.dotty

import scala.meta._

class DerivesSuite extends BaseDottySuite {

  test("simple-derives") {
    val derivesEnum = """|enum Tree[T] derives Eq, Ordering, Show {
                         |  case Branch
                         |  case Leaf
                         |}
                         |""".stripMargin

    runTestAssert[Stat](derivesEnum)(
      Defn.Enum(
        Nil,
        Type.Name("Tree"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          Nil,
          Self(Name(""), None),
          List(
            Defn.EnumCase(Nil, Term.Name("Branch"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil),
            Defn.EnumCase(Nil, Term.Name("Leaf"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil)
          ),
          List(
            Type.Name("Eq"),
            Type.Name("Ordering"),
            Type.Name("Show")
          )
        )
      )
    )

  }

  test("extends-derives") {
    runTestAssert[Stat](
      """|enum Tree[T] extends Bee derives Eq, scala.derives.Ordering, Show {
         |  case Branch
         |  case Leaf
         |}
         |""".stripMargin
    )(
      Defn.Enum(
        Nil,
        Type.Name("Tree"),
        List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        Ctor.Primary(Nil, Name(""), Nil),
        Template(
          Nil,
          List(Init(Type.Name("Bee"), Name(""), Nil)),
          Self(Name(""), None),
          List(
            Defn.EnumCase(Nil, Term.Name("Branch"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil),
            Defn.EnumCase(Nil, Term.Name("Leaf"), Nil, Ctor.Primary(Nil, Name(""), Nil), Nil)
          ),
          List(
            Type.Name("Eq"),
            Type
              .Select(Term.Select(Term.Name("scala"), Term.Name("derives")), Type.Name("Ordering")),
            Type.Name("Show")
          )
        )
      )
    )

  }

  test("case-class-derives") {
    val derivesEnum =
      runTestAssert[Stat](
        """|case class Node(name : String) extends Tree derives Eq:
           |  def hello() = ""
           |  def bye() = ""
           |
           |""".stripMargin,
        assertLayout = Some(
          """|case class Node(name: String) extends Tree derives Eq {
             |  def hello() = ""
             |  def bye() = ""
             |}
             |""".stripMargin
        )
      )(
        Defn.Class(
          List(Mod.Case()),
          Type.Name("Node"),
          Nil,
          Ctor.Primary(
            Nil,
            Name(""),
            List(List(Term.Param(Nil, Term.Name("name"), Some(Type.Name("String")), None)))
          ),
          Template(
            Nil,
            List(Init(Type.Name("Tree"), Name(""), Nil)),
            Self(Name(""), None),
            List(
              Defn.Def(Nil, Term.Name("hello"), Nil, List(List()), None, Lit.String("")),
              Defn.Def(Nil, Term.Name("bye"), Nil, List(List()), None, Lit.String(""))
            ),
            List(
              Type.Name("Eq")
            )
          )
        )
      )

  }

  test("newline-derives") {
    val derivesEnum =
      runTestAssert[Stat](
        """|class A
           |    derives AVeryLongName1,
           |      AVeryLongName2,
           |      AVeryLongName3
           |""".stripMargin,
        assertLayout = Some(
          "class A derives AVeryLongName1, AVeryLongName2, AVeryLongName3"
        )
      )(
        Defn.Class(
          Nil,
          Type.Name("A"),
          Nil,
          Ctor.Primary(Nil, Name(""), Nil),
          Template(
            Nil,
            Nil,
            Self(Name(""), None),
            Nil,
            List(
              Type.Name("AVeryLongName1"),
              Type.Name("AVeryLongName2"),
              Type.Name("AVeryLongName3")
            )
          )
        )
      )
  }

  test("newline-derives-params") {
    val derivesEnum =
      runTestAssert[Stat](
        """|class A[T](a: Int, b: Int)
           |    derives Alpha[T],
           |      Epsilon[T] {
           |  def a = ???
           |}
           |""".stripMargin,
        assertLayout = Some(
          "class A[T](a: Int, b: Int) derives Alpha[T], Epsilon[T] { def a = ??? }"
        )
      )(
        Defn.Class(
          Nil,
          Type.Name("A"),
          List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
          Ctor.Primary(
            Nil,
            Name(""),
            List(
              List(
                Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None),
                Term.Param(Nil, Term.Name("b"), Some(Type.Name("Int")), None)
              )
            )
          ),
          Template(
            Nil,
            Nil,
            Self(Name(""), None),
            List(Defn.Def(Nil, Term.Name("a"), Nil, Nil, None, Term.Name("???"))),
            List(
              Type.Apply(Type.Name("Alpha"), List(Type.Name("T"))),
              Type.Apply(Type.Name("Epsilon"), List(Type.Name("T")))
            )
          )
        )
      )
  }
}

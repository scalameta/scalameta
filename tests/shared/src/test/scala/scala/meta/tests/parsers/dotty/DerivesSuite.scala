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
        pname("Tree"),
        List(pparam("T")),
        ctor,
        Template(
          Nil,
          Nil,
          slf,
          List(
            Defn.EnumCase(Nil, tname("Branch"), Nil, ctor, Nil),
            Defn.EnumCase(Nil, tname("Leaf"), Nil, ctor, Nil)
          ),
          List(
            pname("Eq"),
            pname("Ordering"),
            pname("Show")
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
        pname("Tree"),
        List(pparam("T")),
        ctor,
        Template(
          Nil,
          List(init("Bee")),
          slf,
          List(
            Defn.EnumCase(Nil, tname("Branch"), Nil, ctor, Nil),
            Defn.EnumCase(Nil, tname("Leaf"), Nil, ctor, Nil)
          ),
          List(
            pname("Eq"),
            Type.Select(Term.Select(tname("scala"), tname("derives")), pname("Ordering")),
            pname("Show")
          )
        )
      )
    )

  }

  test("case-class-derives") {
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
        pname("Node"),
        Nil,
        ctorp(List(tparam("name", "String"))),
        Template(
          Nil,
          List(init("Tree")),
          slf,
          List(
            Defn.Def(Nil, tname("hello"), Nil, List(List()), None, str("")),
            Defn.Def(Nil, tname("bye"), Nil, List(List()), None, str(""))
          ),
          List(
            pname("Eq")
          )
        )
      )
    )

  }

  test("newline-derives") {
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
        pname("A"),
        Nil,
        ctor,
        Template(
          Nil,
          Nil,
          slf,
          Nil,
          List(
            pname("AVeryLongName1"),
            pname("AVeryLongName2"),
            pname("AVeryLongName3")
          )
        )
      )
    )
  }

  test("newline-derives-params") {
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
        pname("A"),
        List(pparam("T")),
        ctorp(List(tparam("a", "Int"), tparam("b", "Int"))),
        Template(
          Nil,
          Nil,
          slf,
          List(Defn.Def(Nil, tname("a"), Nil, Nil, None, tname("???"))),
          List(
            Type.Apply(pname("Alpha"), List(pname("T"))),
            Type.Apply(pname("Epsilon"), List(pname("T")))
          )
        )
      )
    )
  }
}

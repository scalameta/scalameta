package scala.meta.tests.parsers.dotty

import scala.meta._

class DerivesSuite extends BaseDottySuite {

  test("simple-derives") {
    val derivesEnum = """|enum Tree[T] derives Eq, Ordering, Show {
                         |  case Branch
                         |  case Leaf
                         |}
                         |""".stripMargin

    runTestAssert[Stat](derivesEnum)(Defn.Enum(
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
        List(pname("Eq"), pname("Ordering"), pname("Show"))
      )
    ))

  }

  test("extends-derives") {
    runTestAssert[Stat](
      """|enum Tree[T] extends Bee derives Eq, scala.derives.Ordering, Show {
         |  case Branch
         |  case Leaf
         |}
         |""".stripMargin
    )(Defn.Enum(
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
    ))

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
    )(Defn.Class(
      List(Mod.Case()),
      pname("Node"),
      Nil,
      ctorp(tparam("name", "String")),
      Template(
        Nil,
        List(init("Tree")),
        slf,
        List(
          Defn.Def(Nil, tname("hello"), Nil, List(List()), None, str("")),
          Defn.Def(Nil, tname("bye"), Nil, List(List()), None, str(""))
        ),
        List(pname("Eq"))
      )
    ))

  }

  test("newline-derives") {
    runTestAssert[Stat](
      """|class A
         |    derives AVeryLongName1,
         |      AVeryLongName2,
         |      AVeryLongName3
         |""".stripMargin,
      assertLayout = Some("class A derives AVeryLongName1, AVeryLongName2, AVeryLongName3")
    )(Defn.Class(
      Nil,
      pname("A"),
      Nil,
      ctor,
      Template(
        Nil,
        Nil,
        slf,
        Nil,
        List(pname("AVeryLongName1"), pname("AVeryLongName2"), pname("AVeryLongName3"))
      )
    ))
  }

  test("newline-derives-params") {
    val layout = "class A[T](a: Int, b: Int) derives Alpha[T], Epsilon[T] { def a = ??? }"
    val tree = Defn.Class(
      Nil,
      pname("A"),
      List(pparam("T")),
      ctorp(tparam("a", "Int"), tparam("b", "Int")),
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
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    derives Alpha[T],
         |      Epsilon[T] {
         |  def a = ???
         |}
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    derives
         |      Alpha[T],
         |      Epsilon[T]
         |      {
         |  def a = ???
         |}
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

  test("newline-derives-coloneol") {
    val layout = "class A[T](a: Int, b: Int) derives Alpha[T], Epsilon[T] { def a = ??? }"
    val tree = Defn.Class(
      Nil,
      pname("A"),
      List(pparam("T")),
      ctorp(tparam("a", "Int"), tparam("b", "Int")),
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
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    derives Alpha[T],
         |      Epsilon[T]:
         |  def a = ???
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    derives
         |      Alpha[T],
         |      Epsilon[T]:
         |  def a = ???
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    derives
         |   Alpha[T],
         |   Epsilon[T]:
         |  def a = ???
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

  test("newline-extends-derives-coloneol") {
    val layout = "class A[T](a: Int, b: Int) extends Alpha[T] derives Epsilon[T] { def a = ??? }"
    val tree = Defn.Class(
      Nil,
      pname("A"),
      List(pparam("T")),
      ctorp(tparam("a", "Int"), tparam("b", "Int")),
      Template(
        Nil,
        List(Init(Type.Apply(pname("Alpha"), List(pname("T"))), anon, emptyArgClause)),
        slf,
        List(Defn.Def(Nil, tname("a"), Nil, None, tname("???"))),
        List(Type.Apply(pname("Epsilon"), List(pname("T"))))
      )
    )
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends Alpha[T]
         |    derives Epsilon[T]:
         |  def a = ???
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |      Alpha[T]
         |    derives
         |      Epsilon[T]:
         |  def a = ???
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |   Alpha[T]
         |    derives
         |   Epsilon[T]:
         |  def a = ???
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

  test("newline-extends-derives-coloneol-not-like-refine-type") {
    val layout = "class A[T](a: Int, b: Int) extends Alpha[T] derives Epsilon[T] { require(true) }"
    val tree = Defn.Class(
      Nil,
      pname("A"),
      List(pparam("T")),
      ctorp(tparam("a", "Int"), tparam("b", "Int")),
      Template(
        Nil,
        List(Init(Type.Apply(pname("Alpha"), List(pname("T"))), anon, emptyArgClause)),
        slf,
        List(Term.Apply(tname("require"), List(bool(true)))),
        List(Type.Apply(pname("Epsilon"), List(pname("T"))))
      )
    )
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends Alpha[T]
         |    derives Epsilon[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |      Alpha[T]
         |    derives
         |      Epsilon[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |   Alpha[T]
         |    derives
         |   Epsilon[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

  test("newline-extends-with-not-derives-coloneol-not-like-refine-type") {
    val layout = "class A[T](a: Int, b: Int) extends Alpha[T] with Epsilon[T] { require(true) }"
    val tree = Defn.Class(
      Nil,
      pname("A"),
      List(pparam("T")),
      ctorp(tparam("a", "Int"), tparam("b", "Int")),
      Template(
        Nil,
        List(
          Init(Type.Apply(pname("Alpha"), List(pname("T"))), anon, emptyArgClause),
          Init(Type.Apply(pname("Epsilon"), List(pname("T"))), anon, emptyArgClause)
        ),
        slf,
        List(Term.Apply(tname("require"), List(bool(true)))),
        Nil
      )
    )
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends Alpha[T]
         |    with Epsilon[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |      Alpha[T]
         |    with
         |      Epsilon[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |   Alpha[T]
         |    with
         |   Epsilon[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

  test("newline-extends-not-derives-coloneol-not-like-refine-type") {
    val layout = "class A[T](a: Int, b: Int) extends Alpha[T] { require(true) }"
    val tree = Defn.Class(
      Nil,
      pname("A"),
      List(pparam("T")),
      ctorp(tparam("a", "Int"), tparam("b", "Int")),
      Template(
        Nil,
        List(Init(Type.Apply(pname("Alpha"), List(pname("T"))), anon, emptyArgClause)),
        slf,
        List(Term.Apply(tname("require"), List(bool(true)))),
        Nil
      )
    )
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends Alpha[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |      Alpha[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat](
      """|class A[T](a: Int, b: Int)
         |    extends
         |   Alpha[T]:
         |  require(true)
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

  test("not-derives") {
    val layout = """|class A {
                    |  def derives() = ???
                    |  derives()
                    |}
                    |""".stripMargin
    val tree = Defn.Class(
      Nil,
      pname("A"),
      Nil,
      ctor,
      Template(
        Nil,
        Nil,
        slf,
        List(
          Defn.Def(Nil, tname("derives"), Nil, List(Nil), None, tname("???")),
          Term.Apply(tname("derives"), Nil)
        ),
        Nil
      )
    )
    runTestAssert[Stat]( // no newline
      """|class A:
         |  def derives() = ???
         |  derives()
         |""".stripMargin,
      Some(layout)
    )(tree)
    runTestAssert[Stat]( // with newline
      """|class A:
         |  def
         |    derives() = ???
         |  derives()
         |""".stripMargin,
      Some(layout)
    )(tree)
  }

}

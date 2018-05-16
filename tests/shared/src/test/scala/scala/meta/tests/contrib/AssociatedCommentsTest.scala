package scala.meta.tests
package contrib

import scala.meta._
import scala.meta.contrib._
import org.scalatest.FunSuite

class AssociatedCommentsTest extends FunSuite {
  test("leading") {
    val input: Source =
      """|import a.b
         |/** leading docstring */
         |object a  {
         |  // leading 2
         |  val x = 2 // trailing
         |}
         |""".stripMargin.parse[Source].get
    val comments = AssociatedComments(input)
    val defnObject = input.find(_.is[Defn.Object]).get
    val defnVal = input.find(_.is[Defn.Val]).get
    val lit = input.find(_.is[Lit]).get
    val Token.Comment(a) = comments.leading(defnVal).head
    val List(Token.Comment("* leading docstring ")) =
      comments.leading(defnObject).to[List]
    val List() = comments.trailing(defnObject).to[List]
    val List(Token.Comment(" leading 2")) =
      comments.leading(defnVal).to[List]
    val List(Token.Comment(" trailing")) =
      comments.trailing(defnVal).to[List]
    val List(Token.Comment(" trailing")) =
      comments.trailing(lit).to[List]
  }

  test("#897 first comment in file") {
    val input = """|/** Scaladoc for class A
                   |  */
                   |class A
                   |/** Scaladoc for object A
                   |  */
                   |object A""".stripMargin.parse[Source].get

    val defnClass = input.find(_.is[Defn.Class]).get
    val defnObject = input.find(_.is[Defn.Object]).get

    assertExpectations(input)(
      leading = Map(
        defnClass -> Set(
          """|/** Scaladoc for class A
             |  */""".stripMargin
        ),
        defnObject -> Set(
          """|/** Scaladoc for object A
             |  */""".stripMargin
        )
      )
    )
  }

  test("single leading comment at the beginning of a file") {
    val input =
      """|// leading
         |object A
         |""".stripMargin.parse[Source].get

    val defnObject = input.find(_.is[Defn.Object]).get

    assertExpectations(input)(
      leading = Map(
        defnObject -> Set("// leading")
      )
    )
  }

  test("multiple leading comments in a single line at the beginning of a file") {
    val input =
      """|/** leading 1 */ /* leading 2 */ // leading 3
         |class A
         |""".stripMargin.parse[Source].get

    val defnClass = input.find(_.is[Defn.Class]).get

    assertExpectations(input)(
      leading = Map(
        defnClass -> Set("/** leading 1 */", "/* leading 2 */", "// leading 3")
      )
    )
  }

  ignore("multiple leading comments in different lines at the beginning of a file") {
    val input =
      """|/** leading 1 */
         |/* leading 2 */
         |// leading 3
         |trait A
         |""".stripMargin.parse[Source].get

    val defnTrait = input.find(_.is[Defn.Trait]).get

    assertExpectations(input)(
      leading = Map(
        defnTrait -> Set("/** leading 1 */", "/* leading 2 */", "// leading 3")
      )
    )
  }

  ignore("single trailing comment at the end of a file") {
    val input =
      """|object A {
         |} // trailing
         |""".stripMargin.parse[Source].get

    val defnObject = input.find(_.is[Defn.Object]).get
    val template = input.find(_.is[Template]).get // overlaps with `object`

    assertExpectations(input)(
      trailing = Map(
        defnObject -> Set("// trailing"),
        template -> Set("// trailing")
      )
    )
  }

  test("multiple trailing comments in a single line at the end of a file") {
    val input =
      """|class A {
         |} /** trailing 1 */ /* trailing 2 */ // trailing 3
         |""".stripMargin.parse[Source].get

    val defnClass = input.find(_.is[Defn.Class]).get
    val template = input.find(_.is[Template]).get // overlaps with `class`

    val expectedComments = Set("/** trailing 1 */", "/* trailing 2 */", "// trailing 3")
    assertExpectations(input)(
      trailing = Map(
        defnClass -> expectedComments,
        template -> expectedComments
      )
    )
  }

  test("multiple trailing comments in different lines at the end of a file") {
    val input =
      """|trait A {
         |} /** trailing 1 */
         |/* trailing 2 */
         |// trailing 3
         |""".stripMargin.parse[Source].get

    val defnTrait = input.find(_.is[Defn.Trait]).get
    val template = input.find(_.is[Template]).get // overlaps with `trait`

    val expectedComments = Set("/** trailing 1 */", "/* trailing 2 */", "// trailing 3")
    assertExpectations(input)(
      trailing = Map(
        defnTrait -> expectedComments,
        template -> expectedComments
      )
    )
  }

  test("comment not associated to any tree") {
    val input =
      """|object A {
         | // foo
         |}
         |""".stripMargin.parse[Source].get

    assertExpectations(input)(
      leading = Map.empty,
      trailing = Map.empty
    )
  }

  test("tree with both leading and trailing comments") {
    val input =
      """|object A {
         | // leading
         | val x = 0 // trailing
         | var y = false
         |}
         |""".stripMargin.parse[Source].get

    val defnVal = input.find(_.is[Defn.Val]).get
    val litInt = input.find(_.is[Lit.Int]).get // overlaps with `val`

    assertExpectations(input)(
      leading = Map(
        defnVal -> Set("// leading")
      ),
      trailing = Map(
        defnVal -> Set("// trailing"),
        litInt -> Set("// trailing")
      )
    )
  }

  test("multiple comments interleaved with trees in a single line") {
    val input =
      """|object A {
         | /* comment 1 */ val /* comment 2 */ foo = /* comment 3 */ 0 /* comment 4 */
         |}
         |""".stripMargin.parse[Source].get

    val defnVal = input.find(_.is[Defn.Val]).get
    val litInt = input.find(_.is[Lit.Int]).get

    assertExpectations(input)(
      leading = Map(
        defnVal -> Set("/* comment 1 */")
      ),
      trailing = Map(
        defnVal -> Set("/* comment 4 */"),
        litInt -> Set("/* comment 4 */")
      )
    )
  }

  test("lone comment in a file") {
    val input = "// foo".parse[Source].get

    assertExpectations(input)(
      leading = Map.empty,
      trailing = Map.empty
    )
  }

  test("comment after comma should be associated to preceding tree") {
    val input =
      """|object Foo {
         | (
         |   None, // trailing
         |   Some(0)
         | )
         |}
         |""".stripMargin.parse[Source].get

    val none = input.collectFirst { case t @ Name("None") => t }.get

    assertExpectations(input)(
      trailing = Map(
        none -> Set("// trailing")
      )
    )
  }


  private def assertExpectations(input: Source)
                                (leading: Map[Tree, Set[String]] = Map.empty, trailing: Map[Tree, Set[String]] = Map.empty): Unit = {
    val associatedComments = AssociatedComments(input.tokens)
    // check expected leading comments
    for ((t, comments) <- leading) {
      assert(associatedComments.leading(t).map(_.text) === comments,
        s"actual leading comments didn't match expectation for ${t.syntax}")
    }
    // check unexpected leading comments
    input.foreach { t =>
      if (!leading.contains(t))
        assert(associatedComments.leading(t).isEmpty, s"unexpected leading comments for ${t.syntax}")
    }
    // check expected trailing comments
    for ((t, comments) <- trailing) {
      assert(associatedComments.trailing(t).map(_.text) === comments,
        s"actual trailing comments didn't match expectation for ${t.syntax}")
    }
    // check unexpected trailing comments
    input.foreach { t =>
      if (!trailing.contains(t))
        assert(associatedComments.trailing(t).isEmpty, s"unexpected trailing comments for ${t.getClass}")
    }
  }
}

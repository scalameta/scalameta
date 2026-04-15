package scala.meta.tests
package trees

import scala.meta._
import scala.meta.internal.inputs._

class TokensSuite extends TreeSuiteBase {

  implicit val dialect: Dialect = dialects.Scala211

  test("Tree.tokens: parsed, same dialect") {
    val tree = dialects.Scala211("foo + bar // baz").parse[Term].get
    assertEquals(tree.syntax, "foo + bar // baz")
    assertEquals(tree.tokens.syntax, "foo + bar // baz")
  }

  test("Tree.tokens: parsed, different dialect") {
    val tree = dialects.Scala210("foo + bar // baz").parse[Term].get
    assertEquals(tree.syntax, "foo + bar // baz" + EOL)
    assertEquals(tree.tokens.syntax, "foo + bar // baz")
  }

  test("Tree.tokens: manual") {
    val dialect = implicitly[Dialect]
    val tree = tinfix(tname("foo"), "+", tname("bar"))
    assertEquals(tree.text, "foo + bar")
    assertEquals(tree.printSyntaxFor(dialect), "foo + bar")
    assertEquals(tree.syntax, "foo + bar")
    assert(tree.origin ne trees.Origin.None)
    assertEquals(tree.origin.dialectOpt, Some(dialect))
    val tokens = tree.tokenizeFor(dialect)
    assertEquals(tree.tokens.structure, tokens.structure)
    assertEquals(tokens.syntax, "foo + bar")
    assert(tokens.forall(_.input.isInstanceOf[Input.VirtualFile]))
    val dialectTree = tree.withDialectIfRootAndNotSet
    assertEquals(dialectTree, tree)
    assertEquals(dialectTree.text, "foo + bar")
    assertEquals(dialectTree.printSyntaxFor(dialect), "foo + bar")
    assert(dialectTree.origin ne trees.Origin.None)
    val dialectTokens = dialectTree.tokens
    assertEquals(dialectTokens.syntax, "foo + bar")
    assert(dialectTokens.forall(_.input.isInstanceOf[Input.VirtualFile]))

    val parsedTree = tree.maybeParseAs[Stat].get
    assert(parsedTree eq parsedTree.maybeParse.get)
    assertEquals(parsedTree.printSyntaxFor(dialect), "foo + bar")
    assertEquals(parsedTree.syntax, "foo + bar")
    assert(parsedTree.origin.isInstanceOf[trees.Origin.Parsed])
    val parsedTokens = parsedTree.tokens
    assertEquals(parsedTokens.syntax, "foo + bar")
    assert(parsedTokens.forall(_.input.isInstanceOf[Input.VirtualFile]))
  }

  test("Tree.tokens: empty") {
    val emptyTemplate = "class C".parse[Stat].get.children(3)
    assertTree(emptyTemplate)(tplNoBody())
    assertEquals(emptyTemplate.tokens.structure, "Tokens()")
  }

  test("inline can be used as an identifier") {
    val tree = dialects.Scala211("{ val inline = 42 }").parse[Term].get
    assertEquals(tree.syntax, "{ val inline = 42 }")
  }

  test("merge") {
    """|// cl1
       |import p1.a1 // ct1
       |
       |// cl2.1
       |// cl2.2
       |import p2.a2 /* ct2.1 */ /* ct2.2 */
       |
       |
       |   import p3.a3 // ct3
       |// ct4
       |""".stripMargin.parse[Source] match {
      case x: Parsed.Error => fail(s"Failed to parse: ${x.pos.formatMessage("error", x.message)}")
      case Parsed.Success(tree) if tree.stats.lengthCompare(3) != 0 =>
        fail(s"Expected 3 stats: ${tree.structure}")
      case Parsed.Success(tree) =>
        def getTokens(stats: Tree*): IndexedSeq[Tokens] = {
          val res = IndexedSeq.newBuilder[Tokens]
          stats.foreach { s =>
            res += s.tokens
            s.begComment.foreach(res += _.tokens)
            s.endComment.foreach(res += _.tokens)
          }
          res.result()
        }
        val stats = tree.stats.toIndexedSeq
        val merged3 =
          """|// cl1
             |import p1.a1 // ct1
             |
             |// cl2.1
             |// cl2.2
             |import p2.a2 /* ct2.1 */ /* ct2.2 */
             |
             |
             |   import p3.a3 // ct3
             |""".stripMargin
        getTokens(stats: _*).permutations.foreach(x =>
          Tokens.merge(x: _*) match {
            case Seq(one) =>
              assertEquals((1, 37), (one.start, one.length))
              assertNoDiff(one.syntax, merged3)
            case x => fail(x.mkString(s"Expected one range, got ${x.length}: [\n", "\n], [\n", "\n]"))
          }
        )
        getTokens(stats(2), stats(1)).permutations.foreach { x =>
          Tokens.merge(x: _*) match {
            case Seq(one) =>
              assertEquals((12, 26), (one.start, one.length))
              assertNoDiff(
                one.syntax,
                """|// cl2.1
                   |// cl2.2
                   |import p2.a2 /* ct2.1 */ /* ct2.2 */
                   |
                   |
                   |   import p3.a3 // ct3
                   |""".stripMargin
              )
            case x => fail(x.mkString(s"Expected one range, got ${x.length}: [\n", "\n], [\n", "\n]"))
          }
        }
        getTokens(stats(0), stats(1)).permutations.foreach { x =>
          Tokens.merge(x: _*) match {
            case Seq(one) =>
              assertEquals((1, 24), (one.start, one.length))
              assertNoDiff(
                one.syntax,
                """|// cl1
                   |import p1.a1 // ct1
                   |
                   |// cl2.1
                   |// cl2.2
                   |import p2.a2 /* ct2.1 */ /* ct2.2 */
                   |""".stripMargin
              )
            case x => fail(x.mkString(s"Expected one range, got ${x.length}: [\n", "\n], [\n", "\n]"))
          }
        }
        getTokens(stats(0), stats(2)).permutations.foreach { x =>
          Tokens.merge(x: _*) match {
            case Seq(one, two) =>
              assertEquals((1, 9), (one.start, one.length))
              assertNoDiff(
                one.syntax,
                """|// cl1
                   |import p1.a1 // ct1
                   |""".stripMargin
              )
              assertEquals((31, 7), (two.start, two.length))
              assertNoDiff(
                two.syntax,
                """|import p3.a3 // ct3
                   |""".stripMargin
              )
            case x =>
              fail(x.mkString(s"Expected two ranges, got ${x.length}: [\n", "\n], [\n", "\n]"))
          }
        }
    }
  }

}

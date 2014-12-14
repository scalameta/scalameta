import org.scalatest._
import scala.meta.syntactic.show._

class ShowTokenSuite extends ParseSuite {
  // TODO: exhaustively test all literals (including double- and triple-quoted forms of strings and interpolations)
  // TODO: also test identifiers (both vanilla and backquoted forms)

  test("showCode without comments") {
    assert(tokenize("class C  {\t val x = 2}\n\n").map(_.show[Code]).mkString === "class C  {\t val x = 2}\n\n")
  }

  test("showCode with comments") {
    // TODO: uncomment me
    // assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.show[Code]).mkString === "class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n")
  }

  test("showRaw without comments") {
    assert(tokenize("class C  {\t val x = 2}\n\n").map(_.show[Raw]).mkString("\n") === """
      |class (0)
      |  (5)
      |C (6)
      |  (7)
      |  (8)
      |{ (9)
      |\t (10)
      |  (11)
      |val (12)
      |  (15)
      |x (16)
      |  (17)
      |= (18)
      |  (19)
      |2 (20)
      |} (21)
      |\n (22)
      |\n (23)
      |EOF (24)
    """.trim.stripMargin)
  }

  test("showRaw with comments") {
    // TODO: uncomment me
    // assert(tokenize("class C  /*hello world*/{\t val x = 2}\n//bye-bye world\n").map(_.show[Raw]).mkString("\n") === """
    //   |class (0)
    //   |  (5)
    //   |C (6)
    //   |  (7)
    //   |  (8)
    //   |/*hello world*/ (9)
    //   |{ (24)
    //   |\t (25)
    //   |  (26)
    //   |val (27)
    //   |  (30)
    //   |x (31)
    //   |  (32)
    //   |= (33)
    //   |  (34)
    //   |2 (35)
    //   |} (36)
    //   |\n (37)
    //   |//bye-bye world (38)
    //   |\n (53)
    //   |EOF (54)
    // """.trim.stripMargin)
  }
}
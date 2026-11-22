package scala.meta.tests.parsers

import scala.meta._
import scala.meta.tokenizers.TokenizerOptions

import scala.annotation.nowarn

class GroupedWhitespaceParseSuite extends ParseSuite {

  test("#3979: Scala 2.11") {
    implicit val dialect: Dialect = dialects.Scala211
    implicit val parseStat: String => Stat = term(_)
    @nowarn("msg=never used|unused") // runTestAssert does not read it
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = true)

    val code =
      """|for {
         |  _ <-
         |    if (a) {
         |      b
         |    } else {
         |      c
         |    }
         |
         |  _ <- d
         |} yield e
         |""".stripMargin
    val layout =
      """|for (_ <- if (a) {
         |  b
         |} else {
         |  c
         |}; _ <- d) yield e
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(patwildcard, Term.If(tname("a"), blk(tname("b")), blk(tname("c")), Nil)),
        Enumerator.Generator(patwildcard, tname("d")),
      )),
      tname("e"),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

  test("#3979: Scala 3") {
    implicit val dialect: Dialect = dialects.Scala3
    @nowarn("msg=never used|unused") // runTestAssert does not read it
    implicit def tokenizerOptions: TokenizerOptions = new TokenizerOptions(groupWhitespace = true)

    val code =
      """|for {
         |  _ <-
         |    if (a) {
         |      b
         |    } else {
         |      c
         |    }
         |
         |  _ <- d
         |} yield e
         |""".stripMargin

    val struct =
      """|BOF [0..0)
         |KwFor [0..3)
         |LeftBrace [4..5)
         |Underscore [8..9)
         |LeftArrow [10..12)
         |Indentation.Indent [12..12)
         |KwIf [17..19)
         |LeftParen [20..21)
         |Ident(a) [21..22)
         |RightParen [22..23)
         |LeftBrace [24..25)
         |Ident(b) [32..33)
         |LF [33..34)
         |RightBrace [38..39)
         |KwElse [40..44)
         |LeftBrace [45..46)
         |Ident(c) [53..54)
         |LF [54..55)
         |RightBrace [59..60)
         |Indentation.Outdent [61..61)
         |LFLF [61..62)
         |Underscore [64..65)
         |LeftArrow [66..68)
         |Ident(d) [69..70)
         |LF [70..71)
         |RightBrace [71..72)
         |KwYield [73..78)
         |Ident(e) [79..80)
         |EOF [81..81)
         |""".stripMargin.nl2lf
    assertTokenizedAsStructureLines(code, struct, classOf[Stat])

    val layout =
      """|for (_ <- if (a) {
         |  b
         |} else {
         |  c
         |}; _ <- d) yield e
         |""".stripMargin
    val tree = Term.ForYield(
      Term.EnumeratorsBlock(List(
        Enumerator.Generator(patwildcard, Term.If(tname("a"), blk(tname("b")), blk(tname("c")), Nil)),
        Enumerator.Generator(patwildcard, tname("d")),
      )),
      tname("e"),
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}

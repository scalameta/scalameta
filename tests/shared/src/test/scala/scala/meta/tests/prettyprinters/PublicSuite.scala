package scala.meta.tests
package prettyprinters

import scala.meta._

import java.io._
import java.nio.charset.Charset

class PublicSuite extends TreeSuiteBase {

  test("scala.meta.Tree.toString (manual)") {
    val tree = Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
    assertEquals(tree.toString, "foo + bar")
  }

  test("scala.meta.Tree.structure (manual)") {
    val tree = Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
    assertTree(tree)(Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar"))))
  }

  test("scala.meta.Tree.syntax") {
    val tree = Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
    assertWithOriginalSyntax(tree, "foo + bar", "foo + bar")
  }

  test("scala.meta.Tree.toString (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assertEquals(tree.toString, "foo + bar // baz")
  }

  test("scala.meta.Tree.structure (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assertTree(tree)(Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar"))))
  }

  test("scala.meta.Tree.syntax (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assertWithOriginalSyntax(tree, "foo + bar // baz", "foo + bar")
  }

  test("scala.meta.Tree.toString (quasiquotes)") {
    val tree = q"foo + bar // baz"
    assertEquals(tree.toString, "foo + bar // baz")
  }

  test("scala.meta.Tree.structure (quasiquoted)") {
    val tree = q"foo + bar // baz"
    assertTree(tree)(Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar"))))
  }

  test("scala.meta.Tree.syntax (quasiquoted)") {
    val tree = q"foo + bar // baz"
    assertWithOriginalSyntax(tree, "foo + bar // baz", "foo + bar")
  }

  test("scala.meta.dialects.Scala3.toString") {
    assertNoDiff(scala.meta.dialects.Scala3.toString, "Scala36")
  }

  test("scala.meta.dialects.Scala30.toString") {
    assertNoDiff(scala.meta.dialects.Scala30.toString, "Scala30")
  }

  test("scala.meta.dialects.Scala31.toString") {
    assertNoDiff(scala.meta.dialects.Scala31.toString, "Scala32")
  }

  test("scala.meta.dialects.Scala32.toString") {
    assertNoDiff(scala.meta.dialects.Scala32.toString, "Scala32")
  }

  test("scala.meta.dialects.Scala33.toString") {
    assertNoDiff(scala.meta.dialects.Scala33.toString, "Scala33")
  }

  test("scala.meta.dialects.Scala34.toString") {
    assertNoDiff(scala.meta.dialects.Scala34.toString, "Scala34")
  }

  test("scala.meta.dialects.Scala35.toString") {
    assertNoDiff(scala.meta.dialects.Scala35.toString, "Scala35")
  }

  test("scala.meta.dialects.Scala36.toString") {
    assertNoDiff(scala.meta.dialects.Scala36.toString, "Scala36")
  }

  test("scala.meta.dialects.Scala3Future.toString") {
    assertNoDiff(scala.meta.dialects.Scala3Future.toString.substring(0, 6), "Scala3")
  }

  test("scala.meta.dialects.Dotty") {
    // NOTE(olafur): `Dotty` and `Scala3` are identical so it's expected that
    // `toString` returns "Scala3" instead of "Dotty".
    assertEquals(scala.meta.dialects.Dotty, scala.meta.dialects.Scala3)
  }

  test("scala.meta.dialects.Sbt0136.toString") {
    assertNoDiff(scala.meta.dialects.Sbt0136.toString, "Sbt0136")
  }

  test("scala.meta.dialects.Sbt0137.toString") {
    assertNoDiff(scala.meta.dialects.Sbt0137.toString, "Sbt0137")
  }

  test("scala.meta.dialects.Sbt.toString") {
    assertNoDiff(scala.meta.dialects.Sbt.toString, "Sbt1")
  }

  test("scala.meta.dialects.Sbt1.toString") {
    assertNoDiff(scala.meta.dialects.Sbt1.toString, "Sbt1")
  }

  test("scala.meta.dialects.Scala210.toString") {
    assertNoDiff(scala.meta.dialects.Scala210.toString, "Scala210")
  }

  test("scala.meta.dialects.Scala211.toString") {
    assertNoDiff(scala.meta.dialects.Scala211.toString, "Scala211")
  }

  test("scala.meta.dialects.Scala212.toString") {
    assertNoDiff(scala.meta.dialects.Scala212.toString, "Scala212")
  }

  test("scala.meta.dialects.Scala212Source3.toString") {
    assertNoDiff(scala.meta.dialects.Scala212Source3.toString, "Scala212Source3")
  }

  test("scala.meta.dialects.Scala213.toString") {
    assertNoDiff(scala.meta.dialects.Scala213.toString, "Scala213")
  }

  test("scala.meta.dialects.Scala213Source3.toString") {
    assertNoDiff(scala.meta.dialects.Scala213Source3.toString, "Scala213Source3")
  }

  test("scala.meta.dialects.Scala.toString") {
    assertNoDiff(scala.meta.dialects.Scala.toString, "Scala213")
  }

  test("scala.meta.dialects.Typelevel211.toString") {
    assertNoDiff(scala.meta.dialects.Typelevel211.toString, "Typelevel211")
  }

  test("scala.meta.dialects.Typelevel212.toString") {
    assertNoDiff(scala.meta.dialects.Typelevel212.toString, "Typelevel212")
  }

  test("scala.meta.dialects.Paradise211.toString") {
    assertNoDiff(scala.meta.dialects.Paradise211.toString, "Paradise211")
  }

  test("scala.meta.dialects.Paradise212.toString") {
    assertNoDiff(scala.meta.dialects.Paradise212.toString, "Paradise212")
  }

  test("scala.meta.dialects.ParadiseTypelevel211.toString") {
    assertNoDiff(scala.meta.dialects.ParadiseTypelevel211.toString, "ParadiseTypelevel211")
  }

  test("scala.meta.dialects.ParadiseTypelevel212.toString") {
    assertNoDiff(scala.meta.dialects.ParadiseTypelevel212.toString, "ParadiseTypelevel212")
  }

  test("scala.meta.inputs.Input.None.toString")(assertEquals(Input.None.toString, "Input.None"))

  test("scala.meta.inputs.Input.File.toString") {
    val path = RelativePath("hello.scala").toAbsolute
    val syntax = path.syntax
    val input1 = Input.File(path, Charset.forName("latin1"))
    val input2 = Input.File(path, Charset.forName("UTF-8"))
    input1 match { case _: Input.File => }
    input2 match { case _: Input.File => }
    assertEquals(
      input1.toString,
      s"""Input.File(new File("$syntax"), Charset.forName("ISO-8859-1"))"""
    )
    assertEquals(input2.toString, s"""Input.File(new File("$syntax"), Charset.forName("UTF-8"))""")
  }

  test("scala.meta.inputs.Input.Slice.toString") {
    val input = Input.Slice(Input.String("foo"), 0, 2)
    input match { case _: Input.Slice => }
    assertEquals(input.toString, """Input.Slice(Input.String("foo"), 0, 2)""")
  }

  test("scala.meta.inputs.Input.Stream.toString") {
    val latin1 = Charset.forName("latin1")
    val stream = new ByteArrayInputStream("Привет(мир!)".getBytes(latin1))
    val input1 = Input.Stream(stream, latin1)
    val input2 = Input.Stream(stream, Charset.forName("UTF-8"))
    input1 match { case _: Input.Stream => }
    input2 match { case _: Input.Stream => }
    assertEquals(input1.toString, """Input.Stream(<stream>, Charset.forName("ISO-8859-1"))""")
    assertEquals(input2.toString, """Input.Stream(<stream>, Charset.forName("UTF-8"))""")
  }

  test("scala.meta.inputs.Input.String.toString") {
    val input = Input.String("foo")
    input match { case _: Input.String => }
    assertEquals(input.toString, """Input.String("foo")""")
  }

  test("scala.meta.inputs.Input.VirtualFile.toString") {
    val input = Input.VirtualFile("foo.scala", "foo")
    input match { case _: Input.VirtualFile => }
    assertEquals(input.toString, s"""Input.VirtualFile("foo.scala", "foo")""")
  }

  test("scala.meta.inputs.Input.Ammonite.toString") {
    val input = Input.Ammonite(Input.None)
    input match { case _: Input.Ammonite => }
    assertEquals(input.toString, s"""Input.Ammonite(Input.None)""")
  }

  test("scala.meta.inputs.Position.None.toString") {
    assertEquals(Position.None.toString, "Position.None")
  }

  test("scala.meta.inputs.Position.Range.toString") {
    val Term.ApplyInfix(lhs, _, _, _) = "foo + bar".parse[Term].get
    lhs.pos match { case _: Position.Range => ; case _ => }
    assertEquals(lhs.pos.toString, """[0..3) in Input.String("foo + bar")""")
  }

  test("scala.meta.parsers.ParseException.toString") {
    intercept[ParseException] {
      try "foo + class".parse[Term].get
      catch {
        case ex: ParseException =>
          assertEquals(
            ex.toString,
            """|<input>:1: error: `end of file` expected but `class` found
               |foo + class
               |      ^""".stripMargin.lf2nl
          )
          throw ex
      }
    }
  }

  test("scala.meta.parsers.Parsed.Error.toString") {
    val parsed = "foo + class".parse[Term]
    parsed match { case _: Parsed.Error => ; case _ => }
    assertEquals(
      parsed.toString,
      """|<input>:1: error: `end of file` expected but `class` found
         |foo + class
         |      ^""".stripMargin.lf2nl
    )
  }

  test("scala.meta.parsers.Parsed.Success.toString") {
    val parsed = "foo + bar".parse[Term]
    parsed match { case _: Parsed.Success[_] => ; case _ => }
    assertEquals(parsed.toString, "foo + bar")
  }

  test("scala.meta.tokenizers.Tokenized.Success.toString") {
    val tokenized = "foo + bar".tokenize
    tokenized match { case _: Tokenized.Success => ; case _ => }
    assertEquals(tokenized.toString, "foo + bar")
  }

  test("scala.meta.tokens.Token.toString") {
    val token = "foo + bar".tokenize.get(1)
    assertEquals(token.toString, "foo")
  }

  test("scala.meta.tokens.Token.structure") {
    val token = "foo + bar".tokenize.get(1)
    assertEquals(token.structure, "Ident(foo) [0..3)")
  }

  test("scala.meta.tokens.Token.syntax") {
    val token = "foo + bar".tokenize.get(1)
    assertEquals(token.syntax, "foo")
  }

  test("scala.meta.tokens.Tokens.toString") {
    val tokens = "foo + bar".tokenize.get
    assertEquals(tokens.toString, "foo + bar")
  }

  test("scala.meta.tokens.Tokens.structure") {
    val tokens = "foo + bar".tokenize.get
    assertEquals(
      tokens.structure,
      "Tokens(BOF [0..0), Ident(foo) [0..3), Space [3..4), Ident(+) [4..5), Space [5..6), Ident(bar) [6..9), EOF [9..9))"
    )
  }

  test("scala.meta.tokens.Tokens.syntax") {
    val tokens = "foo + bar".tokenize.get
    assertEquals(tokens.syntax, "foo + bar")
  }

  test("scala.meta.tokens.StringExtensions") {
    import scala.meta.tokens.StringExtensions
    assert(!"foo".isBackquoted)
    assert(!"`foo".isBackquoted)
    assert(!"foo`".isBackquoted)
    assert("`".isBackquoted)
    assert("`foo`".isBackquoted)

    assert(!"foo".isIdentSymbolicInfixOperator)
    assert("`foo`".isIdentSymbolicInfixOperator)
    assert("foo_+".isIdentSymbolicInfixOperator)
    assert("+".isIdentSymbolicInfixOperator)
  }

}

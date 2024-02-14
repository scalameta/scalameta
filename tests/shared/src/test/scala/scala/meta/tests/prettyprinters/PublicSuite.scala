package scala.meta.tests
package prettyprinters

import java.io._
import java.nio.charset.Charset
import scala.meta._
import org.scalameta.internal.ScalaCompat.EOL

class PublicSuite extends TreeSuiteBase {
  test("scala.meta.Dialect.toString") {
    // covered below
  }

  test("scala.meta.Tree.toString (manual)") {
    val tree = Term.ApplyInfix(tname("foo"), tname("+"), Nil, List(tname("bar")))
    assert(tree.toString == "foo + bar")
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
    assert(tree.toString == "foo + bar // baz")
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

  test("scala.meta.classifiers.Classifiable.toString") {
    // n/a
  }

  test("scala.meta.classifiers.Classifier.toString") {
    // n/a
  }

  test("scala.meta.cli.Metac.toString") {
    // n/a
  }

  test("scala.meta.cli.Metacp.toString") {
    // n/a
  }

  test("scala.meta.cli.Metai.toString") {
    // n/a
  }

  test("scala.meta.cli.Metap.toString") {
    // n/a
  }

  test("scala.meta.cli.Reporter.toString") {
    // n/a
  }

  test("scala.meta.common.Convert.toString") {
    // n/a
  }

  test("scala.meta.common.Optional.toString") {
    // n/a
  }

  test("scala.meta.dialects.Scala3.toString") {
    assertNoDiff(scala.meta.dialects.Scala3.toString, "Scala33")
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

  test("scala.meta.inputs.InputException") {}

  test("scala.meta.inputs.Input.toString") {
    // covered below
  }

  test("scala.meta.dialects.AllowEverything.toString") {
    // Satisfy surface suite.
  }

  test("scala.meta.Member.Case.toString") {
    // Satisfy surface suite.
  }

  test("scala.meta.inputs.Input.None.toString") {
    assert(Input.None.toString == "Input.None")
  }

  test("scala.meta.inputs.Input.File.toString") {
    val path = RelativePath("hello.scala").toAbsolute
    val syntax = path.syntax
    val input1 = Input.File(path, Charset.forName("latin1"))
    val input2 = Input.File(path, Charset.forName("UTF-8"))
    input1 match { case _: Input.File => }
    input2 match { case _: Input.File => }
    assert(input1.toString == s"""Input.File(new File("$syntax"), Charset.forName("ISO-8859-1"))""")
    assert(input2.toString == s"""Input.File(new File("$syntax"), Charset.forName("UTF-8"))""")
  }

  test("scala.meta.inputs.Input.Slice.toString") {
    val input = Input.Slice(Input.String("foo"), 0, 2)
    input match { case _: Input.Slice => }
    assert(input.toString == """Input.Slice(Input.String("foo"), 0, 2)""")
  }

  test("scala.meta.inputs.Input.Stream.toString") {
    val latin1 = Charset.forName("latin1")
    val stream = new ByteArrayInputStream("Привет(мир!)".getBytes(latin1))
    val input1 = Input.Stream(stream, latin1)
    val input2 = Input.Stream(stream, Charset.forName("UTF-8"))
    input1 match { case _: Input.Stream => }
    input2 match { case _: Input.Stream => }
    assert(input1.toString == """Input.Stream(<stream>, Charset.forName("ISO-8859-1"))""")
    assert(input2.toString == """Input.Stream(<stream>, Charset.forName("UTF-8"))""")
  }

  test("scala.meta.inputs.Input.String.toString") {
    val input = Input.String("foo")
    input match { case _: Input.String => }
    assert(input.toString == """Input.String("foo")""")
  }

  test("scala.meta.inputs.Input.VirtualFile.toString") {
    val input = Input.VirtualFile("foo.scala", "foo")
    input match { case _: Input.VirtualFile => }
    assert(input.toString == s"""Input.VirtualFile("foo.scala", "foo")""")
  }

  test("scala.meta.inputs.Input.Ammonite.toString") {
    val input = Input.Ammonite(Input.None)
    input match { case _: Input.Ammonite => }
    assert(input.toString == s"""Input.Ammonite(Input.None)""")
  }

  test("scala.meta.inputs.Position.toString") {
    // covered below
  }

  test("scala.meta.inputs.Position.None.toString") {
    assert(Position.None.toString == "Position.None")
  }

  test("scala.meta.inputs.Position.Range.toString") {
    val Term.ApplyInfix(lhs, _, _, _) = "foo + bar".parse[Term].get
    lhs.pos match { case _: Position.Range => ; case _ => }
    assert(lhs.pos.toString == """[0..3) in Input.String("foo + bar")""")
  }

  test("scala.meta.io.AbsolutePath.toString") {
    // NOTE: come up with a platform-independent test
  }

  test("scala.meta.io.Classpath.toString") {
    // NOTE: come up with a platform-independent test
  }

  test("scala.meta.io.RelativePath.toString") {
    // NOTE: come up with a platform-independent test
  }

  val untestedClasses = List(
    "scala.meta.metap.Format.Proto",
    "scala.meta.metac.Settings",
    "scala.meta.metap.Settings",
    "scala.meta.metacp.Result",
    "scala.meta.metap.Format.Compact",
    "scala.meta.tokens.Token.Indentation",
    "scala.meta.metap.Format",
    "scala.meta.metacp.Settings",
    "scala.meta.metap.Format.Detailed",
    "scala.meta.tokens.Token.Unquote",
    "scala.meta.tokens.Token.LFLF",
    "scala.meta.tokens.Token.Ellipsis"
  )
  untestedClasses.foreach { name =>
    test(name + ".toString") {
      // n/a
    }
  }

  test("scala.meta.parsers.ParseException.toString") {
    intercept[ParseException] {
      try "foo + class".parse[Term].get
      catch {
        case ex: ParseException =>
          assert(ex.toString == """
            |<input>:1: error: end of file expected but class found
            |foo + class
            |      ^
          """.trim.stripMargin.split('\n').mkString(EOL))
          throw ex
      }
    }
  }

  test("scala.meta.parsers.Parsed.toString") {
    // covered below
  }

  test("scala.meta.parsers.Parsed.Error.toString") {
    val parsed = "foo + class".parse[Term]
    parsed match { case _: Parsed.Error => ; case _ => }
    assert(parsed.toString == """
      |<input>:1: error: end of file expected but class found
      |foo + class
      |      ^
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("scala.meta.parsers.Parsed.Success.toString") {
    val parsed = "foo + bar".parse[Term]
    parsed match { case _: Parsed.Success[_] => ; case _ => }
    assert(parsed.toString == "foo + bar")
  }

  test("scala.meta.prettyprinters.Show.toString") {
    // n/a
  }

  test("scala.meta.prettyprinters.Structure.toString") {
    // n/a
  }

  test("scala.meta.prettyprinters.Syntax.toString") {
    // n/a
  }

  test("scala.meta.quasiquotes.Lift.toString") {
    // n/a
  }

  test("scala.meta.quasiquotes.Unlift.toString") {
    // n/a
  }

  test("scala.meta.tokenizers.Tokenize.toString") {
    // n/a
  }

  test("scala.meta.tokenizers.TokenizeException.toString") {
    interceptMessage[TokenizeException](
      """|<input>:1: error: unclosed string literal
         |"c
         |^""".stripMargin.replace("\n", EOL)
    )(""""c""".tokenize.get)
  }

  test("scala.meta.tokenizers.Tokenized.Error.toString") {
    """"c""".tokenize match {
      case x: Tokenized.Error =>
        assertEquals(
          x.toString,
          """|<input>:1: error: unclosed string literal
             |"c
             |^""".stripMargin.replace("\n", EOL)
        )
      case x => fail(s"tokenized is not an error: $x")
    }
  }

  test("scala.meta.tokenizers.Tokenized.Success.toString") {
    val tokenized = "foo + bar".tokenize
    tokenized match { case _: Tokenized.Success => ; case _ => }
    assert(tokenized.toString == "foo + bar")
  }

  test("scala.meta.tokens.Token.toString") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.toString == "foo")
  }

  test("scala.meta.tokens.Token.structure") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.structure == "foo [0..3)")
  }

  test("scala.meta.tokens.Token.syntax") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.syntax == "foo")
  }

  test("scala.meta.tokens.Tokens.toString") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.toString == "foo + bar")
  }

  test("scala.meta.tokens.Tokens.structure") {
    val tokens = "foo + bar".tokenize.get
    assert(
      tokens.structure == "Tokens(BOF [0..0), foo [0..3),   [3..4), + [4..5),   [5..6), bar [6..9), EOF [9..9))"
    )
  }

  test("scala.meta.tokens.Tokens.syntax") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.syntax == "foo + bar")
  }

  test("scala.meta.tokens.Token.Interpolation.toString") {
    // n/a
  }

  test("scala.meta.tokens.Token.Xml.toString") {
    // n/a
  }

  test("scala.meta.transversers.SimpleTraverser.toString") {
    // n/a
  }

  test("scala.meta.transversers.Transformer.toString") {
    // n/a
  }

  test("scala.meta.transversers.Traverser.toString") {
    // n/a
  }

  test("scala.meta.XtensionDialectTokenSyntax") {}
  test("scala.meta.XtensionDialectApply") {}
  test("scala.meta.XtensionDialectTreeSyntax") {}
  test("scala.meta.XtensionDialectTokensSyntax") {}
  test("scala.meta.XtensionTree") {}
  test("scala.meta.XtensionTreeT") {}

  test("scala.meta.trees.Origin") {}
  test("scala.meta.trees.Origin.DialectOnly") {}
  test("scala.meta.trees.Origin.None") {}
  test("scala.meta.trees.Origin.Parsed") {}
  test("scala.meta.trees.Origin.ParsedSource") {}

  test("scala.meta.trees.Error") {}
  test("scala.meta.trees.Error.MissingDialectException") {}

}

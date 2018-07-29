package scala.meta.tests
package prettyprinters

import java.io._
import java.nio.charset.Charset
import org.scalatest._
import scala.meta._
import compat.Platform.EOL


class PublicSuite extends FunSuite {
  test("scala.meta.Dialect.toString") {
    // covered below
  }

  test("scala.meta.Tree.toString (manual)") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.toString === "foo + bar")
  }

  test("scala.meta.Tree.structure (manual)") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.structure === """Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))""")
  }

  test("scala.meta.Tree.syntax") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.syntax === "foo + bar")
  }

  test("scala.meta.Tree.toString (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assert(tree.toString === "foo + bar // baz")
  }

  test("scala.meta.Tree.structure (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assert(tree.structure === """Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))""")
  }

  test("scala.meta.Tree.syntax (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assert(tree.syntax === "foo + bar // baz")
  }

  test("scala.meta.Tree.toString (quasiquotes)") {
    val tree = q"foo + bar // baz"
    assert(tree.toString === "foo + bar")
  }

  test("scala.meta.Tree.structure (quasiquoted)") {
    val tree = q"foo + bar // baz"
    assert(tree.structure === """Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))""")
  }

  test("scala.meta.Tree.syntax (quasiquoted)") {
    val tree = q"foo + bar // baz"
    assert(tree.syntax === "foo + bar")
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

  test("scala.meta.dialects.Dotty.toString") {
    assert(scala.meta.dialects.Dotty.toString === "Dotty")
  }

  test("scala.meta.dialects.Sbt0136.toString") {
    assert(scala.meta.dialects.Sbt0136.toString === "Sbt0136")
  }

  test("scala.meta.dialects.Sbt0137.toString") {
    assert(scala.meta.dialects.Sbt0137.toString === "Sbt0137")
  }

  test("scala.meta.dialects.Sbt.toString") {
    assert(scala.meta.dialects.Sbt.toString === "Sbt1")
  }

  test("scala.meta.dialects.Sbt1.toString") {
    assert(scala.meta.dialects.Sbt1.toString === "Sbt1")
  }

  test("scala.meta.dialects.Scala210.toString") {
    assert(scala.meta.dialects.Scala210.toString === "Scala210")
  }

  test("scala.meta.dialects.Scala211.toString") {
    assert(scala.meta.dialects.Scala211.toString === "Scala211")
  }

  test("scala.meta.dialects.Scala212.toString") {
    assert(scala.meta.dialects.Scala212.toString === "Scala212")
  }

  test("scala.meta.dialects.Scala.toString") {
    assert(scala.meta.dialects.Scala.toString === "Scala212")
  }

  test("scala.meta.dialects.Typelevel211.toString") {
    assert(scala.meta.dialects.Typelevel211.toString === "Typelevel211")
  }

  test("scala.meta.dialects.Typelevel212.toString") {
    assert(scala.meta.dialects.Typelevel212.toString === "Typelevel212")
  }

  test("scala.meta.dialects.Paradise211.toString") {
    assert(scala.meta.dialects.Paradise211.toString === "Paradise211")
  }

  test("scala.meta.dialects.Paradise212.toString") {
    assert(scala.meta.dialects.Paradise212.toString === "Paradise212")
  }

  test("scala.meta.dialects.ParadiseTypelevel211.toString") {
    assert(scala.meta.dialects.ParadiseTypelevel211.toString === "ParadiseTypelevel211")
  }

  test("scala.meta.dialects.ParadiseTypelevel212.toString") {
    assert(scala.meta.dialects.ParadiseTypelevel212.toString === "ParadiseTypelevel212")
  }

  test("scala.meta.inputs.Input.toString") {
    // covered below
  }

  test("scala.meta.dialects.AllowEverything.toString") {
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

  test("scala.meta.inputs.Position.toString") {
    // covered below
  }

  test("scala.meta.inputs.Position.None.toString") {
    assert(Position.None.toString == "Position.None")
  }

  test("scala.meta.inputs.Position.Range.toString") {
    val Term.ApplyInfix(lhs, _, _, _) = "foo + bar".parse[Term].get
    lhs.pos match { case _: Position.Range =>; case _ => }
    assert(lhs.pos.toString === """[0..3) in Input.String("foo + bar")""")
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

  test("scala.meta.metac.Settings.toString") {
    // n/a
  }

  test("scala.meta.metacp.Result.toString") {
    // n/a
  }

  test("scala.meta.metacp.Settings.toString") {
    // n/a
  }

  test("scala.meta.metai.Result.toString") {
    // n/a
  }

  test("scala.meta.metai.Settings.toString") {
    // n/a
  }

  test("scala.meta.metap.Format.toString") {
    // n/a
  }

  test("scala.meta.metap.Format.Pretty.toString") {
    // n/a
  }

  test("scala.meta.metap.Format.Compact.toString") {
    // n/a
  }

  test("scala.meta.metap.Format.Detailed.toString") {
    // n/a
  }

  test("scala.meta.metap.Format.Proto.toString") {
    // n/a
  }

  test("scala.meta.metap.Settings.toString") {
    // n/a
  }

  test("scala.meta.parsers.Parse.toString") {
    // n/a
  }

  test("scala.meta.parsers.ParseException.toString") {
    intercept[ParseException] {
      try "foo + class".parse[Term].get
      catch {
        case ex: ParseException =>
          assert(ex.toString === """
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
    parsed match { case _: Parsed.Error =>; case _ => }
    assert(parsed.toString === """
      |<input>:1: error: end of file expected but class found
      |foo + class
      |      ^
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("scala.meta.parsers.Parsed.Success.toString") {
    val parsed = "foo + bar".parse[Term]
    parsed match { case _: Parsed.Success[_] =>; case _ => }
    assert(parsed.toString === "foo + bar")
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
    intercept[TokenizeException] {
      try """"c""".tokenize.get
      catch {
        case ex: TokenizeException =>
          assert(ex.toString === """
            |<input>:1: error: unclosed string literal
            |"c
            |^
          """.trim.stripMargin.split('\n').mkString(EOL))
          throw ex
      }
    }
  }

  test("scala.meta.tokenizers.Tokenized.Error.toString") {
    val tokenized = """"c""".tokenize
    tokenized match { case _: Tokenized.Error =>; case _ => }
    assert(tokenized.toString === """
      |<input>:1: error: unclosed string literal
      |"c
      |^
    """.trim.stripMargin.split('\n').mkString(EOL))
  }

  test("scala.meta.tokenizers.Tokenized.Success.toString") {
    val tokenized = "foo + bar".tokenize
    tokenized match { case _: Tokenized.Success =>; case _ => }
    assert(tokenized.toString === "foo + bar")
  }

  test("scala.meta.tokens.Token.toString") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.toString === "foo")
  }

  test("scala.meta.tokens.Token.structure") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.structure === "foo [0..3)")
  }

  test("scala.meta.tokens.Token.syntax") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.syntax === "foo")
  }

  test("scala.meta.tokens.Tokens.toString") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.toString === "foo + bar")
  }

  test("scala.meta.tokens.Tokens.structure") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.structure === "Tokens(BOF [0..0), foo [0..3),   [3..4), + [4..5),   [5..6), bar [6..9), EOF [9..9))")
  }

  test("scala.meta.tokens.Tokens.syntax") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.syntax === "foo + bar")
  }

  test("scala.meta.tokens.Token.Constant.toString") {
    // n/a
  }

  test("scala.meta.tokens.Token.Interpolation.toString") {
    // n/a
  }

  test("scala.meta.tokens.Token.Xml.toString") {
    // n/a
  }

  test("scala.meta.transversers.Transformer.toString") {
    // n/a
  }

  test("scala.meta.transversers.Traverser.toString") {
    // n/a
  }
}

package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._

class PublicSuite extends FunSuite {
  test("scala.meta.Dialect.toString") {
    Dialect.all.foreach(d => assert(d.name == d.toString))
  }

  test("scala.meta.Tree.toString (manual)") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.toString === "foo + bar")
  }

  test("scala.meta.Tree.show[Structure] (manual)") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.show[Structure] === """Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, Seq(Term.Name("bar")))""")
  }

  test("scala.meta.Tree.show[Syntax]") {
    val tree = Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, List(Term.Name("bar")))
    assert(tree.show[Syntax] === "foo + bar")
  }

  test("scala.meta.Tree.toString (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assert(tree.toString === "foo + bar // baz")
  }

  test("scala.meta.Tree.show[Structure] (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assert(tree.show[Structure] === """Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, Seq(Term.Name("bar")))""")
  }

  test("scala.meta.Tree.show[Syntax] (parsed)") {
    val tree = "foo + bar // baz".parse[Term].get
    assert(tree.show[Syntax] === "foo + bar // baz")
  }

  test("scala.meta.Tree.toString (quasiquotes)") {
    val tree = q"foo + bar // baz"
    assert(tree.toString === "foo + bar")
  }

  test("scala.meta.Tree.show[Structure] (quasiquoted)") {
    val tree = q"foo + bar // baz"
    assert(tree.show[Structure] === """Term.ApplyInfix(Term.Name("foo"), Term.Name("+"), Nil, Seq(Term.Name("bar")))""")
  }

  test("scala.meta.Tree.show[Syntax] (quasiquoted)") {
    val tree = q"foo + bar // baz"
    assert(tree.show[Syntax] === "foo + bar")
  }

  test("scala.meta.classifiers.Classifiable.toString") {
    // n/a
  }

  test("scala.meta.classifiers.Classifier.toString") {
    // n/a
  }

  test("scala.meta.common.Convert.toString") {
    // n/a
  }

  test("scala.meta.common.Optional.toString") {
    // n/a
  }

  test("scala.meta.dialects.Dotty.toString") {
    // covered above
  }

  test("scala.meta.dialects.Metalevel.toString") {
    // covered below
  }

  test("scala.meta.dialects.Metalevel.Quoted.toString") {
    assert(dialects.Metalevel.Quoted.toString === "Quoted")
  }

  test("scala.meta.dialects.Metalevel.Zero.toString") {
    assert(dialects.Metalevel.Zero.toString === "Zero")
  }

  test("scala.meta.dialects.Sbt0136.toString") {
    // covered above
  }

  test("scala.meta.dialects.Sbt0137.toString") {
    // covered above
  }

  test("scala.meta.dialects.Scala210.toString") {
    // covered above
  }

  test("scala.meta.dialects.Scala211.toString") {
    // covered above
  }

  test("scala.meta.inputs.Input.toString") {
    // covered below
  }

  test("scala.meta.inputs.Input.None.toString") {
    assert(Input.None.toString == "Input.None")
  }

  test("scala.meta.inputs.Input.File.toString") {
    import java.io._
    import java.nio.charset.Charset
    val file = new File("hello.scala")
    val input1 = Input.File(file, Charset.forName("cp1251"))
    val input2 = Input.File(file, Charset.forName("UTF-8"))
    assert(input1.toString == """Input.File(new File("hello.scala"), Charset.forName("windows-1251"))""")
    assert(input2.toString == """Input.File(new File("hello.scala"), Charset.forName("UTF-8"))""")
  }

  test("scala.meta.inputs.Input.Slice.toString") {
    val input = Input.Slice(Input.String("foo"), 0, 2)
    assert(input.toString == """Input.Slice(Input.String("foo"), 0, 2)""")
  }

  test("scala.meta.inputs.Input.Stream.toString") {
    import java.io._
    import java.nio.charset.Charset
    val cp1251 = Charset.forName("cp1251")
    val stream = new ByteArrayInputStream("Привет(мир!)".getBytes(cp1251))
    val input1 = Input.Stream(stream, cp1251)
    val input2 = Input.Stream(stream, Charset.forName("UTF-8"))
    assert(input1.toString == """Input.Stream(<stream>, Charset.forName("windows-1251"))""")
    assert(input2.toString == """Input.Stream(<stream>, Charset.forName("UTF-8"))""")
  }

  test("scala.meta.inputs.Input.String.toString") {
    val input = Input.String("foo")
    assert(input.toString == """Input.String("foo")""")
  }

  test("scala.meta.inputs.Point.toString") {
    // covered below
  }

  test("scala.meta.inputs.Point.None.toString") {
    assert(Point.None.toString == "Point.None")
  }

  test("scala.meta.inputs.Point.Offset.toString") {
    val Term.ApplyInfix(lhs, _, _, _) = "foo + bar".parse[Term].get
    assert(lhs.pos.start.toString === """0 in Input.String("foo + bar")""")
    assert(lhs.pos.end.toString === """3 in Input.String("foo + bar")""")
  }

  test("scala.meta.inputs.Position.toString") {
    // covered below
  }

  test("scala.meta.inputs.Position.None.toString") {
    assert(Position.None.toString == "Position.None")
  }

  test("scala.meta.inputs.Position.Range.toString") {
    val Term.ApplyInfix(lhs, _, _, _) = "foo + bar".parse[Term].get
    assert(lhs.pos.toString === """[0..3) in Input.String("foo + bar")""")
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
          """.trim.stripMargin)
          throw ex
      }
    }
  }

  test("scala.meta.parsers.Parsed.toString") {
    // covered below
  }

  test("scala.meta.parsers.Parsed.Error.toString") {
    val parsed = "foo + class".parse[Term]
    assert(parsed.toString === """
      |<input>:1: error: end of file expected but class found
      |foo + class
      |      ^
    """.trim.stripMargin)
  }

  test("scala.meta.parsers.Parsed.Success.toString") {
    val parsed = "foo + bar".parse[Term]
    assert(parsed.toString === "foo + bar")
  }

  test("scala.meta.prettyprinters.LowPriorityOptions.Lazy.toString") {
    assert(scala.meta.prettyprinters.Options.Lazy.toString == "Lazy")
  }

  test("scala.meta.prettyprinters.Options.toString") {
    // covered above and below
  }

  test("scala.meta.prettyprinters.Options.Eager.toString") {
    assert(scala.meta.prettyprinters.Options.Eager.toString == "Eager")
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
          """.trim.stripMargin)
          throw ex
      }
    }
  }

  test("scala.meta.tokenizers.Tokenized.Error.toString") {
    val tokenized = """"c""".tokenize
    assert(tokenized.toString === """
      |<input>:1: error: unclosed string literal
      |"c
      |^
    """.trim.stripMargin)
  }

  test("scala.meta.tokenizers.Tokenized.Success.toString") {
    val tokenized = "foo + bar".tokenize
    assert(tokenized.toString === "foo + bar")
  }

  test("scala.meta.tokens.Token.toString") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.toString === "foo")
  }

  test("scala.meta.tokens.Token.show[Structure]") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.show[Structure] === "foo [0..3)")
  }

  test("scala.meta.tokens.Token.show[Syntax]") {
    val token = "foo + bar".tokenize.get(1)
    assert(token.show[Syntax] === "foo")
  }

  test("scala.meta.tokens.Tokens.toString") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.toString === "foo + bar")
  }

  test("scala.meta.tokens.Tokens.show[Structure]") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.show[Structure] === "Tokens(BOF [0..0), foo [0..3),   [3..4), + [4..5),   [5..6), bar [6..9), EOF [9..9))")
  }

  test("scala.meta.tokens.Tokens.show[Syntax]") {
    val tokens = "foo + bar".tokenize.get
    assert(tokens.show[Syntax] === "foo + bar")
  }

  test("scala.meta.transversers.Transformer.toString") {
    // n/a
  }

  test("scala.meta.transversers.Traverser.toString") {
    // n/a
  }
}

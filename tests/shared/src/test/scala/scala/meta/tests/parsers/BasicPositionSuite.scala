package scala.meta.tests
package parsers

import scala.meta._

class BasicPositionSuite extends BasePositionSuite(dialects.Scala213) {
  checkPositions[Term](
    "1 + (2 / 3) * 4",
    """|Type.ArgClause 1 + @@(2 / 3) * 4
       |Term.ApplyInfix (2 / 3) * 4
       |Term.ApplyInfix 2 / 3
       |Type.ArgClause 1 + (2 / @@3) * 4
       |Type.ArgClause 1 + (2 / 3) * @@4
       |""".stripMargin
  )

  checkPositions[Term](
    "1 + (()) * 4",
    """|Type.ArgClause 1 + @@(()) * 4
       |Term.ApplyInfix (()) * 4
       |Type.ArgClause 1 + (()) * @@4
       |""".stripMargin
  )

  checkPositions[Term](
    "1 + ((1, 2, 3)) * 4",
    """|Type.ArgClause 1 + @@((1, 2, 3)) * 4
       |Term.ApplyInfix ((1, 2, 3)) * 4
       |Term.Tuple (1, 2, 3)
       |Type.ArgClause 1 + ((1, 2, 3)) * @@4
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (b)",
    """|Type.ArgClause a f @@(b)
       |Term.ArgClause (b)
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (123)",
    """|Type.ArgClause a f @@(123)
       |Term.ArgClause (123)
       |""".stripMargin
  )

  checkPositions[Term](
    "a f ()",
    """|Type.ArgClause a f @@()
       |Term.ArgClause ()
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (())",
    """|Type.ArgClause a f @@(())
       |Term.ArgClause (())
       |""".stripMargin
  )

  checkPositions[Term](
    "a f ((1, 2, 3))",
    """|Type.ArgClause a f @@((1, 2, 3))
       |Term.ArgClause ((1, 2, 3))
       |Term.Tuple (1, 2, 3)
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (()).foo",
    """|Type.ArgClause a f @@(()).foo
       |Term.Select (()).foo
       |""".stripMargin
  )

  checkPositions[Term](
    "a f (())(b)",
    """|Type.ArgClause a f @@(())(b)
       |Term.Apply (())(b)
       |Term.ArgClause (b)
       |""".stripMargin
  )

  checkPositions[Term](
    "1 + { 2 / 3 } * 4",
    """|Type.ArgClause 1 + @@{ 2 / 3 } * 4
       |Term.ApplyInfix { 2 / 3 } * 4
       |Term.Block { 2 / 3 }
       |Term.ApplyInfix 2 / 3
       |Type.ArgClause 1 + { 2 / @@3 } * 4
       |Type.ArgClause 1 + { 2 / 3 } * @@4
       |""".stripMargin
  )

  checkPositions[Term](
    "{ 2 / 3 } + 4",
    """|Term.Block { 2 / 3 }
       |Term.ApplyInfix 2 / 3
       |Type.ArgClause { 2 / @@3 } + 4
       |Type.ArgClause { 2 / 3 } + @@4
       |""".stripMargin
  )

  checkPositions[Term](
    "(1 + 2).foo",
    """|Term.ApplyInfix 1 + 2
       |Type.ArgClause (1 + @@2).foo
       |""".stripMargin
  )
  checkPositions[Term](
    "foo == (a + b).c(d)",
    """|Type.ArgClause foo == @@(a + b).c(d)
       |Term.Apply (a + b).c(d)
       |Term.Select (a + b).c
       |Term.ApplyInfix a + b
       |Type.ArgClause foo == (a + @@b).c(d)
       |Term.ArgClause (d)
       |""".stripMargin
  )
  checkPositions[Stat](
    // Issue #333
    """def shortInfo: String = s"created=$x"""",
    """|Term.Interpolate s"created=$x"
       |""".stripMargin
  )
  checkPositions[Case](
    // Issue #331
    "case foo if bar || baz =>",
    """|Term.ApplyInfix bar || baz
       |Type.ArgClause case foo if bar || @@baz =>
       |Term.Block case foo if bar || baz =>@@
       |""".stripMargin
  )
  checkPositions[Stat](
    """a + b + c""",
    """|Term.ApplyInfix a + b
       |Type.ArgClause a + @@b + c
       |Type.ArgClause a + b + @@c
       |""".stripMargin
  )

  test("apply(startLine,startColumn,endLine,endColumn)") {
    val input = Input.String(
      """|  val x = 2 // line 0
         |
         |            // line 2""".stripMargin
    )
    val x = Position.Range(input, 0, 2, 0, 11)
    assertEquals(x.text, "val x = 2")
    val x2 = Position.Range(input, 0, 2, 0, Int.MaxValue)
    assertEquals(x2.text, "val x = 2 // line 0")
    val empty = Position.Range(input, 1, 0, 1, Int.MaxValue)
    assertEquals(empty.text, "")
    val last = Position.Range(input, 2, 0, 2, Int.MaxValue)
    assertEquals(last.text, "            // line 2")
  }

  checkPositions[Stat](
    """(((a +: b) +: c) +: d)""",
    """|Term.ApplyInfix (a +: b) +: c
       |Term.ApplyInfix a +: b
       |Type.ArgClause (((a +: @@b) +: c) +: d)
       |Type.ArgClause (((a +: b) +: @@c) +: d)
       |Type.ArgClause (((a +: b) +: c) +: @@d)
       |""".stripMargin
  )

  checkPositions[Stat](
    """(((a :+ b) :+ c) :+ d)""",
    """|Term.ApplyInfix (a :+ b) :+ c
       |Term.ApplyInfix a :+ b
       |Type.ArgClause (((a :+ @@b) :+ c) :+ d)
       |Type.ArgClause (((a :+ b) :+ @@c) :+ d)
       |Type.ArgClause (((a :+ b) :+ c) :+ @@d)
       |""".stripMargin
  )

  checkPositions[Stat](
    """(a +: (b +: (c +: d) +: b) +: a)""",
    """|Type.ArgClause (a +: @@(b +: (c +: d) +: b) +: a)
       |Term.ApplyInfix (b +: (c +: d) +: b) +: a
       |Term.ApplyInfix b +: (c +: d) +: b
       |Type.ArgClause (a +: (b +: @@(c +: d) +: b) +: a)
       |Term.ApplyInfix (c +: d) +: b
       |Term.ApplyInfix c +: d
       |Type.ArgClause (a +: (b +: (c +: @@d) +: b) +: a)
       |Type.ArgClause (a +: (b +: (c +: d) +: @@b) +: a)
       |Type.ArgClause (a +: (b +: (c +: d) +: b) +: @@a)
       |""".stripMargin
  )

  checkPositions[Stat](
    """(a +: (b +: (c +: d)))""",
    """|Type.ArgClause (a +: @@(b +: (c +: d)))
       |Term.ArgClause (b +: (c +: d))
       |Term.ApplyInfix b +: (c +: d)
       |Type.ArgClause (a +: (b +: @@(c +: d)))
       |Term.ArgClause (c +: d)
       |Term.ApplyInfix c +: d
       |Type.ArgClause (a +: (b +: (c +: @@d)))
       |""".stripMargin
  )

  checkPositions[Stat](
    """(a :+ (b :+ (c :+ d)))""",
    """|Type.ArgClause (a :+ @@(b :+ (c :+ d)))
       |Term.ArgClause (b :+ (c :+ d))
       |Term.ApplyInfix b :+ (c :+ d)
       |Type.ArgClause (a :+ (b :+ @@(c :+ d)))
       |Term.ArgClause (c :+ d)
       |Term.ApplyInfix c :+ d
       |Type.ArgClause (a :+ (b :+ (c :+ @@d)))
       |""".stripMargin
  )

  checkPositions[Stat](
    """(a :+ (b :+ (c :+ d) :+ b) :+ a)""",
    """|Term.ApplyInfix a :+ (b :+ (c :+ d) :+ b)
       |Type.ArgClause (a :+ @@(b :+ (c :+ d) :+ b) :+ a)
       |Term.ArgClause (b :+ (c :+ d) :+ b)
       |Term.ApplyInfix b :+ (c :+ d) :+ b
       |Term.ApplyInfix b :+ (c :+ d)
       |Type.ArgClause (a :+ (b :+ @@(c :+ d) :+ b) :+ a)
       |Term.ArgClause (c :+ d)
       |Term.ApplyInfix c :+ d
       |Type.ArgClause (a :+ (b :+ (c :+ @@d) :+ b) :+ a)
       |Type.ArgClause (a :+ (b :+ (c :+ d) :+ @@b) :+ a)
       |Type.ArgClause (a :+ (b :+ (c :+ d) :+ b) :+ @@a)
       |""".stripMargin
  )

  checkPositions[Stat](
    """(a (b (c d) b) a)""",
    """|Term.Apply a (b (c d) b)
       |Term.ArgClause (b (c d) b)
       |Term.Select b (c d) b
       |Term.Apply b (c d)
       |Term.ArgClause (c d)
       |Term.Select c d
       |""".stripMargin
  )

  checkPositions[Stat](
    """(a + (b + (c d)))""",
    """|Type.ArgClause (a + @@(b + (c d)))
       |Term.ArgClause (b + (c d))
       |Term.ApplyInfix b + (c d)
       |Type.ArgClause (a + (b + @@(c d)))
       |Term.ArgClause (c d)
       |Term.Select c d
       |""".stripMargin
  )

  checkPositions[Stat](
    """(a (b (c d)))""",
    """|Term.ArgClause (b (c d))
       |Term.Apply b (c d)
       |Term.ArgClause (c d)
       |Term.Select c d
       |""".stripMargin
  )

  checkPositions[Stat](
    """(((c d) b) a)""",
    """|Term.Select (c d) b
       |Term.Select c d
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a -> b)""",
    """|Term.ApplyInfix a -> b
       |Type.ArgClause case foo => (a -> @@b)
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a -> b) -> c""",
    """|Term.ApplyInfix (a -> b) -> c
       |Term.ApplyInfix a -> b
       |Type.ArgClause case foo => (a -> @@b) -> c
       |Type.ArgClause case foo => (a -> b) -> @@c
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a :+ b)""",
    """|Term.ApplyInfix a :+ b
       |Type.ArgClause case foo => (a :+ @@b)
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a :+ b) :+ c""",
    """|Term.ApplyInfix (a :+ b) :+ c
       |Term.ApplyInfix a :+ b
       |Type.ArgClause case foo => (a :+ @@b) :+ c
       |Type.ArgClause case foo => (a :+ b) :+ @@c
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a +: b)""",
    """|Term.ApplyInfix a +: b
       |Type.ArgClause case foo => (a +: @@b)
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a +: b) +: c""",
    """|Term.ApplyInfix (a +: b) +: c
       |Term.ApplyInfix a +: b
       |Type.ArgClause case foo => (a +: @@b) +: c
       |Type.ArgClause case foo => (a +: b) +: @@c
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a, b)""",
    """|Term.Tuple (a, b)
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => (a, b).bar""",
    """|Term.Select (a, b).bar
       |Term.Tuple (a, b)
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => {(a -> b)}""",
    """|Term.Block {(a -> b)}
       |Term.ApplyInfix a -> b
       |Type.ArgClause case foo => {(a -> @@b)}
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => {(a :+ b)}""",
    """|Term.Block {(a :+ b)}
       |Term.ApplyInfix a :+ b
       |Type.ArgClause case foo => {(a :+ @@b)}
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => {(a +: b)}""",
    """|Term.Block {(a +: b)}
       |Term.ApplyInfix a +: b
       |Type.ArgClause case foo => {(a +: @@b)}
       |""".stripMargin
  )

  checkPositions[Case](
    """case foo => {(a, b)}""",
    """|Term.Block {(a, b)}
       |Term.Tuple (a, b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a -> b)""",
    """|Term.ApplyInfix a -> b
       |Type.ArgClause val foo = (a -> @@b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a -> b) -> c""",
    """|Term.ApplyInfix (a -> b) -> c
       |Term.ApplyInfix a -> b
       |Type.ArgClause val foo = (a -> @@b) -> c
       |Type.ArgClause val foo = (a -> b) -> @@c
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a :+ b)""",
    """|Term.ApplyInfix a :+ b
       |Type.ArgClause val foo = (a :+ @@b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a :+ b) :+ c""",
    """|Term.ApplyInfix (a :+ b) :+ c
       |Term.ApplyInfix a :+ b
       |Type.ArgClause val foo = (a :+ @@b) :+ c
       |Type.ArgClause val foo = (a :+ b) :+ @@c
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a +: b)""",
    """|Term.ApplyInfix a +: b
       |Type.ArgClause val foo = (a +: @@b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a +: b) +: c""",
    """|Term.ApplyInfix (a +: b) +: c
       |Term.ApplyInfix a +: b
       |Type.ArgClause val foo = (a +: @@b) +: c
       |Type.ArgClause val foo = (a +: b) +: @@c
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a, b)""",
    """|Term.Tuple (a, b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = (a, b).bar""",
    """|Term.Select (a, b).bar
       |Term.Tuple (a, b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = {(a -> b)}""",
    """|Term.Block {(a -> b)}
       |Term.ApplyInfix a -> b
       |Type.ArgClause val foo = {(a -> @@b)}
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = {(a :+ b)}""",
    """|Term.Block {(a :+ b)}
       |Term.ApplyInfix a :+ b
       |Type.ArgClause val foo = {(a :+ @@b)}
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = {(a +: b)}""",
    """|Term.Block {(a +: b)}
       |Term.ApplyInfix a +: b
       |Type.ArgClause val foo = {(a +: @@b)}
       |""".stripMargin
  )

  checkPositions[Stat](
    """val foo = {(a, b)}""",
    """|Term.Block {(a, b)}
       |Term.Tuple (a, b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """type foo = (a -> b)""",
    """|Type.ParamClause type foo @@= (a -> b)
       |Type.ApplyInfix a -> b
       |Type.Bounds type foo = @@(a -> b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """type foo = (a -> b) -> c""",
    """|Type.ParamClause type foo @@= (a -> b) -> c
       |Type.ApplyInfix (a -> b) -> c
       |Type.ApplyInfix a -> b
       |Type.Bounds type foo = @@(a -> b) -> c
       |""".stripMargin
  )

  checkPositions[Stat](
    """type foo = (a :+ b)""",
    """|Type.ParamClause type foo @@= (a :+ b)
       |Type.ApplyInfix a :+ b
       |Type.Bounds type foo = @@(a :+ b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """type foo = (a :+ b) :+ c""",
    """|Type.ParamClause type foo @@= (a :+ b) :+ c
       |Type.ApplyInfix (a :+ b) :+ c
       |Type.ApplyInfix a :+ b
       |Type.Bounds type foo = @@(a :+ b) :+ c
       |""".stripMargin
  )

  checkPositions[Stat](
    """type foo = (a +: b)""",
    """|Type.ParamClause type foo @@= (a +: b)
       |Type.ApplyInfix a +: b
       |Type.Bounds type foo = @@(a +: b)
       |""".stripMargin
  )

  checkPositions[Stat](
    """type foo = (a +: b) +: c""",
    """|Type.ParamClause type foo @@= (a +: b) +: c
       |Type.ApplyInfix (a +: b) +: c
       |Type.ApplyInfix a +: b
       |Type.Bounds type foo = @@(a +: b) +: c
       |""".stripMargin
  )

  checkPositions[Stat](
    """type foo = (a, b)""",
    """|Type.ParamClause type foo @@= (a, b)
       |Type.Tuple (a, b)
       |Type.Bounds type foo = @@(a, b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ (a -> b)""",
    """|Pat.ExtractInfix (a -> b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ (a -> b) -> c""",
    """|Pat.ExtractInfix (a -> b) -> c
       |Pat.ExtractInfix (a -> b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ (a :+ b)""",
    """|Pat.ExtractInfix (a :+ b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ (a :+ b) :+ c""",
    """|Pat.ExtractInfix (a :+ b) :+ c
       |Pat.ExtractInfix (a :+ b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ (a +: b)""",
    """|Pat.ExtractInfix (a +: b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ (a +: b) +: c""",
    """|Pat.ExtractInfix (a +: b) +: c
       |Pat.ExtractInfix (a +: b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ (a, b)""",
    """|Pat.Tuple (a, b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ ((a) | (b))""",
    """|Pat.Alternative ((a) | (b))
       |Pat.Var (a)
       |Pat.Var (b)
       |""".stripMargin
  )

  checkPositions[Pat](
    """foo @ ((a) | ((b) | (c)))""",
    """|Pat.Alternative ((a) | ((b) | (c)))
       |Pat.Var (a)
       |Pat.Alternative ((b) | (c))
       |Pat.Var (b)
       |Pat.Var (c)
       |""".stripMargin
  )

  checkPositions[Stat](
    """|object A {
       |  (try foo finally bar)
       |}
       |""".stripMargin,
    """|Template {
       |  (try foo finally bar)
       |}
       |Template.Body {
       |  (try foo finally bar)
       |}
       |Term.Try try foo finally bar
       |""".stripMargin
  )

  checkPositions[Stat](
    """|val A = {
       |  (try foo finally bar)
       |}
       |""".stripMargin,
    """|Term.Block {
       |  (try foo finally bar)
       |}
       |Term.Try try foo finally bar
       |""".stripMargin
  )

  checkPositions[Stat](
    """|for {
       |  a <- (b)
       |  c = (d)
       |} yield c
       |""".stripMargin,
    """|Term.EnumeratorsBlock {
       |  a <- (b)
       |  c = (d)
       |}
       |Enumerator.Generator a <- (b)
       |Enumerator.Val c = (d)
       |""".stripMargin
  )

  checkPositions[Stat](
    """|val foo = (a + b) match {
       |  case _ =>
       |}
       |""".stripMargin,
    """|Term.Match (a + b) match {
       |  case _ =>
       |}
       |Term.ApplyInfix a + b
       |Type.ArgClause val foo = (a + @@b) match {
       |Term.CasesBlock {
       |  case _ =>
       |}
       |Case case _ =>
       |Term.Block @@}
       |""".stripMargin
  )

}

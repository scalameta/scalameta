package scala.meta.tests
package prettyprinters

import scala.meta._

/**
 * Pins Term/Type/Pat infix parenthesization for hand-built trees (no parser involved): parse ->
 * print -> parse round-trips cannot localize printer-only regressions, and several shapes below
 * cannot be produced by the parser at all (they arrive from Scalafix rewrites or quasiquote
 * splices).
 */
class InfixParensSuite extends TreeSuiteBase {

  // term/pat operator rules are dialect-independent; dialect-sensitive type pairs use checkTypeBoth
  implicit val dialect: Dialect = dialects.Scala213

  // expected syntax doubles as (section-prefixed) test name to keep MUnit names unique
  private def checkTerm(tree: => Term, expected: String)(implicit loc: munit.Location): Unit =
    test(s"term: $expected")(assertSyntax(expected)(tree))

  private def checkType(tree: => Type, expected: String)(implicit loc: munit.Location): Unit =
    test(s"type: $expected")(assertSyntax(expected)(tree))

  private def checkPat(tree: => Pat, expected: String)(implicit loc: munit.Location): Unit =
    test(s"pat: $expected")(assertSyntax(expected)(tree))

  // type-infix precedence is consulted only when dialect.useInfixTypePrecedence: same tree, both renderings
  private def checkTypeBoth(tree: => Type, scala213: String, scala3: String)(implicit
      loc: munit.Location,
  ): Unit = test(s"type 213/3: $scala213 / $scala3") {
    assertNoDiff(tree.reprint(dialects.Scala213), scala213, "Scala213")
    assertNoDiff(tree.reprint(dialects.Scala3), scala3, "Scala3")
  }

  // A. associativity: children on their natural side print flat, on the other side get parens

  checkTerm(tinfix(tinfix(tname("a"), "-", tname("b")), "-", tname("c")), "a - b - c")
  checkTerm(tinfix(tname("a"), "-", tinfix(tname("b"), "-", tname("c"))), "a - (b - c)")
  checkTerm(tinfix(tname("a"), "::", tinfix(tname("b"), "::", tname("c"))), "a :: b :: c")
  checkTerm(tinfix(tinfix(tname("a"), "::", tname("b")), "::", tname("c")), "(a :: b) :: c")
  checkTerm(tinfix(tname("a"), "+:", tinfix(tname("b"), "+:", tname("c"))), "a +: b +: c")
  checkTerm(tinfix(tinfix(tname("a"), "+:", tname("b")), "+:", tname("c")), "(a +: b) +: c")

  // B. precedence: parens appear iff the child binds looser than its parent

  checkTerm(tinfix(tname("a"), "+", tinfix(tname("b"), "*", tname("c"))), "a + b * c")
  checkTerm(tinfix(tinfix(tname("a"), "+", tname("b")), "*", tname("c")), "(a + b) * c")
  checkTerm(tinfix(tinfix(tname("a"), "*", tname("b")), "+", tname("c")), "a * b + c")
  checkTerm(tinfix(tname("a"), "*", tinfix(tname("b"), "+", tname("c"))), "a * (b + c)")
  // same precedence, different ops: only the off-side child needs parens
  checkTerm(tinfix(tinfix(tname("a"), "+", tname("b")), "-", tname("c")), "a + b - c")
  checkTerm(tinfix(tname("a"), "+", tinfix(tname("b"), "-", tname("c"))), "a + (b - c)")
  // alphanumeric ops have the lowest precedence
  checkTerm(tinfix(tinfix(tname("a"), "max", tname("b")), "+", tname("c")), "(a max b) + c")
  checkTerm(tinfix(tname("a"), "max", tinfix(tname("b"), "+", tname("c"))), "a max b + c")
  checkTerm(
    tinfix(tinfix(tname("a"), "<", tname("b")), "&&", tinfix(tname("c"), "<", tname("d"))),
    "a < b && c < d",
  )
  checkTerm(tinfix(tinfix(tname("a"), "&&", tname("b")), "||", tname("c")), "a && b || c")
  checkTerm(tinfix(tname("a"), "&&", tinfix(tname("b"), "||", tname("c"))), "a && (b || c)")

  // C. mixed associativity forces parens even where precedence alone would not

  // prec alone would omit parens (:: is 7, + is 8, child on RHS) — this isolates the assoc rule
  checkTerm(tinfix(tname("a"), "::", tinfix(tname("b"), "+", tname("c"))), "a :: (b + c)")
  checkTerm(tinfix(tinfix(tname("a"), "+", tname("b")), "::", tname("c")), "(a + b) :: c")
  // +: is right-assoc (ends in colon) with prec of '+'; :+ is left-assoc with prec of ':'
  checkTerm(tinfix(tname("x"), "+:", tinfix(tname("xs"), ":+", tname("y"))), "x +: (xs :+ y)")

  // D. unary and non-infix operands

  checkTerm(tinfix(unary("-", tname("a")), "+", tname("b")), "-a + b")
  checkTerm(unary("-", tinfix(tname("a"), "+", tname("b"))), "-(a + b)")
  checkTerm(unary("!", tapply(tname("f"), tname("x"))), "!f(x)")
  checkTerm(tinfix(Term.Ascribe(tname("a"), pname("Int")), "+", tname("b")), "(a: Int) + b")

  // E. single-arg unwrap whitelist: which RHS shapes print bare vs wrapped in an ArgClause;
  // several of these cannot be produced by the parser — they pin the printer contract for
  // rewrite-produced trees

  checkTerm(tinfix(tname("a"), "+", lit(1)), "a + 1")
  checkTerm(tinfix(tname("a"), "++", tselect(tname("b"), tname("c"))), "a ++ b.c")
  checkTerm(tinfix(tname("a"), "++", tapply(tname("f"), tname("b"))), "a ++ f(b)")
  checkTerm(tinfix(tname("a"), "+", Term.Placeholder()), "a + _")
  // Tuple is not whitelisted: the extra parens disambiguate from a two-argument call
  checkTerm(tinfix(tname("a"), "->", Term.Tuple(List(tname("b"), tname("c")))), "a -> ((b, c))")
  // Lit.Unit is explicitly excluded from the Lit whitelist
  checkTerm(tinfix(tname("a"), "->", lit()), "a -> (())")
  checkTerm(tinfix(tname("a"), "op", Term.Ascribe(tname("b"), pname("Int"))), "a op (b: Int)")
  checkTerm(tinfix(tname("a"), "op", blk()), "a op {}")
  checkTerm(tinfix(tname("a"), "op", tname("b"), tname("c")), "a op (b, c)")
  // zero-arg infix is not parseable under Scala213 (allowEmptyInfixArgs is off) — printer-only shape
  checkTerm(tinfix(tname("a"), "op"), "a op ()")
  // type args on infix calls are not parser-producible either
  checkTerm(tinfix(tname("a"), "op", List(pname("T")), tname("b")), "a op[T] b")

  // F. types: assoc/side rules apply in all dialects; precedence only under useInfixTypePrecedence

  checkType(pinfix(pname("A"), "|", pinfix(pname("B"), "|", pname("C"))), "A | (B | C)")
  checkType(pinfix(pfunc(pname("A"))(pname("B")), "&", pname("C")), "(A => B) & C")
  checkType(pinfix(pname("A"), "&", pfunc(pname("B"))(pname("C"))), "A & (B => C)")
  checkType(pfunc(pname("A"))(pinfix(pname("B"), "&", pname("C"))), "A => B & C")
  checkType(pinfix(pname("A"), "::", pinfix(pname("B"), "::", pname("C"))), "A :: B :: C")
  checkType(pinfix(pinfix(pname("A"), "::", pname("B")), "::", pname("C")), "(A :: B) :: C")
  // mixed associativity forces parens regardless of the precedence flag
  checkType(pinfix(pinfix(pname("A"), "&", pname("B")), "::", pname("C")), "(A & B) :: C")
  // without useInfixTypePrecedence (213) all type ops rank equal: only the side rule fires
  checkTypeBoth(
    pinfix(pinfix(pname("A"), "|", pname("B")), "&", pname("C")),
    scala213 = "A | B & C",
    scala3 = "(A | B) & C",
  )
  checkTypeBoth(
    pinfix(pname("A"), "|", pinfix(pname("B"), "&", pname("C"))),
    scala213 = "A | (B & C)",
    scala3 = "A | B & C",
  )
  // control: parens in both dialects, for different reasons (side rule vs precedence)
  checkTypeBoth(
    pinfix(pname("A"), "&", pinfix(pname("B"), "|", pname("C"))),
    scala213 = "A & (B | C)",
    scala3 = "A & (B | C)",
  )
  // Type.And/Type.Or delegate to the same infix machinery
  test("type: Type.And/Type.Or delegate to ApplyInfix parenthesization") {
    val tree = Type.And(Type.Or(pname("A"), pname("B")), pname("C"))
    assertNoDiff(tree.reprint(dialects.Scala3), "(A | B) & C")
  }

  // G. patterns

  checkPat(patinfix(patvar("a"), "::", patinfix(patvar("b"), "::", tname("Nil"))), "a :: b :: Nil")
  checkPat(patinfix(patinfix(patvar("a"), "::", patvar("b")), "::", tname("Nil")), "(a :: b) :: Nil")
  checkPat(patinfix(patvar("x"), "+:", patinfix(patvar("xs"), ":+", patvar("y"))), "x +: (xs :+ y)")
  // Pat.Alternative binds loosest: wrapped as an operand, unwrapped as a parent
  checkPat(patinfix(Pat.Alternative(lit(1), lit(2)), "::", patvar("rest")), "(1 | 2) :: rest")
  checkPat(Pat.Alternative(patinfix(patvar("a"), "::", patvar("b")), tname("Nil")), "a :: b | Nil")
  checkPat(patinfix(Pat.Bind(patvar("x"), patwildcard), "::", patvar("rest")), "(x @ _) :: rest")
  checkPat(Pat.Bind(patvar("x"), patinfix(patvar("a"), "::", patvar("b"))), "x @ a :: b")
  checkPat(patinfix(Pat.Typed(patvar("x"), pname("Int")), "::", patvar("rest")), "(x: Int) :: rest")
  checkPat(patinfix(patextract(tname("Some"), patvar("x")), "::", patvar("rest")), "Some(x) :: rest")
  checkPat(patinfix(patvar("a"), "op", patvar("b"), patvar("c")), "a op (b, c)")
  // the op is backquoted to disambiguate from Pat.Alternative's `|`
  checkPat(patinfix(patvar("a"), "|", patvar("b")), "a `|` b")

}

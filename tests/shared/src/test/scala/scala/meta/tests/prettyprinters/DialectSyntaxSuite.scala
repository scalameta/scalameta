package scala.meta.tests
package prettyprinters

import scala.meta._

/**
 * Pins dialect-flag-driven rendering of ONE hand-built tree under several dialects. Round-trips
 * cannot cover this: the same tree must print differently per dialect.
 *
 * Out of scope on purpose: `given` new-vs-old syntax and brace context-bounds — those branches
 * sniff origin tokens, which hand-built trees never have, so only their fallback is reachable (see
 * the fallback section below for the nodes where that fallback is worth pinning).
 */
class DialectSyntaxSuite extends TreeSuiteBase {

  // no suite-level implicit dialect on purpose: every assertion names its dialect

  private def checkReprint(name: String, tree: => Tree)(
      expected: (Dialect, String)*,
  )(implicit loc: munit.Location): Unit = test(name)(expected.foreach { case (dialect, syntax) =>
    assertNoDiff(tree.reprint(dialect), syntax, s"dialect: $dialect")
  })

  // A. pure flag matrix

  // allowPostfixStarVarargSplices
  checkReprint("vararg splice", tapply(tname("f"), Term.Repeated(tname("xs"))))(
    dialects.Scala213 -> "f(xs: _*)",
    dialects.Scala213Source3 -> "f(xs*)",
    dialects.Scala3 -> "f(xs*)",
  )

  // allowStarWildcardImport
  checkReprint("wildcard import", Import(List(Importer(tname("a"), List(Importee.Wildcard())))))(
    dialects.Scala213 -> "import a._",
    dialects.Scala3 -> "import a.*",
  )

  // allowAsForImportRename
  checkReprint(
    "import rename",
    Import(List(Importer(tname("a"), List(Importee.Rename(Name("b"), Name("c")))))),
  )(dialects.Scala213 -> "import a.{b => c}", dialects.Scala3 -> "import a.b as c")
  checkReprint("unimport", Import(List(Importer(tname("a"), List(Importee.Unimport(Name("b")))))))(
    dialects.Scala213 -> "import a.{b => _}",
    dialects.Scala3 -> "import a.b as _",
  )

  // allowMatchAsOperator
  checkReprint("select match", tselectmatch(tname("x"), Case(patwildcard, None, lit(1))))(
    dialects.Scala213 ->
      """|x match {
         |  case _ => 1
         |}""".stripMargin,
    dialects.Scala3 ->
      """|x.match {
         |  case _ => 1
         |}""".stripMargin,
  )

  // allowAtForExtractorVarargs is on in EVERY standard dialect, so `@` always wins; the colon
  // branch (allowColonForExtractorVarargs) is reachable only through a tweaked dialect
  checkReprint("extractor varargs", Pat.Bind(patvar("xs"), Pat.SeqWildcard()))(
    dialects.Scala213 -> "xs @ _*",
    dialects.Scala3 -> "xs @ _*",
    dialects.Scala3.withAllowAtForExtractorVarargs(false) -> "xs: _*",
  )

  // B. dialect-forbidden constructs throw instead of misprinting; each paired with the
  // dialect where the same tree prints fine

  test("Mod.Inline requires allowInlineMods") {
    val tree = Defn.Val(List(Mod.Inline()), List(patvar("x")), None, lit(1))
    val e = intercept[UnsupportedOperationException](tree.reprint(dialects.Scala213))
    assert(e.getMessage.contains("doesn't support inline modifiers"))
    assertNoDiff(tree.reprint(dialects.Scala3), "inline val x = 1")
  }

  test("trait parameters require allowTraitParameters") {
    // the guard fires on ctor MODS, not on the params themselves
    def tree(mods: List[Mod]) = Defn.Trait(
      Nil,
      pname("A"),
      noPpc,
      Ctor.Primary(mods, anon, List(tpc(tparam("a", pname("Int"))))),
      tplNoBody(),
    )
    val treeWithMods = tree(List(Mod.Private(anon)))
    val e = intercept[UnsupportedOperationException](treeWithMods.reprint(dialects.Scala213))
    assert(e.getMessage.contains("doesn't support trait parameters"))
    assertNoDiff(treeWithMods.reprint(dialects.Scala3), "trait A private (a: Int)")
    // mod-less ctor params slip past the guard even under 213: pins the guard's actual shape
    assertNoDiff(tree(Nil).reprint(dialects.Scala213), "trait A(a: Int)")
  }

  test("extractor varargs require an enabled separator") {
    val tree = Pat.Bind(patvar("xs"), Pat.SeqWildcard())
    val dialect = dialects.Scala211.withAllowAtForExtractorVarargs(false)
    val e = intercept[UnsupportedOperationException](tree.reprint(dialect))
    assert(e.getMessage.contains("doesn't support extractor varargs"))
  }

  test("literal-type pattern requires allowLiteralTypes") {
    val tree = Pat.Typed(patvar("x"), lit(1))
    val e = intercept[UnsupportedOperationException](tree.reprint(dialects.Scala211))
    assert(e.getMessage.contains("doesn't support literal types"))
    assertNoDiff(tree.reprint(dialects.Scala213), "x: 1")
  }

  // C. no-origin fallbacks of token-sniffing branches (cf. RegressionSyntaxSuite): hand-built
  // trees have no origin tokens, so `?` requires allowUnderscoreAsTypePlaceholder, not Scala3

  checkReprint("type wildcard fallback", papply(pname("List"), pwildcard))(
    dialects.Scala213 -> "List[_]",
    dialects.Scala3 -> "List[_]",
    dialects.Scala3Future -> "List[?]",
  )
  checkReprint("anonymous type param fallback", Type.AnonymousParam(None))(dialects.Scala3 -> "_")
  checkReprint("double literal without origin text", dbl("1"))(dialects.Scala213 -> "1.0")
  checkReprint("double literal keeps value text", dbl("1.5"))(dialects.Scala213 -> "1.5")
  checkReprint("string literal quoting", str("ab"))(dialects.Scala213 -> "\"ab\"")
  checkReprint("multiline string literal is triple-quoted", str("a\nb"))(
    dialects.Scala213 -> ("\"\"\"" + "a\nb" + "\"\"\""),
  )

}

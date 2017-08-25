package scala.meta
package tests

class SyntheticSuite extends BaseSemanticSuite {
  implicit val database = Database.load(classpath, sourcepath)

  test("Database.symbols") {
    val entry = database.entries.find(_.input.syntax.contains("Example")).get
    val source = entry.input.parse[Source].get
    val sugarAsserts = source.collect {
      case t: Defn.Def if t.name.value == "main" =>
        val symbol = entry.names.find(_.position == t.name.pos).get.symbol
        val expectedInput =
          Input.Denotation("(args: scala.Array[scala.Predef.String])scala.Unit", symbol.syntax)
        val infoSymbols = entry.symbols.find(_.symbol == symbol).get.denotation.resolvedNames
        assert(infoSymbols.nonEmpty)
        infoSymbols.foreach {
          case ResolvedName(Position.Range(input, _, _), _, false) =>
            assert(input == expectedInput)
          case els =>
            sys.error(s"Unexpected $els")
        }
    }
    assert(sugarAsserts.nonEmpty)

  }

  test("Database.sugars") {
    val entry = database.entries.find(_.input.syntax.contains("Sugar")).get
    val source = entry.input.parse[Source].get
    val sugarAsserts = source.collect {
      case q"$term.stripPrefix($_)" =>
        val sugar = entry.sugars.find(_.position == term.pos).get
        assert(sugar.names.nonEmpty)
    }
    assert(sugarAsserts.nonEmpty)
  }

}

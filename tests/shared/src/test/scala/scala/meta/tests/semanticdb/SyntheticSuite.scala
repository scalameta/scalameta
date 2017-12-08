package scala.meta.tests
package semanticdb

import scala.meta._

class SyntheticSuite extends BaseSemanticSuite {
  implicit val database = Database.load(classpath, sourcepath)

  test("Database.symbols") {
    val entry = database.documents.find(_.input.syntax.contains("Example")).get
    val source = entry.input.parse[Source].get
    val syntheticAsserts = source.collect {
      case t: Defn.Def if t.name.value == "main" =>
        val symbol = entry.names.find(_.position == t.name.pos).get.symbol
        val expectedInput =
          Input.Denotation("(args: Array[String]): Unit", symbol)
        val infoSymbols = entry.symbols.find(_.symbol == symbol).get.denotation.names
        assert(infoSymbols.nonEmpty)
        infoSymbols.foreach {
          case ResolvedName(Position.Range(input, _, _), _, false) =>
            assert(input == expectedInput)
          case els =>
            sys.error(s"Unexpected $els")
        }
    }
    assert(syntheticAsserts.nonEmpty)

  }

  test("Database.synthetics") {
    val entry = database.documents.find(_.input.syntax.contains("Synthetic")).get
    val source = entry.input.parse[Source].get
    val syntheticAsserts = source.collect {
      case q"$term.stripPrefix($_)" =>
        val synthetic = entry.synthetics.find(_.position == term.pos).get
        assert(synthetic.names.nonEmpty)
    }
    assert(syntheticAsserts.nonEmpty)
  }

}

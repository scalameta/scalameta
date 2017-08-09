package scala.meta
package tests

class SugarSuite extends BaseSemanticSuite {

  test("Database.sugars") {
    implicit val database = Database.load(classpath, sourcepath)
    val entry = database.entries.find(_.input.syntax.contains("Sugar")).get
    val source = entry.input.parse[Source].get
    val sugarAsserts = source.collect {
      case q"$term.stripPrefix($_)" =>
        val sugar = entry.sugars.find(_.pos == term.pos).get
        assert(sugar.names.nonEmpty)
    }
    assert(sugarAsserts.nonEmpty)
  }

}

package scala.meta
package tests

class SugarSuite extends BaseSemanticSuite {

  test("Database.sugars") {
    implicit val mirror = Database.load(classpath, sourcepath)
    val attribute = mirror.entries.find(_.input.syntax.contains("Sugar")).get
    val sugarAsserts = attribute.source.collect {
      case q"$term.stripPrefix($_)" =>
        val sugar = term.sugar.get
        val arrayOpsNames = sugar.collect {
          case n @ q"augmentString" => n.asInstanceOf[Name].symbol
        }
        assert(arrayOpsNames.nonEmpty)
    }
    assert(sugarAsserts.nonEmpty)
  }

}

package scala.meta.tests.semanticdb

class SymbolDocstringSuite extends SemanticdbSuite {

  test("Class-level docstrings should be associated with the class symbol") {
    val document = computeDatabaseFromSnippet(
      """
        |/**
        | * This is a docstring for the class
        | */
        |case class Person(
        |  name: String,
        |  age: Int
        |)
      """.trim.stripMargin
    )
    assert(document.symbols.toList == Nil)
  }

  test(
    """
      | @param annotations on a class should be associated with the symbols for the associated
      | parameters of the constructor
    """.stripMargin
  ) {
    val document = computeDatabaseFromSnippet(
      """
        |/**
        | * This is a docstring for the class
        | *
        | * @param name this documents a parameter
        | * @param age
        | */
        |case class Person(
        |  name: String,
        |  age: Int
        |)
      """.trim.stripMargin
    )
    assert(document.symbols.toList == Nil)
  }

  test("@param annotations for a method should be associated with the symbols for parameters") {
    val document = computeDatabaseFromSnippet(
      """
        |object A {
        |  /**
        |   * This is a docstring for the class
        |   *
        |   * @param name this documents a parameter
        |   * @param age
        |   */
        |  def foobar(name: String, age: Int)
        |}
      """.trim.stripMargin
    )
    assert(document.symbols.toList == Nil)
  }
}

package scala.meta.tests.trees

import scala.meta._
import scala.meta.internal.trees.FieldAnnotations._
import scala.meta.internal.trees._

import munit.FunSuite

object AstVersionsSuite {

  @ast
  class Added(
      name: Term.Name,
      @newField("4.17.4")
      count: Int = 0,
  ) extends Tree

  @ast
  class Replaced(
      name: Term.Name,
      @replacesFields("4.17.4", FromValues)
      value: Term.Name,
  ) extends Tree {
    @replacedField("4.17.4")
    final def values: List[Term.Name] = List(value)
  }

  object FromValues {
    def apply(values: List[Term.Name]): Term.Name = values.head
  }

}

class AstVersionsSuite extends FunSuite {

  import AstVersionsSuite._

  private def assertSignatures(cls: Class[_], methods: String*)(
      expected: String*,
  )(implicit loc: munit.Location): Unit = {
    def show(tpe: java.lang.reflect.Type) = tpe.getTypeName.replaceAll("\\b([a-z]\\w*\\.)+", "")
      .replace('$', '.')
    val obtained = cls.getMethods.toSeq.filter(m => !m.isBridge && methods.contains(m.getName))
      .map(m =>
        m.getName + m.getGenericParameterTypes.map(show).mkString("(", ", ", "): ") +
          show(m.getGenericReturnType),
      )
    assertEquals(obtained.sorted, expected)
  }

  test("Added: field added at the frozen version") {
    assertSignatures(Added.getClass, "apply", "createWithComments", "newBuilder", "unapply")(
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name): AstVersionsSuite.Added",
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, Dialect): AstVersionsSuite.Added",
      "apply(Origin, Term.Name): AstVersionsSuite.Added",
      "apply(Origin, Term.Name, Dialect): AstVersionsSuite.Added",
      "apply(Term.Name): AstVersionsSuite.Added",
      "apply(Term.Name, Dialect): AstVersionsSuite.Added",
      "createWithComments(Term.Name, Option<Tree.Comments>, Option<Tree.Comments>, Dialect): AstVersionsSuite.Added",
      "newBuilder(Term.Name, Dialect): AstVersionsSuite.Added.teststreesAstVersionsSuiteAddedImpl",
      "unapply(AstVersionsSuite.Added): Option<Term.Name>",
    )
  }

  test("Added.Initial") {
    assertSignatures(Added.Initial.getClass, "apply", "createWithComments", "newBuilder", "unapply")(
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name): AstVersionsSuite.Added",
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, Dialect): AstVersionsSuite.Added",
      "apply(Origin, Term.Name): AstVersionsSuite.Added",
      "apply(Origin, Term.Name, Dialect): AstVersionsSuite.Added",
      "apply(Term.Name): AstVersionsSuite.Added",
      "apply(Term.Name, Dialect): AstVersionsSuite.Added",
      "createWithComments(Term.Name, Option<Tree.Comments>, Option<Tree.Comments>, Dialect): AstVersionsSuite.Added",
      "unapply(AstVersionsSuite.Added): Option<Term.Name>",
    )
  }

  test("Added.After_4_17_4")(
    assertSignatures(
      Added.After_4_17_4.getClass,
      "apply",
      "createWithComments",
      "newBuilder",
      "unapply",
    )("unapply(AstVersionsSuite.Added): Option<Tuple2<Term.Name, Object>>"),
  )

  test("Added.Builder") {
    assertSignatures(
      classOf[Added.Builder],
      "name",
      "count",
      "value",
      "origin",
      "begComment",
      "endComment",
      "result",
    )(
      "begComment(Option<Tree.Comments>): AstVersionsSuite.Added.teststreesAstVersionsSuiteAddedImpl",
      "count(int): AstVersionsSuite.Added.teststreesAstVersionsSuiteAddedImpl",
      "endComment(Option<Tree.Comments>): AstVersionsSuite.Added.teststreesAstVersionsSuiteAddedImpl",
      "name(Term.Name): AstVersionsSuite.Added.teststreesAstVersionsSuiteAddedImpl",
      "origin(Origin): AstVersionsSuite.Added.teststreesAstVersionsSuiteAddedImpl",
      "result(): AstVersionsSuite.Added",
    )
  }

  test("Added: tree") {
    assertSignatures(
      classOf[Added],
      "name",
      "count",
      "value",
      "values",
      "copy",
      "fullCopy",
      "copyWithComments",
      "toBuilder",
    )(
      "copy(Term.Name): AstVersionsSuite.Added",
      "copyWithComments(Term.Name, Option<Tree.Comments>, Option<Tree.Comments>): AstVersionsSuite.Added",
      "count(): int",
      "fullCopy(Term.Name, int, Option<Tree.Comments>, Option<Tree.Comments>): AstVersionsSuite.Added",
      "name(): Term.Name",
      "toBuilder(): AstVersionsSuite.Added.teststreesAstVersionsSuiteAddedImpl",
    )
  }

  test("Replaced: field replaced at the frozen version") {
    assertSignatures(Replaced.getClass, "apply", "createWithComments", "newBuilder", "unapply")(
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, List<Term.Name>): AstVersionsSuite.Replaced",
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, List<Term.Name>, Dialect): AstVersionsSuite.Replaced",
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, Term.Name): AstVersionsSuite.Replaced",
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, Term.Name, Dialect): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, List<Term.Name>): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, List<Term.Name>, Dialect): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, Term.Name): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, Term.Name, Dialect): AstVersionsSuite.Replaced",
      "apply(Term.Name, List<Term.Name>): AstVersionsSuite.Replaced",
      "apply(Term.Name, List<Term.Name>, Dialect): AstVersionsSuite.Replaced",
      "apply(Term.Name, Term.Name): AstVersionsSuite.Replaced",
      "apply(Term.Name, Term.Name, Dialect): AstVersionsSuite.Replaced",
      "createWithComments(Term.Name, List<Term.Name>, Option<Tree.Comments>, Option<Tree.Comments>, Dialect): AstVersionsSuite.Replaced",
      "createWithComments(Term.Name, Term.Name, Option<Tree.Comments>, Option<Tree.Comments>, Dialect): AstVersionsSuite.Replaced",
      "newBuilder(Term.Name, List<Term.Name>, Dialect): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
      "newBuilder(Term.Name, Term.Name, Dialect): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
      "unapply(AstVersionsSuite.Replaced): Option<Tuple2<Term.Name, List<Term.Name>>>",
    )
  }

  test("Replaced.Initial") {
    assertSignatures(
      Replaced.Initial.getClass,
      "apply",
      "createWithComments",
      "newBuilder",
      "unapply",
    )(
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, List<Term.Name>): AstVersionsSuite.Replaced",
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, List<Term.Name>, Dialect): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, List<Term.Name>): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, List<Term.Name>, Dialect): AstVersionsSuite.Replaced",
      "apply(Term.Name, List<Term.Name>): AstVersionsSuite.Replaced",
      "apply(Term.Name, List<Term.Name>, Dialect): AstVersionsSuite.Replaced",
      "createWithComments(Term.Name, List<Term.Name>, Option<Tree.Comments>, Option<Tree.Comments>, Dialect): AstVersionsSuite.Replaced",
      "unapply(AstVersionsSuite.Replaced): Option<Tuple2<Term.Name, List<Term.Name>>>",
    )
  }

  test("Replaced.After_4_17_4") {
    assertSignatures(
      Replaced.After_4_17_4.getClass,
      "apply",
      "createWithComments",
      "newBuilder",
      "unapply",
    )(
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, Term.Name): AstVersionsSuite.Replaced",
      "apply(Origin, Option<Tree.Comments>, Option<Tree.Comments>, Term.Name, Term.Name, Dialect): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, Term.Name): AstVersionsSuite.Replaced",
      "apply(Origin, Term.Name, Term.Name, Dialect): AstVersionsSuite.Replaced",
      "apply(Term.Name, Term.Name): AstVersionsSuite.Replaced",
      "apply(Term.Name, Term.Name, Dialect): AstVersionsSuite.Replaced",
      "createWithComments(Term.Name, Term.Name, Option<Tree.Comments>, Option<Tree.Comments>, Dialect): AstVersionsSuite.Replaced",
      "unapply(AstVersionsSuite.Replaced): Option<Tuple2<Term.Name, Term.Name>>",
    )
  }

  test("Replaced.Builder") {
    assertSignatures(
      classOf[Replaced.Builder],
      "name",
      "count",
      "value",
      "origin",
      "begComment",
      "endComment",
      "result",
    )(
      "begComment(Option<Tree.Comments>): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
      "endComment(Option<Tree.Comments>): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
      "name(Term.Name): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
      "origin(Origin): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
      "result(): AstVersionsSuite.Replaced",
      "value(Term.Name): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
    )
  }

  test("Replaced: tree") {
    assertSignatures(
      classOf[Replaced],
      "name",
      "count",
      "value",
      "values",
      "copy",
      "fullCopy",
      "copyWithComments",
      "toBuilder",
    )(
      "copy(Term.Name, List<Term.Name>): AstVersionsSuite.Replaced",
      "copy(Term.Name, Term.Name): AstVersionsSuite.Replaced",
      "copyWithComments(Term.Name, List<Term.Name>, Option<Tree.Comments>, Option<Tree.Comments>): AstVersionsSuite.Replaced",
      "copyWithComments(Term.Name, Term.Name, Option<Tree.Comments>, Option<Tree.Comments>): AstVersionsSuite.Replaced",
      "fullCopy(Term.Name, Term.Name, Option<Tree.Comments>, Option<Tree.Comments>): AstVersionsSuite.Replaced",
      "name(): Term.Name",
      "toBuilder(): AstVersionsSuite.Replaced.teststreesAstVersionsSuiteReplacedImpl",
      "value(): Term.Name",
      "values(): List<Term.Name>",
    )
  }

}

package scala.meta.tests.testkit
package contrib

import scala.meta._
import scala.meta.contrib._
import scala.meta.testkit._

import scala.concurrent.duration

class EqualProps extends munit.FunSuite {

  override def munitTimeout = duration.FiniteDuration(2, duration.MINUTES)

  test("isEqual > referential") {
    val errors = SyntaxAnalysis.onParsed[Tree](ContribSuite.corpus) { ast =>
      // empty transformation preserve structural equality
      val termNameTransformer: PartialFunction[Tree, Tree] = { case Term.Name(x) => Term.Name(x) }
      val typeNameTransformer: PartialFunction[Tree, Tree] = { case Type.Name(x) => Type.Name(x) }

      // We need to make sure that the tree will actually be transformed. or ref equality will be true
      val transformWillChangeRef = termNameTransformer.isDefinedAt(ast) ||
        typeNameTransformer.isDefinedAt(ast)

      val a = ast.transform(termNameTransformer)
      val b = ast.transform(typeNameTransformer)

      val refEqual = a.equals(b) && transformWillChangeRef // should be false
      val structuralEqual = a.isEqual(b) // should be true

      if (refEqual || !structuralEqual) List(a) else Nil
    }
    assert(errors.isEmpty)
  }
}

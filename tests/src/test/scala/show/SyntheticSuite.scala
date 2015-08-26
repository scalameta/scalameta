import org.scalatest._
import scala.meta._
import scala.meta.internal.{semantic => s}
import scala.meta.internal.ui.Attributes
import scala.meta.dialects.Scala211

class SyntheticSuite extends ParseSuite {
  test("comprehensive show[Attributes]") {
    val tree1 = q"List".asInstanceOf[scala.meta.internal.ast.Term.Name]
    val tree2 = tree1.withDenot(tree1.denot).withExpansion(q"scala.collection.immutable.List").withTyping(t"List.type")
    assert(tree2.show[Attributes] === """
      |Term.Name("List"){1}<1>
      |{1} Type.Singleton(Term.Name("List"))
      |<1> Term.Select(Term.Select(Term.Select(Term.Name("scala"), Term.Name("collection")), Term.Name("immutable")), Term.Name("List"))
    """.trim.stripMargin)
  }
}

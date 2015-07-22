import org.scalatest._
import scala.meta._
import scala.meta.internal.{semantic => s}
import scala.meta.dialects.Scala211

class SyntheticSuite extends ParseSuite {
  test("comprehensive show[Semantics]") {
    val tree1 = q"List".asInstanceOf[scala.meta.internal.ast.Term.Name]
    val tree2 = tree1.withDenot(tree1.denot).withExpansion(q"scala.collection.immutable.List").withTyping(t"List.type")
    assert(tree2.show[Semantics] === """
      |Term.Name("List")[1]{1}<1>
      |[1] Type.Singleton(Term.Name("package")[2])::scala.package.List
      |[2] Type.Singleton(Term.Name("scala")[3])::scala.package
      |[3] Type.Singleton(Term.Name("_root_")[7])::scala
      |[4] Type.Singleton(Term.Name("scala")[3])::scala.collection
      |[5] Type.Singleton(Term.Name("collection")[4])::scala.collection.immutable
      |[6] Type.Singleton(Term.Name("immutable")[5])::scala.collection.immutable.List
      |[7] 0::_root_
      |{1} Type.Singleton(Term.Name("List")[1])
      |<1> Term.Select(Term.Select(Term.Select(Term.Name("scala")[3], Term.Name("collection")[4]), Term.Name("immutable")[5]), Term.Name("List")[6])
    """.trim.stripMargin)
  }
}

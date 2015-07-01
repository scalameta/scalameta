import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tql._
import scala.meta.internal.{ ast => impl }
import org.scalatest.FunSuite

class InferPartialSuite extends FunSuite {
  test("TransformAndInferDef") {
    val testCode =
      """import dummy.test
        |/* This high-level comment should be persisted */
        |case class Test(string: String)
        |// TODO: this is a second high-level comment.
        |object Test {
        |  val string = "This is a string"
        |  def funToReplace(x: Int, y: String) {
        |    val myString = x.toString + ":" + y + string
        |    val res = {
        |                                  Test(myString) /* Just creating a new case object */
        |    }
        |    /* And yet another comment! */
        |  }
        |}
      """.stripMargin
    val testTree = testCode.parse[Source]

    // Forcing synthetic tokens using TQL
    val trans = (transform {
      case t: impl.Defn.Def if t.name.value == "funToReplace" =>
        t.copy(name = impl.Term.Name("aNewName"))
      case t: impl.Defn.Val =>
        t.copy()
      case t: impl.Defn.Object if t.name.value == "Test" =>
        t.copy(name = impl.Term.Name("Test2"))
    }).topDown

    val transformed = trans(testTree)
    val newCode = transformed.tree.get.tokens.map(_.show[Syntax]).mkString

    /* Cheking that highl-level comments are persisted */
    assert(newCode.contains("/* This high-level comment should be persisted */"))
    assert(newCode.contains("// TODO: this is a second high-level comment."))

    /* Checking that inner comment of unmodified parts are persisted */
    assert(newCode.contains("/* Just creating a new case object */"))

    /* Checking that indentation is persisted for unmodified part */
    assert(newCode.contains("                                  "))
  }

}

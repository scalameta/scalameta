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
        |object Test {
        |  val string = "This is a string"  
        |  def funToReplace(x: Int, y: String) {
        |    val myString = x.toString + ":" + y + string
        |    val res = {
        |      Test(myString) /* Just creating a new case object */
        |    }
        |    /* And yet another comment! */
        |  }
        |}
      """.stripMargin
    val testTree = testCode.parse[Source]

    // Forcing synthetic tokens using TQL
    val trans = (transform {
      case t: impl.Defn.Def if t.name.value == "funToReplace" => 
        t.copy(name = impl.Term.Name("aNewName")) andCollect Unit
      case t: impl.Defn.Val =>
        t.copy() andCollect Unit
    }).topDown

    val transformed = trans(testTree)
    val newCode = transformed.tree.get.tokens.map(_.show[Code]).mkString
    println("Printing original tokens:")
    println(testTree.tokens.map(_.code).mkString)
    println("-----------")
    println("Printing tokens corresponding to stats in Source:")
    testTree.asInstanceOf[impl.Source].stats.foreach {s => println(s.tokens.map(_.code).mkString + "\n")}
    println("-----------")
    println("Printing tokens after modification by TQL:")
    println(newCode)
    assert(newCode.contains("/* Just creating a new case object */"))
  }

}

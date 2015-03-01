import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tql._
import scala.meta.internal.{ast => impl}
import org.scalatest.FunSuite

class ObeyRuleSuite extends FunSuite {
  val propagandaCode = """
    package propaganda
    object Propaganda {
      def main(args: Array[String]) {
        println("Starting the Propaganda!")
        val u = 17
        def test {
          val x = 1
        }
        def test2 {
          val y = {
            val z = 19
            1
          }
          var yo = 2
        }
        var c = 3
        val v = List(1,2,3).toSet()
      }
    }
  """
  val propagandaTree = propagandaCode.parse[Source]

  //rule taken from the Obey project
  val listToSetBool = topDown(transform {
    case tt @ impl.Term.Apply(t @ impl.Term.Select(impl.Term.Apply(impl.Term.Name("List"), _), impl.Term.Name("toSet")), _) =>
      t andCollect tt.toString
  })

  test("listToSetBool") {
    val rewrittenTree = listToSetBool(propagandaTree)
    val rewrittenCode = rewrittenTree.tree.get
    assert(rewrittenCode != propagandaCode)
  }
}

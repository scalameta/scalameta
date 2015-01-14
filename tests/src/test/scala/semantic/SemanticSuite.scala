import org.scalatest._
import scala.meta._
import scala.meta.semantic._

class SemanticSuite extends FunSuite {
  def dontExecuteJustCompile() = {
    implicit val c: Context = ???
    val term1: Term = ???
    term1.tpe
    val term2: Term with Type = ???
    term2.tpe
  }
}

import org.scalatest._
import scala.{meta => api}
import scala.meta.internal.{ast => impl}

class SemanticSuite extends FunSuite {
  def dontExecuteJustCompile() = {
    import scala.meta.semantic._
    implicit val c: Context = ???
    val term1a: api.Term = ???
    term1a.tpe
    val term2a: api.Term with api.Type = ???
    term2a.tpe
    val term3a: api.Term.Ref = ???
    term3a.tpe
    val term1b: impl.Term = ???
    term1b.tpe
    val term2b: impl.Term with impl.Type = ???
    term2b.tpe
    val term3b: impl.Term.Ref = ???
    term3b.tpe
  }
}

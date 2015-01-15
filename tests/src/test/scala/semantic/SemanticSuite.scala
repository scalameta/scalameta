import org.scalatest._
import scala.{meta => api}
import scala.meta.internal.{ast => impl}

class SemanticSuite extends FunSuite {
  def dontExecuteJustCompile() = {
    import scala.meta.semantic._
    implicit val c: Context = ???
    val term1a: api.Term = ???
    (term1a.tpe: api.Type)
    val term2a: api.Term with api.Type = ???
    (term2a.tpe: api.Type)
    val term3a: api.Term.Ref = ???
    (term3a.tpe: api.Type)
    val term1b: impl.Term = ???
    (term1b.tpe: api.Type)
    val term2b: impl.Term with impl.Type = ???
    (term2b.tpe: api.Type)
    val term3b: impl.Term.Ref = ???
    (term3b.tpe: api.Type)
    (typeOf[Int]: api.Type.Ref)
    (typeOf[Int]: impl.Type.Ref)
    (termOf(Int): api.Term.Ref)
    (termOf(Int): impl.Term.Ref)
  }
}

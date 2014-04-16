import org.scalatest.FunSuite

class BasicSuite extends FunSuite {
  test("everything works") {
    import scala.reflect.Tree._
    import scala.reflect.Tree.Term.{Ident => TermIdent, _}
    import scala.reflect.Tree.Type.{Ident => TypeIdent, _}
    val parent1 = Parent(TypeIdent(""), List(List()))
    Template(Nil, List(parent1, parent1), Self(None, None), Nil)
  }
}
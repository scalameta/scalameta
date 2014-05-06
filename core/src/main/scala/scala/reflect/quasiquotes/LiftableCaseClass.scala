package scala.reflect.quasiquotes
import org.scalareflect.LiftableMaterializer

object Materialized {
  val materializer = new { val u: reflect.runtime.universe.type = reflect.runtime.universe } with LiftableMaterializer
  val instances = materializer.materialize[reflect.core.Tree]

}

object Test extends App {
  import reflect.runtime.universe.Quasiquote
  import reflect.core._, Aux._
  import Materialized.instances._
  val t =  Decl.Type(Nil, Type.Ident("F", false),
                 TypeParam(Nil, Some(Type.Ident("T", false)),
                           Nil, Nil, Nil, TypeBounds.empty) :: Nil,
                 TypeBounds.empty)
  println(q"$t")
}

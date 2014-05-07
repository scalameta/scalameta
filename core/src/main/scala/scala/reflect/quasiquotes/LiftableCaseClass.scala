package scala.reflect.quasiquotes
import org.scalareflect.LiftableMaterializer

object Materialized {
  val materializer = new { val u: reflect.runtime.universe.type = reflect.runtime.universe } with LiftableMaterializer
  val instances = materializer.materialize[reflect.core.Tree]
}

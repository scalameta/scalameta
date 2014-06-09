import org.scalatest._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class AdtSuite extends FunSuite {
  object AdtReflection extends { val u: ru.type = ru } with org.scalareflect.adt.AdtReflection
  def symbolOf[T: TypeTag]: TypeSymbol = ru.symbolOf[T]
}

package scala.meta.tests
package dialects

import org.scalatest._
import scala.meta.Dialect

class ReflectionSuite extends FunSuite {
  test("Dialect.standards") {
    val dialects = scala.meta.dialects.`package`
    def isDialectGetter(m: java.lang.reflect.Method) = m.getParameterTypes.isEmpty && m.getReturnType == classOf[Dialect]
    val dialectGetters = dialects.getClass.getDeclaredMethods.filter(isDialectGetter)
    val reflectiveStandards = dialectGetters.map(m => (m.getName, m.invoke(dialects).asInstanceOf[Dialect])).toMap
    assert(Dialect.standards === reflectiveStandards)
  }
}
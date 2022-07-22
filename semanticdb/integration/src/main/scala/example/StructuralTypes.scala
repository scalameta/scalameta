package example

import scala.languageFeature.reflectiveCalls

object StructuralTypes {
  val a = new {
    val b = 1
  }
  a.b
}

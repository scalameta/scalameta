package docs

import scala.meta._

//noinspection ScalaUnusedSymbol
// This is a summy class to record a screenshot for the landing page
object LandingPage {

  val program = q"a"

  program match {
    case q"object $name extends App { ..$statements }" =>
      println("name = " + name.value)
  }

}

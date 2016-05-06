package scala.meta.tests
package dialects

import org.scalatest.FunSuite
import scala.meta.Dialect
import scala.meta.dialects.{QuasiquoteTerm, QuasiquotePat}

class ReflectionSuite extends FunSuite {
  test("Dialect.forName") {
    val regularDialects = Dialect.all
    val quasiquotingDialects = {
      for {
        dialect <- Dialect.all
        multiline <- List(false, true)
        unquoteParser <- List("Term", "Pat")
      } yield {
        if (unquoteParser == "Term") QuasiquoteTerm(dialect, multiline)
        else QuasiquotePat(dialect, multiline)
      }
    }
    val comprehensiveDialects = regularDialects ++ quasiquotingDialects
    comprehensiveDialects.foreach(d => {
      val d1 = Dialect.forName(d.name)
      assert(d1 === d)
    })
  }
}
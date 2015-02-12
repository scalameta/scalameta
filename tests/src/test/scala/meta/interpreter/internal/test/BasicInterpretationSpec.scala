package scala.meta.interpreter.internal.test

import org.scalatest._
import scala.meta.semantic._
import scala.meta._
import scala.meta.internal.interpreter.Interpreter

class BasicInterpretationSpec extends FlatSpec {

  def metaExpression(expression: String): Term = {
    import scala.meta._
    import scala.meta.internal.hosts.scalac.Scalahost

    implicit val c = Scalahost.mkToolboxContext(scala.reflect.runtime.currentMirror)
    // TODO: workaround for the exception that define("x + 1") reises
    val expresion = c.define(s"""class Z {
        def main: Any = {
          $expression
        }
     }""")

    import scala.meta.internal.ast.Defn
    val body = expresion match {
      case Defn.Class(mods, name, smth, ctor, template) =>
        val Some(List(Defn.Def(Nil, name, Nil, Nil, Some(_), body: Term))) = template.stats
        body
    }
    body
  }

  def metaDefine(classes: List[String]): List[Tree] = {
    import scala.meta._
    import scala.meta.internal.hosts.scalac.Scalahost

    implicit val c = Scalahost.mkToolboxContext(scala.reflect.runtime.currentMirror)
    classes.map(clazz => c.define(clazz))
  }

  def interpret(expression: String, classes: List[String] = Nil): Any =
    Interpreter.eval(metaExpression(expression))

  "An interpreter" should "execute simple expressions" in {

    intercept[RuntimeException] {
      interpret("""val x = 1; x + 1""")
    }

  }
}


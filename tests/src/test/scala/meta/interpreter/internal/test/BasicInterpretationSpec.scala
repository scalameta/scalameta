package scala.meta.interpreter.internal.test

import org.scalatest._
import scala.meta.semantic._
import scala.meta.internal.interpreter.Interpreter
import scala.meta.dialects.Scala211
import scala.meta._
import scala.meta.ui._
import scala.meta.internal.{ ast => impl }
import scala.meta.internal.hosts.scalac.contexts.StandaloneContext

class BasicInterpretationSpec extends FlatSpec with ShouldMatchers {

  def metaExpression(expression: String)(implicit c: StandaloneContext): Term = {
    val expresion: Tree = c.define(s"""class Z {
        def main: Any = {
          $expression
        }
     }""")

    val body = expresion match {
      case impl.Source(List(impl.Defn.Class(mods, name, smth, ctor, template))) =>
        val Some(List(impl.Defn.Def(Nil, name, Nil, Nil, Some(_), body: Term))) = template.stats
        body
    }
    body
  }

  def metaDefine(classes: List[String])(implicit c: StandaloneContext): List[Tree] = {
    import scala.meta._
    import scala.meta.internal.hosts.scalac.Scalahost
    val options = "-cp " + System.getProperty("sbt.paths.tests.classpath")
    implicit val c = Scalahost.mkStandaloneContext(options)
    classes.map(clazz => c.define(clazz))
  }

  def interpret(expression: String, classes: List[String] = Nil)(implicit c: StandaloneContext): Any =
    Interpreter.eval(metaExpression(expression))

  import scala.meta._
  import scala.meta.internal.hosts.scalac.Scalahost
  val options = "-cp " + System.getProperty("sbt.paths.tests.classpath")
  implicit val c = Scalahost.mkStandaloneContext(options)

  "An interpreter" should "execute simple expressions" in {

    interpret("""val x = 1; x + 1""") should be(2)
    interpret("""List(1,2,3)""") should be(List(1, 2, 3))
    interpret("""List(List(1),List(2),List(3))""") should be(List(List(1), List(2), List(3)))
    interpret("""if(true) {val x = 1; x + 1} else {val y = 1; y + 1}""")

    // Higher-order functions
    interpret("""val f = (x: String) => x + "2"; f("4")""")

    // String interpolators
    // interpret("""s"Begin: ${val x = 1; x +1} end"""") should be(2)

    // Using higher-order functions.
    // interpret("""val lst = List(1,2,3); lst.map(x => x + 1)""")
    // interpret("""List((1,2), (2,3), (3,4)).map(x => x._1 + 1)

  }

}


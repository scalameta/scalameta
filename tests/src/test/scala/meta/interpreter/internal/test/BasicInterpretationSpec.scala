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
    // TODO: workaround for the exception that define("x + 1") reises
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

  }
}


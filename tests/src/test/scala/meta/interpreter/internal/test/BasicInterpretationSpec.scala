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
  }

  it should "handle method calls on objects" in {
    interpret("""List(1,2,3)""") should be(List(1, 2, 3))
    interpret("""List(List(1),List(2),List(3))""") should be(List(List(1), List(2), List(3)))
  }

  it should "handle method calls on objects on functions with multiple params" in {
    interpret("""val tpl = Tuple2.apply(2,3); tpl._1""") should be(2)
  }

  it should "deal with conditionals" in {
    interpret("""if(true) {val x = 1; x + 1} else {val y = 1; y + 1}""")
  }

  it should "work with tuples" in {
    interpret("""List((1,2), (2,3), (3,4))""") should be(List((1, 2), (2, 3), (3, 4)))
  }

  it should "support calling functions" in {
    interpret("""val x = List(1,2,3); x take 2""") should be(List(1, 2))
  }

  it should "handle higher-order functions" in {
    //Higher-order functions
    interpret("""val f = (x: String) => x + "2"; f("4")""")
  }

  it should "handle string interpolation" in {
    interpret(""" s"Begin ${val x = 1; x + 1} the end" """) should be("Begin 2 the end")
  }

  it should "handle pattern matching" in {
    interpret("""s"Hi" match {case x: String => x}""") should be("Hi")
  }

  it should "handle accessing package objects" in {
    val e = intercept[RuntimeException] {
      interpret("""sys.error("Catch me outside!")""")
    }
    e.getMessage() should be("Catch me outside!")
  }

  it should "support passing higher-order functions to collections" in {
    interpret("""val lst = List(1,2,3); lst.map(x => x + 1)""")
  }

}

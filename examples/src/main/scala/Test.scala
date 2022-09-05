import scala.meta._
import scala.meta.prettyprinters.Show

object Test extends App {
  type N  = Int

  println("Start")
  val program = """object Main extends App { print("Hello!") }"""
  //val tree = program.parse[Source].get

  val expr = """
               |object Test{
               |  def foo[T](x: T)[U](y: U) = x
               |  def bar[T](x: T)[U](y: U): (T,U)
               |  def test(x: Int): Int
               |  foo[Int]
               |  foo[Int](4)[String]("hello")
               |}
               |""".stripMargin
  
  val withDialect = dialects.Scala3Experimental(expr)
  val tree = withDialect.parse[Source].get


  println(tree.syntax)
  println(tree.structure)





  println("End")
}

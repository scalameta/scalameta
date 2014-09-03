import org.scalatest._

class ScalaToMeta extends FunSuite with ToolboxHarness {
  test("definition of old Join") {
    val result = typecheckConvertAndPrettyprint("""
      |import scala.reflect.macros.whitebox._
      |import scala.language.experimental.macros
      |object Join {
      |  def impl(c: Context)(x: c.Tree, y: c.Tree) = {
      |    import c.universe._
      |    def fields(tree: Tree) = tree.tpe.members.collect{ case m: TermSymbol if m.isGetter => m }
      |    val xfields = fields(x).map(f => f -> q"xtemp")
      |    val yfields = fields(y).map(f => f -> q"ytemp")
      |    val getters = (xfields ++ yfields).map{ case (f, ref) => q"val ${f.name} = $ref.${f.name}" }
      |    qQQQ
      |      val xtemp = $x
      |      val ytemp = $y
      |      new { ..$getters }
      |    QQQ
      |  }
      |  def apply[T, U](x: T, y: U): Any = macro impl
      |}
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
    assert(result === """
      |import scala.reflect.macros.whitebox._
      |import scala.language.experimental.macros
      |object Join {
      |  def impl(c: Context)(x: c.Tree, y: c.Tree) = {
      |    import c.universe._
      |    def fields(tree: Tree) = tree.tpe.members.collect({
      |      case m: TermSymbol if m.isGetter =>
      |        m
      |    })
      |    val xfields = fields(x).map(f => f -> q"xtemp")
      |    val yfields = fields(y).map(f => f -> q"ytemp")
      |    val getters = (xfields ++ yfields).map({
      |      case (f, ref) =>
      |        q"val ${f.name} = $ref.${f.name}"
      |    })
      |    qQQQ
      |      val xtemp = $x
      |      val ytemp = $y
      |      new { ..$getters }
      |    QQQ
      |  }
      |  def apply[T, U](x: T, y: U): Any = macro Join.impl
      |}
    """.trim.stripMargin.replace("QQQ", "\"\"\""))
  }
}

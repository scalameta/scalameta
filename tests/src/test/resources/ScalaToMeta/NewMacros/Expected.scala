import org.scalatest._
import scala.language.reflectiveCalls
import scala.language.experimental.macros
import scala.meta._
import scala.meta.semantic._
import scala.meta.semantic.errors.throwExceptions
class NewMacros extends FunSuite {
  test("new join") {
    def join[T, U](x: T, y: U): Any = macro {
      val xfields = x.tpe.vals.map(f => f -> q"xtemp")
      val yfields = y.tpe.vals.map(f => f -> q"ytemp")
      val getters = (xfields ++ yfields).map({
        case (f, ref) =>
          q"val ${f.name} = $ref.${f.name}"
      })
      c.whitebox(q"""
        val xtemp = $x
        val ytemp = $y
        new { ..$getters }
      """)
    }
    val result = join(new { val x = 2 }, new { val y = 3 })
    assert(result.x === 2)
    assert(result.y === 3)
  }
}
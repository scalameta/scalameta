import org.scalatest._

class NewSuite extends FunSuite {
  test("new join") {
    // TODO: make this work
    // def join[T, U](x: T, y: U): Any = macro {
    //   import scala.reflect.core._
    //   import scala.reflect.semantic._
    //   val xfields = x.tpe.vals.map(f => f -> q"xtemp")
    //   val yfields = y.tpe.vals.map(f => f -> q"ytemp")
    //   val getters = (xfields ++ yfields).map{ case (f, ref) => q"val ${f.name} = $ref.${f.name}" }
    //   q"""
    //     val xtemp = $x
    //     val ytemp = $y
    //     new { ..$getters }
    //   """
    // }
  }
}
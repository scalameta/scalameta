import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Hello {
  def world: String = macro impl
  def impl(c: Context) = {
    import c.universe._
    q""" "Hello world!" """
  }
}
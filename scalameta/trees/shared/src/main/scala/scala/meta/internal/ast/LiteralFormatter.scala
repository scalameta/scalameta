package scala.meta.internal.ast

trait LiteralFormatter[T] {
  def format(e: T): String
}
object LiteralFormatter {

}

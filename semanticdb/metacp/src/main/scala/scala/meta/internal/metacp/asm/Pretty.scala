package scala.meta.internal.metacp.asm

trait Pretty {
  def print(sb: StringBuilder): Unit
  final def pretty: String = {
    val sb = new StringBuilder
    this.print(sb)
    sb.toString
  }
}

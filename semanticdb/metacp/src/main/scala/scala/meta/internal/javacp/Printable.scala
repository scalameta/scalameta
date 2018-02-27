package scala.meta.internal.javacp

/** Helper to print parsed JavaTypeSignature back into original signature */
trait Printable {
  def print(sb: StringBuilder): Unit
  final def pretty: String = {
    val sb = new StringBuilder
    this.print(sb)
    sb.toString
  }
}

package lang.meta
package semanticdb

import scala.compat.Platform.EOL
import lang.meta.inputs._
import lang.meta.internal.inputs._

final case class Attributes(
  input: Input,
  language: String,
  names: List[ResolvedName],
  messages: List[Message],
  symbols: List[ResolvedSymbol],
  sugars: List[Sugar]
) {
  def syntax: String = {
    val lines = scala.collection.mutable.ListBuffer[String]()
    def appendSection(name: String, section: Seq[String]): Unit = {
      if (section.nonEmpty) {
        lines += (name + ":")
        lines ++= section
        lines += ""
      }
    }
    appendSection("Language", List(language))
    appendSection("Names", names.sortBy(_.pos.start).map(_.syntax))
    appendSection("Messages", messages.sortBy(_.pos.start).map(_.syntax))
    appendSection("Symbols", symbols.sortBy(_.symbol.syntax).map(_.syntax))
    appendSection("Sugars", sugars.sortBy(_.pos.start).map(_.syntax))
    lines.mkString(EOL)
  }

  def structure: String = ???

  override def toString: String = syntax
}

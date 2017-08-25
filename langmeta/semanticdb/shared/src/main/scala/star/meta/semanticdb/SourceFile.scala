package org.langmeta
package semanticdb

import scala.compat.Platform.EOL
import org.langmeta.inputs._
import org.langmeta.internal.inputs._

final case class SourceFile(
  input: Input,
  language: String,
  names: List[ResolvedName],
  messages: List[Message],
  symbols: List[ResolvedSymbol],
  synthetics: List[Synthetic]
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
    appendSection("Names", names.sortBy(_.position.start).map(_.syntax))
    appendSection("Messages", messages.sortBy(_.position.start).map(_.syntax))
    appendSection("Symbols", symbols.sortBy(_.symbol.syntax).map(_.syntax))
    appendSection("Synthetics", synthetics.sortBy(_.position.start).map(_.syntax))
    lines.mkString(EOL)
  }

  def structure: String = ???

  override def toString: String = syntax
}

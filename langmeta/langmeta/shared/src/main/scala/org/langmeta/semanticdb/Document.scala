package org.langmeta.semanticdb

import scala.compat.Platform.EOL
import scala.math.Ordering
import org.langmeta.inputs._
import org.langmeta.internal.inputs._

final case class Document(
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
    appendSection("Names", names.sorted.map(_.syntax))
    appendSection("Messages", messages.sorted.map(_.syntax))
    appendSection("Symbols", symbols.sorted.map(_.syntax))
    appendSection("Synthetics", synthetics.sorted.map(_.syntax))
    lines.mkString(EOL)
  }

  private implicit def positionOrder: Ordering[Position] =
    Ordering.by(p => (p.start, p.end))
  private implicit def resolvedNameOrder: Ordering[ResolvedName] =
    Ordering.by(n => (n.position, n.syntax))
  private implicit def messageOrder: Ordering[Message] =
    Ordering.by(m => (m.position, m.syntax))
  private implicit def resolvedSymbolOrder: Ordering[ResolvedSymbol] =
    Ordering.by(_.syntax)
  private implicit def synthOrder: Ordering[Synthetic] =
    Ordering.by(s => (s.position, s.syntax))

  def structure: String = ???

  override def toString: String = syntax
}

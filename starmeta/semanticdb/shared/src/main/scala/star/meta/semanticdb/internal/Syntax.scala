package star.meta.internal
package semanticdb

import star.meta.inputs._
import star.meta.semanticdb._
import scala.collection.mutable
import scala.compat.Platform.EOL
import star.meta.internal.io.PathIO

object DatabaseSyntax {
  def apply(database: Database): String = {
    val s_entries = database.entries.map { attrs =>
        val s_input = PathIO.toUnix(attrs.input.syntax)
        val separator = EOL + "-" * s_input.toString.length + EOL
        s_input + separator + attrs.syntax
    }
    s_entries.mkString(EOL + EOL)
  }
}

object AttributesSyntax {
  def apply(attrs: Attributes): String = {
    import attrs._

    val lines = mutable.ListBuffer[String]()
    def appendSection(name: String, section: List[String]): Unit = {
      if (section.nonEmpty) {
        lines += (name + ":")
        lines ++= section
        lines += ""
      }
    }
    implicit class XtensionPositionRange(pos: Position) {
      def range = s"[${pos.start}..${pos.end})"
    }

    val s_language = language
    appendSection("Language", List(s_language))

    val s_names = names.toList.sortBy(_._1.start).map {
      case ((pos, symbol)) =>
        val text = if (pos.text.nonEmpty) pos.text else "ε"
        s"${pos.range}: $text => $symbol"
    }
    appendSection("Names", s_names)

    val s_messages = messages.toList.sortBy(_.position.start).map {
      case Message(pos, severity, message) =>
        s"${pos.range}: [${severity.toString.toLowerCase}] ${message}"
    }
    appendSection("Messages", s_messages)

    val s_denots = denotations.toList.sortBy(_._1.syntax).map {
      case ((symbol, denotation)) =>
        s"$symbol => $denotation"
    }
    appendSection("Denotations", s_denots)

    val s_sugars = sugars.toList.sortBy(_._1.start).map {
      case ((pos, sugar)) =>
        val sugar_names =
          if (sugar.names.isEmpty) ""
          else {
            sugar.names.map {
              case (pos, symbol) => s"  ${pos.range}: ${pos.text} => $symbol"
            }.mkString("\n", "\n", "")
          }
        s"${pos.range}: ${sugar.syntax}$sugar_names"
    }
    appendSection("Sugars", s_sugars)

    lines.mkString(EOL)
  }
}

object FlagSyntax {
  def apply(flags: Long): String = {
    val buf = new StringBuilder
    def append(flag: String) = {
      if (buf.isEmpty) buf ++= flag
      else buf ++= (" " + flag)
    }
    def hasFlag(flag: Long) = (flags & flag) == flag
    if (hasFlag(PRIVATE)) append("PRIVATE")
    if (hasFlag(PROTECTED)) append("PROTECTED")
    if (hasFlag(ABSTRACT)) append("ABSTRACT")
    if (hasFlag(FINAL)) append("FINAL")
    if (hasFlag(SEALED)) append("SEALED")
    if (hasFlag(IMPLICIT)) append("IMPLICIT")
    if (hasFlag(LAZY)) append("LAZY")
    if (hasFlag(CASE)) append("CASE")
    if (hasFlag(COVARIANT)) append("COVARIANT")
    if (hasFlag(CONTRAVARIANT)) append("CONTRAVARIANT")
    if (hasFlag(INLINE)) append("INLINE")
    if (hasFlag(VAL)) append("VAL")
    if (hasFlag(VAR)) append("VAR")
    if (hasFlag(DEF)) append("DEF")
    if (hasFlag(PRIMARYCTOR)) append("PRIMARYCTOR")
    if (hasFlag(SECONDARYCTOR)) append("SECONDARYCTOR")
    if (hasFlag(MACRO)) append("MACRO")
    if (hasFlag(TYPE)) append("TYPE")
    if (hasFlag(PARAM)) append("PARAM")
    if (hasFlag(TYPEPARAM)) append("TYPEPARAM")
    if (hasFlag(OBJECT)) append("OBJECT")
    if (hasFlag(PACKAGE)) append("PACKAGE")
    if (hasFlag(PACKAGEOBJECT)) append("PACKAGEOBJECT")
    if (hasFlag(CLASS)) append("CLASS")
    if (hasFlag(TRAIT)) append("TRAIT")
    buf.toString.toLowerCase
  }
}
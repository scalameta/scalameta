package org.scalameta

class FileLine(val file: sourcecode.File, val line: sourcecode.Line) {
  override def toString: String = {
    val shortFilename = file.value.replaceAll("(.*/|\\.scala)", "")
    Console.GREEN + s"$shortFilename:${line.value}" + Console.RESET
  }
}

object FileLine {
  implicit def generate(implicit file: sourcecode.File, line: sourcecode.Line): FileLine =
    new FileLine(file, line)
}

object logger {
  /** Same as println except includes the file+line number of call-site. */
  def debug(x: Any)(implicit fileLine: FileLine): Unit = {
    println(s"$fileLine $x")
  }

  /** Replaces whitespace characters with non-whitespace characters */
  def revealWhitespace(s: String): String = s.map {
    case '\t' => '†'
    case '\n' => '¶'
    case ' ' => '∙'
    case ch => ch
  }

  /** Prints out the value with and it's source code representation
    *
    * Example: logger.elem(x) // prints "MyFile:24 [x]: 42"
    **/
  def elem(values: sourcecode.Text[Any]*)(implicit fileLine: FileLine): Unit = {
    values.foreach { t =>
      val value = {
        val str = s"${t.value}"
        if (str.contains("\n")) s"\n$str"
        else str
      }
      println(s"$fileLine [${t.source}]: $value")
    }
  }
}

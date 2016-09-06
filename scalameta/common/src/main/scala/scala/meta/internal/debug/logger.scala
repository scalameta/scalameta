package scala.meta.internal.debug

import java.io.File

object logger {

  /** logger.ping() prints out the current filename:lineNumber methodName methodArgs */
  def ping()(
      implicit line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing,
      args: sourcecode.Args
  ): Unit = {
    println(f"$getPosition%-25s PING ${enclosing.value} ${args.value}")
  }

  /** logger.elem(myVar) prints out filename:lineNumber [myVar] ${myVar.value} */
  def elem(ts: sourcecode.Text[Any]*)(
      implicit line: sourcecode.Line,
      file: sourcecode.File,
      enclosing: sourcecode.Enclosing): Unit = {
    ts.foreach { t =>
      log(t, line, file, enclosing, showSource = true)
    }
  }

  /** Highlights spaces and newlines of a string. */
  def reveal(s: String): String = s.map {
    case '\n' => '¶'
    case ' ' => '∙'
    case ch => ch
  }

  /** A big heading to indicate the start of a new part. */
  def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  private def log[T](t: sourcecode.Text[T],
                     line: sourcecode.Line,
                     file: sourcecode.File,
                     enclosing: sourcecode.Enclosing,
                     showSource: Boolean): Unit = {
    val position = getPosition(line, file)
    val key =
      if (showSource) s"[${t.source}]: ${t.value}"
      else t.value
    println(f"$position%-25s $key")
  }

  private def getPosition(
      implicit line: sourcecode.Line,
      file: sourcecode.File
  ): String = f"${new File(file.value).getName}:${line.value}"

}

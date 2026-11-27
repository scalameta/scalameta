package scala.meta.internal.common

import org.scalameta.internal.ScalaCompat

object Implicits {

  implicit class ImplicitString(private val value: String) extends AnyVal {
    def tq(repl: String = "QQQ"): String = value.replace(repl, "\"\"\"")
    def lf2nl: String = if (ScalaCompat.EOL == "\n") value else value.replace("\n", ScalaCompat.EOL)
    def nl2lf: String = if (ScalaCompat.EOL == "\n") value else value.replace(ScalaCompat.EOL, "\n")
  }

}

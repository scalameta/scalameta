package scala.meta.internal

import java.io._

package object cli {
  val devnull: PrintStream = {
    val os = new OutputStream { override def write(b: Int) = () }
    new PrintStream(os)
  }
}

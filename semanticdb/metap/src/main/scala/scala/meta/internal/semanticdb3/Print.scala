package scala.meta.internal.semanticdb3

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.meta.cli._
import scala.meta.internal.metap.Main
import scala.meta.metap.Settings

object Print {

  def document(doc: TextDocument): String = {
    val baos = new ByteArrayOutputStream()
    val main = new Main(Settings(), Reporter().withOut(new PrintStream(baos)))
    main.pprint(doc)
    baos.toString()
  }

}

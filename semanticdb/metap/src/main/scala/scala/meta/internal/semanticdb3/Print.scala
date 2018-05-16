package scala.meta.internal.semanticdb3

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import java.io.OutputStream
import scala.meta.internal.metap.Main
import scala.meta.metap.Reporter
import scala.meta.metap.Settings

object Print {

  private object DevNull
      extends PrintStream(new OutputStream() {
        override def write(b: Int): Unit = {}
      })
  private lazy val noopMain = new Main(Settings(), Reporter().withOut(DevNull))

  def document(doc: TextDocument): String = {
    val baos = new ByteArrayOutputStream()
    val main = new Main(Settings(), Reporter().withOut(new PrintStream(baos)))
    main.pprint(doc)
    baos.toString()
  }

  def tpe(doc: TextDocument, tpe: Type): String = noopMain.pprint(tpe, doc).mkString

  def range(range: Range, doc: Option[TextDocument] = None): String = noopMain.pprint(range, doc)
}

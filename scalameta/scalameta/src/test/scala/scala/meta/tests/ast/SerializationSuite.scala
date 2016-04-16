package scala.meta.tests
package ast

import org.scalatest._
import java.nio.charset.Charset
import java.io._
import scala.meta._
import scala.meta.dialects.Scala211

class SerializationSuite extends FunSuite {
  private def tryRoundtrip(x: Any): Unit = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(x)
    oos.close()
    baos.close()
    // TODO: commented out because of an exception described in:
    // https://groups.google.com/forum/#!topic/scala-user/6aLchfkzEH4
    //
    // val bais = new ByteArrayInputStream(baos.toByteArray)
    // val ois = new ObjectInputStream(bais)
    // ois.readObject.asInstanceOf[Source]
    // ois.close()
    // bais.close()
  }

  test("Input.String-based trees are serializable") {
    val source = "class C".parse[Source]
    tryRoundtrip(source)
  }

  test("Input.Stream-based trees are serializable") {
    val stream = new java.io.ByteArrayInputStream("class C".getBytes(Charset.forName("UTF-8")))
    val source = stream.parse[Source]
    tryRoundtrip(source)
  }

  test("Input.File-based trees are serializable") {
    val file = File.createTempFile("dummy", ".scala")
    file.deleteOnExit()
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write("class C")
    writer.close()
    val source = file.parse[Source]
    tryRoundtrip(source)
  }
}
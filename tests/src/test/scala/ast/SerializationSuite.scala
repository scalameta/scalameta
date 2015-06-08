import org.scalatest._
import java.io._
import scala.meta._
import scala.meta.dialects.Scala211

class SerializationSuite extends FunSuite {
  private def trySerialize(x: Any): Unit = {
    val baos = new ByteArrayOutputStream();
    val oos = new ObjectOutputStream(baos);
    oos.writeObject(x);
    oos.close();
    baos.close();
  }

  test("Input.String-based trees are serializable") {
    val source = "class C".parse[Source]
    trySerialize(source)
  }

  test("Input.File-based trees are serializable") {
    val file = File.createTempFile("dummy", ".scala")
    file.deleteOnExit()
    val writer = new BufferedWriter(new FileWriter(file))
    writer.write("class C")
    writer.close()
    val source = file.parse[Source]
    trySerialize(source)
  }
}
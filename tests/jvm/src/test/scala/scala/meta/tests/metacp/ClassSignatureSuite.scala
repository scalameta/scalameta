package scala.meta.tests.metacp

import scala.tools.asm.signature.SignatureReader
import scala.tools.asm.tree.ClassNode

class ClassSignatureSuite extends BaseSignatureSuite {
  def assertClassRoundtrip(signature: String): Unit = {
    val signatureReader = new SignatureReader(signature)
    val visitor = new ClassSignatureVisitor
    signatureReader.accept(visitor)
    val classSignature = visitor.result()
    //      pprint.log(classSignature)
    val obtained = classSignature.pretty
    assertNoDiff(obtained, signature)
  }

  def checkClassSignature(name: String, classpath: () => String): Unit = {
    checkSignatureCallback(name, classpath) { node: ClassNode =>
      List(
        (node.signature, { () =>
          if (node.signature != null) {
            assertClassRoundtrip(node.signature)
          }
        })
      )
    }
  }

  def checkClassRoundtrip(signature: String): Unit = {
    test(signature) { assertClassRoundtrip(signature) }
  }

  def checkClassSignatureLibrary(coordinates: Coordinates): Unit = {
    checkClassSignature(
      "class-" + coordinates.name,
      coordinates.classpath
    )
  }

  checkClassSignatureLibrary(scalameta)
  checkClassSignatureLibrary(akka)
  checkClassSignatureLibrary(spark)

}

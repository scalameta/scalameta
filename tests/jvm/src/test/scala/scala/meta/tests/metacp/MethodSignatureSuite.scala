package scala.meta.tests.metacp

import scala.tools.asm.signature.SignatureReader
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.MethodNode
import scala.collection.JavaConverters._

class MethodSignatureSuite extends BaseSignatureSuite {
  def assertMethodRoundtrip(signature: String): Unit = {
    val signatureReader = new SignatureReader(signature)
    val visitor = new MethodSignatureVisitor
    signatureReader.accept(visitor)
    val classSignature = visitor.result()
    //      pprint.log(classSignature)
    val obtained = classSignature.pretty
    assertNoDiff(obtained, signature)
  }

  def checkMethodSignature(name: String, classpath: () => String): Unit = {
    checkSignatureCallback(name, classpath) { node: ClassNode =>
      node.methods.asScala.iterator.map { method: MethodNode =>
        val signature = if (method.signature == null) method.desc else method.signature
        (signature, { () =>
          assertMethodRoundtrip(signature)
        })
      }.toList
    }
  }

  def checkMethodRoundtrip(signature: String): Unit = {
    test(signature) { assertMethodRoundtrip(signature) }
  }

  def checkMethodSignatureLibrary(coordinates: Coordinates): Unit = {
    checkMethodSignature(
      "method-" + coordinates.name,
      coordinates.classpath
    )
  }

  allLibraries.foreach(checkMethodSignatureLibrary)

//  checkMethodSignatureLibrary(scalameta)
//  checkMethodSignatureLibrary(akka)
  //  checkMethodSignatureLibrary(spark)
//  checkMethodSignatureLibrary(kafka)
}

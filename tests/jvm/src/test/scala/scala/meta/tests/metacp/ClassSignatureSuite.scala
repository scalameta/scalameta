package scala.meta.tests.metacp

import scala.meta.internal.metacp.JavaTypeSignature.ClassSignature
import scala.meta.internal.metacp.asm.ClassSignatureVisitor
import scala.tools.asm.tree.ClassNode

class ClassSignatureSuite extends BaseSignatureSuite[ClassSignature] {

  override def newVisitor() = new ClassSignatureVisitor()

  override def callback(node: ClassNode): List[(String, () => Unit)] =
    List(
      (node.signature, { () =>
        if (node.signature != null) {
          assertRoundtrip(node.signature)
        }
      })
    )

  allLibraries.foreach(checkSignatureLibrary)

}

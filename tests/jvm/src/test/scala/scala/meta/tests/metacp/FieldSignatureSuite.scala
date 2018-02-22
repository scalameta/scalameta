package scala.meta.tests.metacp

import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.FieldNode
import scala.collection.JavaConverters._
import scala.meta.internal.metacp.asm.FieldSignatureVisitor
import scala.meta.internal.metacp.asm.JavaTypeSignature.FieldSignature

class FieldSignatureSuite extends BaseSignatureSuite[FieldSignature] {
  override def newVisitor() = new FieldSignatureVisitor()
  override def callback(node: ClassNode): List[(String, () => Unit)] =
    node.fields.asScala.iterator.map { field: FieldNode =>
      val signature = if (field.signature == null) field.desc else field.signature
      (signature, { () =>
        assertRoundtrip(signature)
      })
    }.toList

  def checkField(signature: String): Unit =
    test(signature) { assertRoundtrip(signature) }

//  checkField("I")
  allLibraries.foreach(checkSignatureLibrary)
}

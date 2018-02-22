package scala.meta.tests.metacp

import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.MethodNode
import scala.collection.JavaConverters._
import scala.meta.internal.metacp.JavaTypeSignature.MethodSignature
import scala.meta.internal.metacp.asm.MethodSignatureVisitor
import scala.meta.internal.metacp.asm.TypedSignatureVisitor

class MethodSignatureSuite extends BaseSignatureSuite[MethodSignature] {
  override def newVisitor(): TypedSignatureVisitor[MethodSignature] =
    new MethodSignatureVisitor()

  override def callback(node: ClassNode): List[(String, () => Unit)] =
    node.methods.asScala.iterator.map { method: MethodNode =>
      val signature = if (method.signature == null) method.desc else method.signature
      (signature, { () =>
        assertRoundtrip(signature)
      })
    }.toList

  allLibraries.foreach(checkSignatureLibrary)

}

package scala.meta.tests.metacp

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.signature.SignatureVisitor
import scala.tools.asm.{Opcodes => o}

class SemanticdbSignatureVisitor extends SignatureVisitor(o.ASM5) {
  val descriptors = ListBuffer.empty[String]
  var seenParameter = false

  override def visitParameterType(): SignatureVisitor = {
    super.visitParameterType()
    this
  }

  override def visitClassType(classType: String): Unit = {
    if (!seenParameter) {
      pprint.log(classType)
      seenParameter = true
    }
  }

}

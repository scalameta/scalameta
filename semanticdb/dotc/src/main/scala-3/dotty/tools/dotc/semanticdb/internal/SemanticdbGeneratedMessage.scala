/* Adapted by scalameta from GeneratedMessageCompanion.scala of ScalaPB 0.11.20, under the
 * Apache License 2.0 in META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB
 * classes need. */
package dotty.tools.dotc.semanticdb.internal

trait SemanticdbGeneratedMessage extends Any with Product with Serializable {

  def serializedSize: Int

  def writeTo(output: SemanticdbOutputStream): Unit

  final def toByteArray: Array[Byte] = {
    val bytes = new Array[Byte](serializedSize)
    val output = SemanticdbOutputStream.newInstance(bytes)
    writeTo(output)
    output.checkNoSpaceLeft()
    bytes
  }

}

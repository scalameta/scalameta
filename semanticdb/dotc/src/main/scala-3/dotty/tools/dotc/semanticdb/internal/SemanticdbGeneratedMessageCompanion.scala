/* Adapted by scalameta from GeneratedMessageCompanion.scala of ScalaPB 0.11.20, under the
 * Apache License 2.0 in META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB
 * classes need. */
package dotty.tools.dotc.semanticdb.internal

trait SemanticdbGeneratedMessageCompanion[A <: SemanticdbGeneratedMessage] extends Serializable {

  type ValueType = A

  def parseFrom(input: SemanticdbInputStream): A

  def parseFrom(input: Array[Byte]): A = parseFrom(SemanticdbInputStream.newInstance(input))

  /** Returns `a` merged with the message read from `input`. */
  def merge(a: A, input: SemanticdbInputStream): A =
    parseFrom(a.toByteArray ++ parseFrom(input).toByteArray)

}

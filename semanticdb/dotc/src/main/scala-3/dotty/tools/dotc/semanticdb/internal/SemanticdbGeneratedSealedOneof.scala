/* Adapted by scalameta from GeneratedMessageCompanion.scala of ScalaPB 0.11.20, under the
 * Apache License 2.0 in META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB
 * classes need. */
package dotty.tools.dotc.semanticdb.internal

trait SemanticdbGeneratedSealedOneof extends Any with Product with Serializable {

  type MessageType <: SemanticdbGeneratedMessage

  def isEmpty: Boolean

  def isDefined: Boolean

  def asMessage: MessageType

}

/* Adapted by scalameta from GeneratedMessageCompanion.scala of ScalaPB 0.11.20, under the
 * Apache License 2.0 in META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB
 * classes need. */
package dotty.tools.dotc.semanticdb.internal

trait SemanticdbGeneratedEnum extends Any with Product with Serializable {

  type EnumType <: SemanticdbGeneratedEnum
  type RecognizedType <: EnumType

  def value: Int

  def index: Int

  def name: String

  override def toString: String = name

  def companion: SemanticdbGeneratedEnumCompanion[EnumType]

  def isUnrecognized: Boolean = false

  def asRecognized: Option[RecognizedType]

}

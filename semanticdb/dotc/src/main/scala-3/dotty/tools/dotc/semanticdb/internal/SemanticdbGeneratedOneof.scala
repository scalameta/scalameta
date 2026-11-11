/* Adapted by scalameta from GeneratedMessageCompanion.scala of ScalaPB 0.11.20, under the
 * Apache License 2.0 in META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB
 * classes need. */
package dotty.tools.dotc.semanticdb.internal

trait SemanticdbGeneratedOneof extends Any with Product with Serializable {

  type ValueType

  def number: Int

  def isDefined: Boolean

  def isEmpty: Boolean

  def value: ValueType

}

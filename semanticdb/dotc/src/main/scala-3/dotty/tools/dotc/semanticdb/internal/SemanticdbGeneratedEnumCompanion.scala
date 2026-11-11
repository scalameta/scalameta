/* Adapted by scalameta from GeneratedMessageCompanion.scala of ScalaPB 0.11.20, under the
 * Apache License 2.0 in META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB
 * classes need. */
package dotty.tools.dotc.semanticdb.internal

trait SemanticdbGeneratedEnumCompanion[A <: SemanticdbGeneratedEnum] extends Serializable {

  type ValueType = A

  def fromValue(value: Int): A

  def values: Seq[A]

}

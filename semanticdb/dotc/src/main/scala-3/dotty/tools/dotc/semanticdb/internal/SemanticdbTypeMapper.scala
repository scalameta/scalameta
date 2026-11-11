/* Adapted by scalameta from TypeMapper.scala of ScalaPB 0.11.20, under the Apache License 2.0 in
 * META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB classes need. */
package dotty.tools.dotc.semanticdb.internal

abstract class SemanticdbTypeMapper[BaseType, CustomType] {

  def toCustom(base: BaseType): CustomType

  def toBase(custom: CustomType): BaseType

}

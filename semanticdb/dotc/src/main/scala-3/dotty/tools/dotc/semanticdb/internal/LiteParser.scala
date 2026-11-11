/* Adapted by scalameta from LiteParser.scala of ScalaPB 0.11.20, under the Apache License 2.0 in
 * META-INF/LICENSE-ScalaPB.txt: changed to the API that the SemanticDB classes need. */
package dotty.tools.dotc.semanticdb.internal

object LiteParser {

  def readMessage[A <: SemanticdbGeneratedMessage](input: SemanticdbInputStream, message: A)(
      implicit cmp: SemanticdbGeneratedMessageCompanion[A],
  ): A = {
    val length = input.readRawVarint32()
    val oldLimit = input.pushLimit(length)
    val result = cmp.merge(message, input)
    input.checkLastTagWas(0)
    input.popLimit(oldLimit)
    result
  }

  def readMessage[A <: SemanticdbGeneratedMessage](
      input: SemanticdbInputStream,
  )(implicit cmp: SemanticdbGeneratedMessageCompanion[A]): A = {
    val length = input.readRawVarint32()
    val oldLimit = input.pushLimit(length)
    val result = cmp.parseFrom(input)
    input.checkLastTagWas(0)
    input.popLimit(oldLimit)
    result
  }

}

package scala.meta.internal

import scala.meta.inputs._
import scala.language.implicitConversions

package object inputs {
  type InternalInput = star.meta.internal.inputs.InternalInput
  implicit def XtensionPositionFormatMessage(pos: Position) = new star.meta.internal.inputs.XtensionPositionFormatMessage(pos)
  implicit def XtensionInputSyntaxStructure(input: Input) = new star.meta.internal.inputs.XtensionInputSyntaxStructure(input)
  implicit def XtensionPositionSyntaxStructure(pos: Position) = new star.meta.internal.inputs.XtensionPositionSyntaxStructure(pos)
}
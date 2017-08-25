package scala.meta.internal

import scala.meta.inputs._
import scala.language.implicitConversions

package object inputs {
  type InternalInput = org.langmeta.internal.inputs.InternalInput
  implicit def XtensionPositionFormatMessage(pos: Position) =
    new org.langmeta.internal.inputs.XtensionPositionFormatMessage(pos)
  implicit def XtensionInputSyntaxStructure(input: Input) =
    new org.langmeta.internal.inputs.XtensionInputSyntaxStructure(input)
  implicit def XtensionPositionSyntaxStructure(pos: Position) =
    new org.langmeta.internal.inputs.XtensionPositionSyntaxStructure(pos)
}

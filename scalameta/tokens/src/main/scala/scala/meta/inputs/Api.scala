package scala.meta
package inputs

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Input = scala.meta.inputs.Input
  val Input = scala.meta.inputs.Input

  type Content = scala.meta.inputs.Content
  val Content = scala.meta.inputs.Content

  type Position = scala.meta.inputs.Position
  val Position = scala.meta.inputs.Position

  type Point = scala.meta.inputs.Point
  val Point = scala.meta.inputs.Point
}

package scala.meta
package inputs

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Input = scala.meta.inputs.Input
  lazy val Input = scala.meta.inputs.Input

  type Position = scala.meta.inputs.Position
  lazy val Position = scala.meta.inputs.Position

  type Point = scala.meta.inputs.Point
  lazy val Point = scala.meta.inputs.Point
}

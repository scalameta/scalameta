package scala.meta.inputs

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Input = scala.meta.inputs.Input
  object Input {
    val None = scala.meta.inputs.Input.None

    type String = scala.meta.inputs.Input.String
    val String = scala.meta.inputs.Input.String

    type Stream = scala.meta.inputs.Input.Stream
    val Stream = scala.meta.inputs.Input.Stream

    type File = scala.meta.inputs.Input.File
    val File = scala.meta.inputs.Input.File

    type VirtualFile = scala.meta.inputs.Input.VirtualFile
    val VirtualFile = scala.meta.inputs.Input.VirtualFile

    type Slice = scala.meta.inputs.Input.Slice
    val Slice = scala.meta.inputs.Input.Slice
  }

  type Position = scala.meta.inputs.Position
  object Position {
    val None = scala.meta.inputs.Position.None

    type Range = scala.meta.inputs.Position.Range
    val Range = scala.meta.inputs.Position.Range
  }
}

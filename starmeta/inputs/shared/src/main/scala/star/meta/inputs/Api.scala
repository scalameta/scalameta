package star.meta
package inputs

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Input = star.meta.inputs.Input
  object Input {
    val None = star.meta.inputs.Input.None

    type String = star.meta.inputs.Input.String
    val String = star.meta.inputs.Input.String

    type Stream = star.meta.inputs.Input.Stream
    val Stream = star.meta.inputs.Input.Stream

    type File = star.meta.inputs.Input.File
    val File = star.meta.inputs.Input.File

    type VirtualFile = star.meta.inputs.Input.VirtualFile
    val VirtualFile = star.meta.inputs.Input.VirtualFile

    type Sugar = star.meta.inputs.Input.Sugar
    val Sugar = star.meta.inputs.Input.Sugar

    type Slice = star.meta.inputs.Input.Slice
    val Slice = star.meta.inputs.Input.Slice
  }

  type Position = star.meta.inputs.Position
  object Position {
    val None = star.meta.inputs.Position.None

    type Range = star.meta.inputs.Position.Range
    val Range = star.meta.inputs.Position.Range
  }
}

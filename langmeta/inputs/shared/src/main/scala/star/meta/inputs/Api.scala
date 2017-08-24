package lang.meta
package inputs

private[meta] trait Api {
}

private[meta] trait Aliases {
  type Input = lang.meta.inputs.Input
  object Input {
    val None = lang.meta.inputs.Input.None

    type String = lang.meta.inputs.Input.String
    val String = lang.meta.inputs.Input.String

    type Stream = lang.meta.inputs.Input.Stream
    val Stream = lang.meta.inputs.Input.Stream

    type File = lang.meta.inputs.Input.File
    val File = lang.meta.inputs.Input.File

    type VirtualFile = lang.meta.inputs.Input.VirtualFile
    val VirtualFile = lang.meta.inputs.Input.VirtualFile

    type Sugar = lang.meta.inputs.Input.Sugar
    val Sugar = lang.meta.inputs.Input.Sugar

    type Denotation = lang.meta.inputs.Input.Denotation
    val Denotation = lang.meta.inputs.Input.Denotation

    type Slice = lang.meta.inputs.Input.Slice
    val Slice = lang.meta.inputs.Input.Slice
  }

  type Position = lang.meta.inputs.Position
  object Position {
    val None = lang.meta.inputs.Position.None

    type Range = lang.meta.inputs.Position.Range
    val Range = lang.meta.inputs.Position.Range
  }
}

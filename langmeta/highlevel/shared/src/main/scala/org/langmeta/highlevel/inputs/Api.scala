package org.langmeta.highlevel.inputs

private[langmeta] trait Api {
}

private[langmeta] trait Aliases {
  type Input = org.langmeta.highlevel.inputs.Input
  object Input {
    val None = org.langmeta.highlevel.inputs.Input.None

    type String = org.langmeta.highlevel.inputs.Input.String
    val String = org.langmeta.highlevel.inputs.Input.String

    type Stream = org.langmeta.highlevel.inputs.Input.Stream
    val Stream = org.langmeta.highlevel.inputs.Input.Stream

    type File = org.langmeta.highlevel.inputs.Input.File
    val File = org.langmeta.highlevel.inputs.Input.File

    type VirtualFile = org.langmeta.highlevel.inputs.Input.VirtualFile
    val VirtualFile = org.langmeta.highlevel.inputs.Input.VirtualFile

    type Synthetic = org.langmeta.highlevel.inputs.Input.Synthetic
    val Synthetic = org.langmeta.highlevel.inputs.Input.Synthetic

    type Denotation = org.langmeta.highlevel.inputs.Input.Denotation
    val Denotation = org.langmeta.highlevel.inputs.Input.Denotation

    type Slice = org.langmeta.highlevel.inputs.Input.Slice
    val Slice = org.langmeta.highlevel.inputs.Input.Slice
  }

  type Position = org.langmeta.highlevel.inputs.Position
  object Position {
    val None = org.langmeta.highlevel.inputs.Position.None

    type Range = org.langmeta.highlevel.inputs.Position.Range
    val Range = org.langmeta.highlevel.inputs.Position.Range
  }
}

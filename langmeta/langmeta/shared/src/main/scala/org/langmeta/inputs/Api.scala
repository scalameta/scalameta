package org.langmeta
package inputs

private[langmeta] trait Api {
}

private[langmeta] trait Aliases {
  type Input = org.langmeta.inputs.Input
  object Input {
    val None = org.langmeta.inputs.Input.None

    type String = org.langmeta.inputs.Input.String
    val String = org.langmeta.inputs.Input.String

    type Stream = org.langmeta.inputs.Input.Stream
    val Stream = org.langmeta.inputs.Input.Stream

    type File = org.langmeta.inputs.Input.File
    val File = org.langmeta.inputs.Input.File

    type VirtualFile = org.langmeta.inputs.Input.VirtualFile
    val VirtualFile = org.langmeta.inputs.Input.VirtualFile

    type Slice = org.langmeta.inputs.Input.Slice
    val Slice = org.langmeta.inputs.Input.Slice
  }

  type Position = org.langmeta.inputs.Position
  object Position {
    val None = org.langmeta.inputs.Position.None

    type Range = org.langmeta.inputs.Position.Range
    val Range = org.langmeta.inputs.Position.Range
  }
}

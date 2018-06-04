package scala.meta.internal.scalacp

sealed trait LinkMode
case object SymlinkChildren extends LinkMode
case object HardlinkChildren extends LinkMode

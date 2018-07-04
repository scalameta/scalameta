package scala.meta.internal.scalacp

sealed trait SymbolLookup
final case object PackageLookup extends SymbolLookup
final case object JavaLookup extends SymbolLookup
final case object ScalaLookup extends SymbolLookup
final case object MissingLookup extends SymbolLookup

package scala.meta.internal.scalacp

sealed trait SymbolLookup
case object PackageLookup extends SymbolLookup
case object JavaLookup extends SymbolLookup
case object ScalaLookup extends SymbolLookup
case object MissingLookup extends SymbolLookup

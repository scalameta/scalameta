package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}

trait SymbolOpsCompat {
  self: SemanticdbOps =>
  implicit class XtensionGSymbolCompat(sym: g.Symbol) {
    private lazy val implicitPrimitiveConversionNames = Set(
      g.termNames.toByte,
      g.termNames.toShort,
      g.termNames.toChar,
      g.termNames.toInt,
      g.termNames.toLong,
      g.termNames.toFloat,
      g.termNames.toDouble
    )

    // Returns true if the `this.sym` resolves to a primitive conversion method like toInt/toLong
    // and the provided mtree name does not match the name of that symbol. This implementation
    // has a minor bug where it doesn't detect implicit conversions when mtree.value has the name
    // of a conversion method like toInt/toLong, but it's a corner case that can be fixed separately
    // if it's a big problem.
    def isImplicitPrimitiveConversion(mtree: m.Name): Boolean = sym.name.startsWith("to") &&
      g.definitions.ScalaValueClassesSet.contains(sym.owner) &&
      implicitPrimitiveConversionNames.contains(sym.name.toTermName) &&
      sym.name.toString() != mtree.value
  }
}

package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}

trait SymbolOpsCompat {
  self: SemanticdbOps =>
  implicit class XtensionGSymbolCompat(sym: g.Symbol) {
    lazy val ScalaValueClasses: List[g.Symbol] = List(
      g.definitions.UnitClass,
      g.definitions.BooleanClass,
      g.definitions.ByteClass,
      g.definitions.ShortClass,
      g.definitions.CharClass,
      g.definitions.IntClass,
      g.definitions.LongClass,
      g.definitions.FloatClass,
      g.definitions.DoubleClass
    )
    lazy val ScalaValueClassesSet = ScalaValueClasses.toSet
    private lazy val implicitPrimitiveConversionNames = Set(
      g.termNames.toByte,
      g.termNames.toShort,
      g.termNames.toChar,
      g.termNames.toInt,
      g.termNames.toLong,
      g.termNames.toFloat,
      g.termNames.toDouble
    )
    // See comment in scala-2.13/.../SymbolOpsCompat.scala
    def isImplicitPrimitiveConversion(mtree: m.Name): Boolean = sym.name.startsWith("to") &&
      ScalaValueClassesSet.contains(sym.owner) &&
      implicitPrimitiveConversionNames.contains(sym.name.toTermName) &&
      sym.name.toString() != mtree.value
  }
}

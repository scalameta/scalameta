package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.{AnnotatedType, ClassSignature, ExistentialType,
  IntersectionType, StructuralType, SymbolInformation, Type, TypeRef, TypeSignature, UniversalType,
  WithType}

import scala.collection.mutable

trait SymbolTable {

  /** Returns the SymbolInformation for the given symbol, or None if the symbol is missing. */
  def info(symbol: String): Option[SymbolInformation]

  /**
   * Tests whether `child` is an erased subtype of `parent`.
   *
   * "Erased" means only symbols are compared and type arguments are ignored — the semantics of
   * `java.lang.Class.isAssignableFrom`, which is what the motivating Scalafix rules need (see
   * scalameta/scalameta#1213). Unlike that JVM-reflection approach it reads the compiler-emitted
   * SemanticDB rather than loading the analyzed program's classes, so it also covers targets whose
   * classes are not JVM-loadable (e.g. Scala.js/Native projects); the query itself runs on the JVM.
   *
   * It is reflexive for resolvable symbols and walks `ClassSignature.parents` transitively,
   * following type aliases through `TypeSignature.upperBound` (e.g. the scala-library parent
   * `scala/package.RuntimeException#` resolves to `java/lang/RuntimeException#`) and unwrapping
   * structural/annotated/existential/universal type wrappers. Symbols not resolvable via [[info]]
   * (including a missing `child`, even when `child == parent`) yield `false` rather than throwing,
   * and the traversal is cycle-safe.
   */
  def isSubtypeOf(child: String, parent: String): Boolean = {
    val seen = mutable.Set.empty[String]
    def loop(sym: String): Boolean = seen.add(sym) && info(sym)
      .exists(i => sym == parent || parentSymbols(i).exists(loop))
    loop(child)
  }

  private def parentSymbols(sinfo: SymbolInformation): List[String] = sinfo.signature match {
    case cs: ClassSignature => cs.parents.iterator.flatMap(typeSymbols).toList
    case ts: TypeSignature => typeSymbols(ts.upperBound) // follow type-alias upper bound
    case _ => Nil
  }

  private def typeSymbols(tpe: Type): List[String] = tpe match {
    case TypeRef(_, symbol, _) => symbol :: Nil
    case WithType(types) => types.iterator.flatMap(typeSymbols).toList
    case IntersectionType(types) => types.iterator.flatMap(typeSymbols).toList
    // unwrap type wrappers to the underlying type(s); e.g. a refined parent is encoded as
    // StructuralType(WithType(...)) (see scalacp/TypeOps.scala).
    case StructuralType(t, _) => typeSymbols(t)
    case AnnotatedType(_, t) => typeSymbols(t)
    case ExistentialType(t, _) => typeSymbols(t)
    case UniversalType(_, t) => typeSymbols(t)
    case _ => Nil
  }

}

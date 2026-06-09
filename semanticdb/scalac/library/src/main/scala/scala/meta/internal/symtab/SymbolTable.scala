package scala.meta.internal.symtab

import scala.meta.internal.semanticdb._

import scala.annotation.tailrec
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
   * For example, a rule banning public APIs that expose `Throwable` subtypes
   * (scalacenter/scalafix#531) checks the symbol from a method's return-type `TypeRef` with
   * `symtab.isSubtypeOf(returnTypeSymbol, "java/lang/Throwable#")`.
   *
   * It is reflexive (by symbol equality, so even for symbols not resolvable via [[info]]) and walks
   * `ClassSignature.parents` transitively, following type aliases through
   * `TypeSignature.upperBound` (e.g. the scala-library parent `scala/package.RuntimeException#`
   * resolves to `java/lang/RuntimeException#`) and unwrapping
   * structural/annotated/existential/universal type wrappers. A `parent` merely named in the
   * ancestor chain matches even when its own `SymbolInformation` is missing, but an unresolvable
   * intermediate link stops the walk there, yielding `false` rather than throwing; the traversal is
   * cycle-safe.
   */
  def isSubtypeOf(child: String, parent: String): Boolean = {
    val seen = mutable.Set.empty[String]
    def loop(sym: String): Boolean = sym == parent || seen.add(sym) && info(sym)
      .exists(parentSymbols(_).exists(loop))
    loop(child)
  }

  private def parentSymbols(sinfo: SymbolInformation): Iterator[String] = sinfo.signature match {
    case cs: ClassSignature => typeSymbolsFlatten(cs.parents)
    case ts: TypeSignature => typeSymbols(ts.upperBound) // follow type-alias upper bound
    case _ => Iterator.empty
  }

  @tailrec
  private def typeSymbols(tpe: Type): Iterator[String] = tpe match {
    case TypeRef(_, symbol, _) => Iterator(symbol)
    case WithType(types) => typeSymbolsFlatten(types)
    case IntersectionType(types) => typeSymbolsFlatten(types)
    // unwrap type wrappers to the underlying type(s); e.g. a refined parent is encoded as
    // StructuralType(WithType(...)) (see scalacp/TypeOps.scala).
    case StructuralType(t, _) => typeSymbols(t)
    case AnnotatedType(_, t) => typeSymbols(t)
    case ExistentialType(t, _) => typeSymbols(t)
    case UniversalType(_, t) => typeSymbols(t)
    case _ => Iterator.empty
  }

  private def typeSymbolsFlatten(types: Iterable[Type]): Iterator[String] = types.iterator
    .flatMap(typeSymbols)

}

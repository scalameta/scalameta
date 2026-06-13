package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.Scala.{Descriptor => d, _}
import scala.meta.internal.semanticdb._

import scala.annotation.tailrec
import scala.collection.mutable

trait SymbolTable {

  /** Returns the SymbolInformation for the given symbol, or None if the symbol is missing. */
  def info(symbol: String): Option[SymbolInformation]

  /**
   * Returns the overload alternatives of `symbol` declared in its owner, or `None` when the
   * overload set cannot be determined from this symbol table.
   *
   * At a resolved call site scalac picks a single overload, so the occurrence points to one
   * disambiguated symbol (e.g. `A.foo(+1).`); this recovers the sibling candidates declared
   * alongside it (`A.foo().`, `A.foo(+1).`). See
   * https://github.com/scalameta/scalameta/issues/1298.
   *
   * `None` ("could not inspect") is deliberately kept distinct from `Some(List(symbol))`
   * ("inspected: no other overload"), so a caller never mistakes a missing entry for a
   * non-overloaded method. The result is only ever a `Some` when the set was actually read off the
   * owner's declarations (or off an unresolved multi-symbol); it is never fabricated from the
   * input:
   *   - `Some(set)` — the same-owner candidates (which include `symbol`) for a global method whose
   *     owner is in this table and declares it; `Some(symbol.asMulti)` for an unresolved
   *     multi-symbol.
   *   - `None` — the empty symbol; a local or non-method symbol (its overloads are not inspectable
   *     from a symbol alone); or a method whose declaring owner is unavailable here or does not
   *     declare it.
   *
   * Scope: only overloads declared in the *same owner* are returned. Overloads inherited from, or
   * split across, supertypes are intentionally excluded — a classpath symbol table cannot resolve
   * them safely: `overriddenSymbols` is `Nil` outside the compiler plugin, so override and overload
   * are indistinguishable when walking parents, and the full receiver-type candidate set further
   * needs the call site's static receiver type, which a symbol alone does not carry.
   */
  def alternatives(symbol: String): Option[List[String]] =
    if (symbol.isNone) None
    else if (symbol.isMulti) Some(symbol.asMulti)
    else symbol.desc match {
      case d.Method(name, _) => info(symbol.owner).map(_.signature).flatMap {
          case c: ClassSignature =>
            val sameName = c.declarations.symbols.filter(_.desc match {
              case d.Method(v, _) => v == name
              case _ => false
            })
            if (sameName.contains(symbol)) Some(sameName) else None
          case _ => None
        }
      case _ => None
    }

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

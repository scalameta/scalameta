package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.Scala.{Descriptor => d, _}
import scala.meta.internal.semanticdb._

import scala.annotation.tailrec
import scala.collection.mutable

trait SymbolTable {

  /** Returns the SymbolInformation for the given symbol, or None if the symbol is missing. */
  def info(symbol: String): Option[SymbolInformation]

  /**
   * Returns all same-owner overloads of `symbol` (including `symbol` itself) in declaration order
   * (the disambiguator sequence: the base overload, then `+1`, `+2`, ...), or `Nil` when the
   * overload set cannot be determined from this symbol table.
   *
   * At a resolved call site scalac picks a single overload, so the occurrence points to one
   * disambiguated symbol (e.g. `A.foo(+1).`); this recovers the full group declared alongside it
   * (`A.foo().`, `A.foo(+1).`). See https://github.com/scalameta/scalameta/issues/1298.
   *
   * A determined result always contains at least `symbol`, so `Nil` unambiguously means "could not
   * inspect" — never "no overloads". `Nil` covers the empty symbol, a local or non-method symbol,
   * and a method whose declaring owner is unavailable here or does not declare it. For an
   * unresolved multi-symbol, the members are returned.
   *
   * Scope: only overloads declared in the *same owner* are returned. Overloads inherited from, or
   * split across, supertypes are intentionally excluded — a classpath symbol table cannot resolve
   * them safely: `overriddenSymbols` is `Nil` outside the compiler plugin, so override and overload
   * are indistinguishable when walking parents, and the full receiver-type candidate set further
   * needs the call site's static receiver type, which a symbol alone does not carry.
   */
  def overloads(symbol: String): List[String] =
    if (symbol.isMulti) symbol.asMulti
    else if (symbol.isGlobal) {
      val (desc, owner) = DescriptorParser(symbol)
      desc match {
        case d.Method(name, _) => info(owner).map(_.signature) match {
            case Some(c: ClassSignature) =>
              // keep only members that really are same-named methods of this owner, so a symbol
              // that merely parses like one (but isn't declared here) isn't fabricated into the set
              val sameName = c.declarations.symbols.filter(_.desc match {
                case d.Method(`name`, _) => true
                case _ => false
              })
              if (sameName.contains(symbol)) sameName else Nil
            case _ => Nil
          }
        case _ => Nil
      }
    } else Nil

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

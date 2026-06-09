package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.Scala.{Descriptor => d, _}
import scala.meta.internal.semanticdb.{ClassSignature, SymbolInformation, XtensionSemanticdbScopeOpt}

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

}

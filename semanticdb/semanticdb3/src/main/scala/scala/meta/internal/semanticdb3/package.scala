package scala.meta.internal

package object semanticdb3 {

  implicit class XtensionSemanticdb3SymbolInformation(info: SymbolInformation) {
    def has(
        prop: SymbolInformation.Property,
        prop2: SymbolInformation.Property,
        props: SymbolInformation.Property*): Boolean =
      has(prop) && has(prop2) && props.forall(has)
    def has(prop: SymbolInformation.Property): Boolean =
      (info.properties & prop.value) != 0
  }

  implicit class XtensionSemanticdb3Scope(scope: Scope) {
    def symbols: List[String] = {
      if (scope.symlinks.nonEmpty) scope.symlinks.toList
      else scope.hardlinks.map(_.symbol).toList
    }
    def infos: List[SymbolInformation] = {
      if (scope.symlinks.nonEmpty) {
        scope.symlinks.map(symbol => SymbolInformation(symbol = symbol)).toList
      } else {
        scope.hardlinks.toList
      }
    }
  }

  implicit class XtensionSemanticdb3ScopeOpt(scopeOpt: Option[Scope]) {
    def symbols: List[String] = scopeOpt.map(_.symbols).getOrElse(Nil)
    def infos: List[SymbolInformation] = scopeOpt.map(_.infos).getOrElse(Nil)
  }

  implicit class XtensionSemanticdb3Scopes(scopes: Seq[Scope]) {
    def symbols: List[List[String]] = scopes.map(_.symbols).toList
    def infos: List[List[SymbolInformation]] = scopes.map(_.infos).toList
  }
}

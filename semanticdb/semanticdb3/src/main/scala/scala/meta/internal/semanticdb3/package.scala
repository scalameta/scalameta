package scala.meta.internal

package object semanticdb3 {

  implicit class XtensionSymbolInformationProperty(info: SymbolInformation) {
    def has(
        prop: SymbolInformation.Property,
        prop2: SymbolInformation.Property,
        props: SymbolInformation.Property*): Boolean =
      has(prop) && has(prop2) && props.forall(has)
    def has(prop: SymbolInformation.Property): Boolean =
      (info.properties & prop.value) != 0
  }

}

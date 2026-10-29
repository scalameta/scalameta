package scala.meta.tests.metacp

import scala.meta.internal.semanticdb.Locator
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath

import scala.collection.mutable

object MetacpOps {

  /** Returns all symbols that have an associated SymbolInformation in the classpath */
  def collectAllGlobalSymbols(classpath: Classpath): collection.Set[String] = {
    val allSymbols = mutable.Set.empty[String]
    Locator(classpath.entries.map(_.toNIO))((_, docs) =>
      for {
        doc <- docs().documents
        sym <- doc.symbols
      } allSymbols += sym.symbol,
    )
    allSymbols
  }

  /** Returns all symbols that are referenced from the signature of info */
  def collectAllSymbolReferences(info: s.SymbolInformation): List[String] = {
    val references = List.newBuilder[String]
    val isHardlink = mutable.Set.empty[String]
    def visitSymbol(symbol: String): Unit = if (!isHardlink(symbol)) references += symbol
    def visitScope(scope: Option[s.Scope])(thunk: => Unit): Unit = {
      var toEnter: Seq[String] = Nil
      scope.foreach { s =>
        toEnter = s.hardlinks.map(_.symbol)
        isHardlink ++= toEnter
        s.symlinks.foreach(visitSymbol)
      }
      thunk
      isHardlink --= toEnter
    }
    def visitType(tpe: s.Type): Unit = tpe match {
      case x: s.TypeRef =>
        visitType(x.prefix)
        visitSymbol(x.symbol)
        x.typeArguments.foreach(visitType)
      case x: s.SingleType =>
        visitType(x.prefix)
        visitSymbol(x.symbol)
      case x: s.ThisType => visitSymbol(x.symbol)
      case x: s.SuperType =>
        visitType(x.prefix)
        visitSymbol(x.symbol)
      case _: s.ConstantType => ()
      case x: s.IntersectionType => x.types.foreach(visitType)
      case x: s.UnionType => x.types.foreach(visitType)
      case x: s.WithType => x.types.foreach(visitType)
      case x: s.StructuralType => visitScope(x.declarations)(visitType(x.tpe))
      case x: s.AnnotatedType =>
        x.annotations.foreach(annot => visitType(annot.tpe))
        visitType(x.tpe)
      case x: s.ExistentialType => visitScope(x.declarations)(visitType(x.tpe))
      case x: s.UniversalType => visitScope(x.typeParameters)(visitType(x.tpe))
      case x: s.ByNameType => visitType(x.tpe)
      case x: s.RepeatedType => visitType(x.tpe)
      case x: s.MatchType =>
        visitType(x.scrutinee)
        x.cases.foreach { kase =>
          visitType(kase.key)
          visitType(kase.body)
        }
      case x: s.LambdaType => visitScope(x.parameters)(visitType(x.returnType))
      case s.NoType =>
    }
    def visitSignature(signature: s.Signature): Unit = signature match {
      case x: s.ClassSignature => visitScope(x.typeParameters)(visitScope(x.declarations) {
          x.parents.foreach(visitType)
          visitType(x.self)
        })
      case sig: s.MethodSignature =>
        sig.parameterLists.foreach(s => visitScope(Some(s))(()))
        visitScope(sig.typeParameters)(visitType(sig.returnType))
      case x: s.TypeSignature => visitScope(x.typeParameters) {
          visitType(x.lowerBound)
          visitType(x.upperBound)
        }
      case x: s.ValueSignature => visitType(x.tpe)
      case s.NoSignature => ()
    }
    visitSignature(info.signature)
    references.result()
  }

  /**
   * Returns all symbols are referenced in the classpath but have no associated SymbolInformation
   */
  def collectReferencedToUndefinedSymbols(classpath: Classpath): Iterable[String] = {
    val isPersistedGlobalSymbol = collectAllGlobalSymbols(classpath)
    val errors = mutable.Set.empty[String]
    Locator(classpath.entries.map(_.toNIO))((path, docs) =>
      for {
        doc <- docs().documents
        sym <- doc.symbols
      } {
        val references = collectAllSymbolReferences(sym)
        errors ++= references.filterNot(isPersistedGlobalSymbol)
      },
    )
    errors
  }

}

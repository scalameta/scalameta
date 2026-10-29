package scala.meta.tests.metacp

import scala.meta.internal.semanticdb.Locator
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath

import java.nio.file.Path

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

  /** Calls `f` with each symbol that the signature of `info` refers to */
  def collectAllSymbolReferences(info: s.SymbolInformation)(f: String => Unit): Unit = {
    val isHardlink = mutable.Set.empty[String]
    def visitSymbol(symbol: String): Unit = if (!isHardlink(symbol)) f(symbol)
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
      case x: s.AppliedType =>
        visitType(x.tpe)
        x.typeArguments.foreach(visitType)
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
  }

  /**
   * Returns, for each SemanticDB file that has some, the symbols it refers to without a
   * SymbolInformation in the classpath, among those that pass `f`
   */
  def collectReferencedToUndefinedSymbols(
      classpath: Classpath,
  )(f: String => Boolean): Seq[(Path, Iterable[String])] = {
    val isPersistedGlobalSymbol = collectAllGlobalSymbols(classpath)
    val errors = Seq.newBuilder[(Path, Iterable[String])]
    Locator(classpath.entries.map(_.toNIO)) { (path, docs) =>
      val pathErrors = mutable.Set.empty[String]
      docs().documents.foreach(doc =>
        doc.symbols.foreach(sym =>
          collectAllSymbolReferences(sym)(x =>
            if (!isPersistedGlobalSymbol(x) && f(x)) pathErrors += x,
          ),
        ),
      )
      if (pathErrors.nonEmpty) errors += path -> pathErrors
    }
    errors.result()
  }

}

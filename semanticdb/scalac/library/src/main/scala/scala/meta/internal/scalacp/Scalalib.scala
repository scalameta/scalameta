package scala.meta.internal.scalacp

import scala.meta.internal.classpath.Classfile
import scala.meta.internal.metacp._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d, DisplayNames => dn, Names => n}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k, Property => p}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.{semanticdb => s}

import scala.collection.mutable
import scala.reflect.NameTransformer

object Scalalib {

  lazy val synthetics: List[ClassfileInfos] = List(
    Scalalib.anyClass,
    Scalalib.anyValClass,
    Scalalib.anyRefClass,
    Scalalib.nothingClass,
    Scalalib.nullClass,
    Scalalib.singletonTrait,
  )
  // scalafmt: { maxColumn = 150 }
  private def anyClass: ClassfileInfos = {
    implicit val symbols = List.newBuilder[s.SymbolInformation]
    builtinMethod("Any", List(p.ABSTRACT), "equals", Nil, List("that" -> anySymbol), booleanSymbol)
    builtinMethod("Any", List(p.FINAL), "==", Nil, List("that" -> anySymbol), booleanSymbol)
    builtinMethod("Any", List(p.FINAL), "!=", Nil, List("that" -> anySymbol), booleanSymbol)
    builtinMethod("Any", List(p.ABSTRACT), "hashCode", Nil, Nil, intSymbol)
    builtinMethod("Any", List(p.FINAL), "##", Nil, Nil, intSymbol)
    builtinMethod("Any", List(p.ABSTRACT), "toString", Nil, Nil, stringSymbol)
    // NOTE: Return type of getClass can't be expressed in the SemanticDB type system.
    // The method is special-cased in both the Java and Scala compilers, so we'll slack a little bit too for the time being.
    builtinMethod("Any", List(p.FINAL), "getClass", Nil, Nil, "java/lang/Class#")
    builtinMethod("Any", List(p.FINAL), "isInstanceOf", List("A"), Nil, booleanSymbol)
    builtinMethod("Any", List(p.FINAL), "asInstanceOf", List("A"), Nil, "scala/Any#asInstanceOf().[A]")
    builtin(k.CLASS, List(p.ABSTRACT), "Any", Nil, symbols.result())
  }

  private def anyValClass: ClassfileInfos = builtin(k.CLASS, List(p.ABSTRACT), "AnyVal", List(anySymbol), Nil)

  private def anyRefClass: ClassfileInfos = {
    // FIXME: https://github.com/scalameta/scalameta/issues/1564
    val synchronizedT = "scala/AnyRef#synchronized().[T]"
    implicit val symbols = List.newBuilder[s.SymbolInformation]
    builtinMethod("AnyRef", List(p.FINAL), "eq", Nil, List("that" -> anyRefSymbol), booleanSymbol)
    builtinMethod("AnyRef", List(p.FINAL), "ne", Nil, List("that" -> anyRefSymbol), booleanSymbol)
    builtinMethod("AnyRef", List(p.FINAL), "synchronized", List("T"), List("body" -> synchronizedT), synchronizedT)
    builtin(k.CLASS, Nil, "AnyRef", List(anySymbol), symbols.result())
  }
  // scalafmt: {}

  private def nothingClass: ClassfileInfos =
    builtin(k.CLASS, List(p.ABSTRACT, p.FINAL), "Nothing", List(anySymbol), Nil)

  private def nullClass: ClassfileInfos =
    builtin(k.CLASS, List(p.ABSTRACT, p.FINAL), "Null", List(anyRefSymbol), Nil)

  private def singletonTrait: ClassfileInfos =
    builtin(k.TRAIT, Nil, "Singleton", List(anySymbol), Nil)

  private[internal] val anySymbol = "scala/Any#"
  private[internal] val anyRefSymbol = "scala/AnyRef#"
  private[internal] val booleanSymbol = "scala/Boolean#"
  private[internal] val intSymbol = "scala/Int#"
  private[internal] val stringSymbol = "java/lang/String#"

  private def scalaPackage: String = Symbols.Global(Symbols.RootPackage, d.Package("scala"))

  private def builtin(
      kind: s.SymbolInformation.Kind,
      props: List[s.SymbolInformation.Property],
      className: String,
      bases: List[String],
      symbols: List[s.SymbolInformation],
  ): ClassfileInfos = {
    val parents = bases.map(base => s.TypeRef(s.NoType, base, Nil))
    val symbol = Symbols.Global(scalaPackage, d.Type(className))
    val ctorSig = s.MethodSignature(Some(s.Scope(Nil)), List(s.Scope(Nil)), s.NoType)
    val ctor = s.SymbolInformation(
      symbol = Symbols.Global(symbol, d.Method(n.Constructor.value, "()")),
      language = l.SCALA,
      kind = k.CONSTRUCTOR,
      properties = p.PRIMARY.value,
      displayName = dn.Constructor,
      signature = ctorSig,
      access = s.PublicAccess(),
      annotations = Nil,
      overriddenSymbols = Nil,
    )
    val builtinSig = {
      val tparams = Some(s.Scope(Nil))
      val decls = symbols.filter(_.kind.isMethod)
      val declarations = if (kind.isClass) ctor +: decls else decls
      s.ClassSignature(tparams, parents, s.NoType, Some(s.Scope(declarations.map(_.symbol))))
    }
    val builtin = s.SymbolInformation(
      symbol = symbol,
      language = l.SCALA,
      kind = kind,
      properties = props.foldLeft(0)((acc, prop) => acc | prop.value),
      displayName = className,
      signature = builtinSig,
      access = s.PublicAccess(),
      annotations = Nil,
      overriddenSymbols = Nil,
    )
    val infos = builtin :: (if (kind.isClass) ctor :: symbols else symbols)
    val relativeUri = "scala/" + NameTransformer.encode(className) + Classfile.Suffix
    val syntheticClassfile = ClassfileInfos(relativeUri, s.Language.SCALA, infos)
    syntheticClassfile
  }

  private def builtinMethod(
      className: String,
      props: List[s.SymbolInformation.Property],
      methodName: String,
      tparamDsls: List[String],
      paramDsls: List[(String, String)],
      retTpeSymbol: String,
  )(implicit builder: mutable.Builder[s.SymbolInformation, _]): Unit = {
    val classSymbol = Symbols.Global(scalaPackage, d.Type(className))
    val methodSymbol = Symbols.Global(classSymbol, d.Method(methodName, "()"))
    val tparamSymbols = List.newBuilder[String]
    tparamDsls.foreach { tparamName =>
      val tparamSymbol = Symbols.Global(methodSymbol, d.TypeParameter(tparamName))
      tparamSymbols += tparamSymbol
      val tparamSig = s.TypeSignature()
      builder += s.SymbolInformation(
        symbol = tparamSymbol,
        language = l.SCALA,
        kind = k.TYPE_PARAMETER,
        displayName = tparamName,
        signature = tparamSig,
        access = s.NoAccess,
        annotations = Nil,
        overriddenSymbols = Nil,
      )
    }
    val paramSymbols = List.newBuilder[String]
    paramDsls.foreach { case (paramName, paramTpeSymbol) =>
      val paramSymbol = Symbols.Global(methodSymbol, d.Parameter(paramName))
      paramSymbols += paramSymbol
      val paramSig = s.ValueSignature(s.TypeRef(s.NoType, paramTpeSymbol, Nil))
      builder += s.SymbolInformation(
        symbol = paramSymbol,
        language = l.SCALA,
        kind = k.PARAMETER,
        displayName = paramName,
        signature = paramSig,
        annotations = Nil,
        overriddenSymbols = Nil,
      )
    }
    val methodSig = {
      val tps = Some(s.Scope(tparamSymbols.result()))
      val returnType = s.TypeRef(s.NoType, retTpeSymbol, Nil)
      s.MethodSignature(tps, List(s.Scope(paramSymbols.result())), returnType)
    }
    builder += s.SymbolInformation(
      symbol = methodSymbol,
      language = l.SCALA,
      kind = k.METHOD,
      properties = props.foldLeft(0)((acc, prop) => acc | prop.value),
      displayName = methodName,
      signature = methodSig,
      access = s.PublicAccess(),
      annotations = Nil,
      overriddenSymbols = Nil,
    )
  }

}

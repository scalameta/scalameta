package scala.meta.internal.scalacp

import scala.meta.internal.io.PathIO
import scala.meta.internal.metacp._
import scala.meta.internal.{semanticdb => s}
import scala.meta.internal.semanticdb.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb.{Language => l}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb.SymbolInformation.{Property => p}

object Scalalib {
  def anyClass: ToplevelInfos = {
    val symbols = List(
      builtinMethod("Any", List(p.ABSTRACT), "equals", Nil, List("that" -> "scala.Any#"), "scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "==", Nil, List("that" -> "scala.Any#"), "scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "!=", Nil, List("that" -> "scala.Any#"), "scala.Boolean#"),
      builtinMethod("Any", List(p.ABSTRACT), "hashCode", Nil, Nil, "scala.Int#"),
      builtinMethod("Any", List(p.FINAL), "##", Nil, Nil, "scala.Int#"),
      builtinMethod("Any", List(p.ABSTRACT), "toString", Nil, Nil, "java.lang.String#"),
      // NOTE: Return type of getClass can't be expressed in the SemanticDB type system.
      // The method is special-cased in both the Java and Scala compilers, so we'll slack a little bit too for the time being.
      builtinMethod("Any", List(p.FINAL), "getClass", Nil, Nil, "java.lang.Class#"),
      builtinMethod("Any", List(p.FINAL), "isInstanceOf", List("A"), Nil, "scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "asInstanceOf", List("A"), Nil, "scala.Any.asInstanceOf(A).[A]"))
    builtin(k.CLASS, List(p.ABSTRACT), "Any", Nil, symbols.flatten)
  }

  def anyValClass: ToplevelInfos = {
    builtin(k.CLASS, List(p.ABSTRACT), "AnyVal", List("scala.Any#"), Nil)
  }

  def anyRefClass: ToplevelInfos = {
    // FIXME: https://github.com/scalameta/scalameta/issues/1564
    val symbols = List(
      builtinMethod("AnyRef", List(p.FINAL), "eq", Nil, List("that" -> "scala.AnyRef#"), "scala.Boolean#"),
      builtinMethod("AnyRef", List(p.FINAL), "ne", Nil, List("that" -> "scala.AnyRef#"), "scala.Boolean#"),
      builtinMethod("AnyRef", List(p.FINAL), "synchronized", List("T"), List("body" -> "scala.AnyRef.synchronized(T).[T]"), "scala.AnyRef.synchronized(T).[T]"))
    builtin(k.CLASS, Nil, "AnyRef", List("scala.Any#"), symbols.flatten)
  }

  def nothingClass: ToplevelInfos = {
    builtin(k.CLASS, List(p.ABSTRACT, p.FINAL), "Nothing", List("scala.Any#"), Nil)
  }

  def nullClass: ToplevelInfos = {
    builtin(k.CLASS, List(p.ABSTRACT, p.FINAL), "Null", List("scala.AnyRef#"), Nil)
  }

  def singletonTrait: ToplevelInfos = {
    builtin(k.TRAIT, Nil, "Singleton", List("scala.Any#"), Nil)
  }

  private def builtin(
      kind: s.SymbolInformation.Kind,
      props: List[s.SymbolInformation.Property],
      name: String,
      bases: List[String],
      symbols: List[s.SymbolInformation]): ToplevelInfos = {
    val parents = bases.map { base =>
      s.TypeRef(s.NoType, base, Nil)
    }
    val symbol = "scala." + name + "#"
    val ctorSig = s.MethodSignature(Some(s.Scope(Nil)), List(s.Scope(Nil)), s.NoType)
    val ctor = s.SymbolInformation(
      symbol = symbol + "`<init>`().",
      language = l.SCALA,
      kind = k.CONSTRUCTOR,
      properties = p.PRIMARY.value,
      name = "<init>",
      signature = ctorSig,
      accessibility = Some(s.Accessibility(a.PUBLIC))
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
      name = name,
      signature = builtinSig,
      accessibility = Some(s.Accessibility(a.PUBLIC))
    )
    val syntheticBase = PathIO.workingDirectory
    val syntheticPath = syntheticBase.resolve("scala/" + name + ".class")
    val syntheticClassfile = ToplevelClassfile(syntheticBase, syntheticPath, null)
    ToplevelInfos(syntheticClassfile, List(builtin), if (kind.isClass) ctor +: symbols else symbols)
  }

  def builtinMethod(
      className: String,
      props: List[s.SymbolInformation.Property],
      methodName: String,
      tparamDsls: List[String],
      paramDsls: List[(String, String)],
      retTpeSymbol: String): List[s.SymbolInformation] = {
    val classSymbol = "scala." + className + "#"
    val encodedMethodName = {
      if (Character.isJavaIdentifierStart(methodName.head)) methodName
      else "`" + methodName + "`"
    }
    val methodSymbol = classSymbol + encodedMethodName + "()."
    val tparams = tparamDsls.map { tparamName =>
      val tparamSymbol = methodSymbol + "[" + tparamName + "]"
      val tparamSig = s.TypeSignature()
      s.SymbolInformation(
        symbol = tparamSymbol,
        language = l.SCALA,
        kind = k.TYPE_PARAMETER,
        properties = 0,
        name = tparamName,
        signature = tparamSig,
        accessibility = None)
    }
    val params = paramDsls.map {
      case (paramName, paramTpeSymbol) =>
        val paramSymbol = methodSymbol + "(" + paramName + ")"
        val paramSig = s.ValueSignature(s.TypeRef(s.NoType, paramTpeSymbol, Nil))
        s.SymbolInformation(
          symbol = paramSymbol,
          language = l.SCALA,
          kind = k.PARAMETER,
          properties = 0,
          name = paramName,
          signature = paramSig)
    }
    val methodSig = {
      val tps = Some(s.Scope(tparams.map(_.symbol)))
      val returnType = s.TypeRef(s.NoType, retTpeSymbol, Nil)
      s.MethodSignature(tps, List(s.Scope(params.map(_.symbol))), returnType)
    }
    val method = s.SymbolInformation(
      symbol = methodSymbol,
      language = l.SCALA,
      kind = k.METHOD,
      properties = props.foldLeft(0)((acc, prop) => acc | prop.value),
      name = methodName,
      signature = methodSig,
      accessibility = Some(s.Accessibility(a.PUBLIC)))
    List(method) ++ tparams ++ params
  }
}

package scala.meta.internal.scalacp

import org.langmeta.internal.io.PathIO
import scala.meta.internal.metacp._
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.{Language => l}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb3.Type.{Tag => t}

object Scalalib {
  def anyClass: ToplevelInfos = {
    val symbols = List(
      builtinMethod("Any", List(p.ABSTRACT), "equals", Nil, List("that" -> "scala.Any#"), "scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "==", Nil, List("that" -> "scala.Any#"), "scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "!=", Nil, List("that" -> "scala.Any#"), "scala.Boolean#"),
      builtinMethod("Any", List(p.ABSTRACT), "hashCode", Nil, Nil, "scala.Int#"),
      builtinMethod("Any", List(p.FINAL), "##", Nil, Nil, "scala.Int#"),
      builtinMethod("Any", List(p.ABSTRACT), "toString", Nil, Nil, "java.lang.String#"),
      // TODO: Return type of getClass can't be expressed in the SemanticDB type system.
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
    // TODO: We're not including methods from java.lang.Object here.
    // The relationship between AnyRef and Object needs more thinking.
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
    builtin(k.TRAIT, List(p.FINAL), "Singleton", List("scala.Any#"), Nil)
  }

  private def builtin(
      kind: s.SymbolInformation.Kind,
      props: List[s.SymbolInformation.Property],
      name: String,
      bases: List[String],
      symbols: List[s.SymbolInformation]): ToplevelInfos = {
    val parents = bases.map { base =>
      s.Type(tag = t.TYPE_REF, typeRef = Some(s.TypeRef(None, base, Nil)))
    }
    val symbol = "scala." + name + "#"
    val builtinTpe = s.Type(tag = t.TYPE_REF, typeRef = Some(s.TypeRef(None, symbol, Nil)))
    val ctorSig = s.MethodType(Nil, List(s.MethodType.ParameterList(Nil)), Some(builtinTpe))
    val ctor = s.SymbolInformation(
      symbol = symbol + "`<init>`().",
      language = l.SCALA,
      kind = k.CONSTRUCTOR,
      properties = p.PRIMARY.value,
      name = "<init>",
      tpe = Some(s.Type(tag = t.METHOD_TYPE, methodType = Some(ctorSig))),
      accessibility = Some(s.Accessibility(a.PUBLIC)),
      owner = symbol
    )
    val builtinSig = {
      val decls = symbols.filter(_.kind.isMethod)
      val declSymbols = decls.map(_.symbol)
      val declarations = if (kind.isClass) ctor.symbol +: declSymbols else declSymbols
      val tpe = s.ClassInfoType(Nil, parents, declarations)
      s.Type(tag = t.CLASS_INFO_TYPE, classInfoType = Some(tpe))
    }
    val builtin = s.SymbolInformation(
      symbol = symbol,
      language = l.SCALA,
      kind = kind,
      properties = props.foldLeft(0)((acc, prop) => acc | prop.value),
      name = name,
      tpe = Some(builtinSig),
      accessibility = Some(s.Accessibility(a.PUBLIC)),
      owner = "scala."
    )
    val syntheticBase = PathIO.workingDirectory
    val syntheticPath = syntheticBase.resolve("scala/" + name + ".class")
    val syntheticClassfile = ToplevelClassfile(syntheticBase, syntheticPath, null)
    ToplevelInfos(syntheticClassfile, List(builtin), ctor +: symbols)
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
    val disambiguator = {
      val paramTypeDescriptors = paramDsls.map(_._2).map { symbol =>
        // TODO: It would be nice to have a symbol parser in semanticdb3.
        val _ :+ last = symbol.split("[\\.|#]").toList
        val last1 = last.stripPrefix("(").stripPrefix("[")
        val last2 = last1.stripSuffix(")").stripSuffix("]").stripSuffix("#")
        last2.stripPrefix("`").stripSuffix("`")
      }
      paramTypeDescriptors.mkString(",")
    }
    val methodSymbol = classSymbol + encodedMethodName + "(" + disambiguator + ")."
    val tparams = tparamDsls.map { tparamName =>
      val tparamSymbol = methodSymbol + "[" + tparamName + "]"
      val tparamSig = s.Type(tag = t.TYPE_TYPE, typeType = Some(s.TypeType()))
      s.SymbolInformation(
        symbol = tparamSymbol,
        language = l.SCALA,
        kind = k.TYPE_PARAMETER,
        properties = 0,
        name = tparamName,
        tpe = Some(tparamSig),
        accessibility = None,
        owner = methodSymbol)
    }
    val params = paramDsls.map {
      case (paramName, paramTpeSymbol) =>
        val paramSymbol = methodSymbol + "(" + paramName + ")"
        val paramSig = s.Type(tag = t.TYPE_REF, typeRef = Some(s.TypeRef(None, paramTpeSymbol, Nil)))
        s.SymbolInformation(
          symbol = paramSymbol,
          language = l.SCALA,
          kind = k.PARAMETER,
          properties = 0,
          name = paramName,
          tpe = Some(paramSig),
          owner = methodSymbol)
    }
    val methodSig = {
      val paramSymbols = params.map(_.symbol)
      val returnType = s.Type(tag = t.TYPE_REF, typeRef = Some(s.TypeRef(None, retTpeSymbol, Nil)))
      val methodType = s.MethodType(Nil, List(s.MethodType.ParameterList(paramSymbols)), Some(returnType))
      s.Type(tag = t.METHOD_TYPE, methodType = Some(methodType))
    }
    val method = s.SymbolInformation(
      symbol = methodSymbol,
      language = l.SCALA,
      kind = k.METHOD,
      properties = props.foldLeft(0)((acc, prop) => acc | prop.value),
      name = methodName,
      tpe = Some(methodSig),
      accessibility = Some(s.Accessibility(a.PUBLIC)),
      owner = classSymbol)
    List(method) ++ tparams ++ params
  }
}

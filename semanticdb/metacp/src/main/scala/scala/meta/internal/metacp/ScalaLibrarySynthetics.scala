package scala.meta.internal.metacp

import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.semanticdb3.Type.{Tag => t}

object ScalaLibrarySynthetics {
  def process(scalaVersion: String, out: AbsolutePath): Unit = {
    // NOTE: we currently discard scalaVersion because Any/AnyVal/AnyRef/Nothing are the same in 2.12/2.11.
    // However, this may not be the case in future Scala versions.
    val settings = Settings(d = out)
    val index = new Index
    val synthetics = List(any, anyVal, anyRef, nothing)
    synthetics.foreach { infos =>
      index.append(infos)
      infos.save(settings)
    }
    index.save(settings)
  }

  def any: ToplevelInfos = {
    val symbols = List(
      builtinMethod("Any", List(p.ABSTRACT), "equals", Nil, List("that" -> "_root_.scala.Any#"), "_root_.scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "==", Nil, List("that" -> "_root_.scala.Any#"), "_root_.scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "!=", Nil, List("that" -> "_root_.scala.Any#"), "_root_.scala.Boolean#"),
      builtinMethod("Any", List(p.ABSTRACT), "hashCode", Nil, Nil, "_root_.scala.Int#"),
      builtinMethod("Any", List(p.FINAL), "##", Nil, Nil, "_root_.scala.Int#"),
      builtinMethod("Any", List(p.ABSTRACT), "toString", Nil, Nil, "_root_.java.lang.String#"),
      // TODO: Return type of getClass can't be expressed in the SemanticDB type system.
      // The method is special-cased in both the Java and Scala compilers, so we'll slack a little bit too for the time being.
      builtinMethod("Any", List(p.FINAL), "getClass", Nil, Nil, "_root_.java.lang.Class#"),
      builtinMethod("Any", List(p.FINAL), "isInstanceOf", List("A"), Nil, "_root_.scala.Boolean#"),
      builtinMethod("Any", List(p.FINAL), "asInstanceOf", List("A"), Nil, "_root_.scala.Any.asInstanceOf(A).[A]"))
    builtinClass(List(p.ABSTRACT), "Any", Nil, symbols.flatten)
  }

  def anyVal: ToplevelInfos = {
    builtinClass(List(p.ABSTRACT), "AnyVal", List("_root_.scala.Any#"), Nil)
  }

  def anyRef: ToplevelInfos = {
    // TODO: We're not including methods from java.lang.Object here.
    // The relationship between AnyRef and Object needs more thinking.
    val symbols = List(
      builtinMethod("AnyRef", List(p.FINAL), "eq", Nil, List("that" -> "_root_.scala.AnyRef#"), "_root_.scala.Boolean#"),
      builtinMethod("AnyRef", List(p.FINAL), "ne", Nil, List("that" -> "_root_.scala.AnyRef#"), "_root_.scala.Boolean#"),
      builtinMethod("AnyRef", List(p.FINAL), "synchronized", List("T"), List("body" -> "_root_.scala.AnyRef.synchronized(T).[T]"), "_root_.scala.AnyRef.synchronized(T).[T]"))
    builtinClass(Nil, "AnyRef", List("_root_.scala.Any#"), symbols.flatten)
  }

  def nothing: ToplevelInfos = {
    builtinClass(List(p.ABSTRACT, p.FINAL), "Nothing", List("_root_.scala.Any#"), Nil)
  }

  private def builtinClass(
      props: List[s.SymbolInformation.Property],
      name: String,
      bases: List[String],
      symbols: List[s.SymbolInformation]): ToplevelInfos = {
    val parents = bases.map { base =>
      s.Type(tag = t.TYPE_REF, typeRef = Some(s.TypeRef(None, base, Nil)))
    }
    val symbol = "_root_.scala." + name + "#"
    val builtinTpe = s.Type(tag = t.TYPE_REF, typeRef = Some(s.TypeRef(None, symbol, Nil)))
    val ctorSig = s.MethodType(Nil, List(s.MethodType.ParameterList(Nil)), Some(builtinTpe))
    val ctor = s.SymbolInformation(
      symbol = symbol + "`<init>`().",
      language = Some(builtinLanguage),
      kind = k.PRIMARY_CONSTRUCTOR,
      name = "<init>",
      tpe = Some(s.Type(tag = t.METHOD_TYPE, methodType = Some(ctorSig))),
      accessibility = Some(s.Accessibility(a.PUBLIC)),
      owner = symbol
    )
    val builtinSig = {
      val decls = symbols.filter(_.kind == k.DEF)
      val tpe = s.ClassInfoType(Nil, parents, ctor.symbol +: decls.map(_.symbol))
      s.Type(tag = t.CLASS_INFO_TYPE, classInfoType = Some(tpe))
    }
    val builtin = s.SymbolInformation(
      symbol = symbol,
      language = Some(builtinLanguage),
      kind = k.CLASS,
      properties = props.foldLeft(0)((acc, prop) => acc | prop.value),
      name = name,
      tpe = Some(builtinSig),
      accessibility = Some(s.Accessibility(a.PUBLIC)),
      owner = "_root_.scala."
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
    val classSymbol = "_root_.scala." + className + "#"
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
        language = Some(builtinLanguage),
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
          language = Some(builtinLanguage),
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
      language = Some(builtinLanguage),
      kind = k.DEF,
      properties = props.foldLeft(0)((acc, prop) => acc | prop.value),
      name = methodName,
      tpe = Some(methodSig),
      accessibility = Some(s.Accessibility(a.PUBLIC)),
      owner = classSymbol)
    List(method) ++ tparams ++ params
  }

  def builtinLanguage: s.Language = {
    s.Language("Scala")
  }
}

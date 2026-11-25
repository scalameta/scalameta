package scala.meta.tests.semanticdb

import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.semanticdb.Print
import scala.meta.internal.{semanticdb => s}
import scala.meta.metap.Format

import java.io.ByteArrayOutputStream

import com.google.protobuf.CodedOutputStream

import munit.FunSuite

/**
 * Unit tests for the metap symbol renderer, pinning the deliberate differences between the
 * Declaration style (SemanticDB vocabulary) and the Definition style (Scala-like syntax) that used
 * to be duplicated blocks in SymbolInformationPrinter.
 */
class InfoPrinterSuite extends FunSuite {

  private val emptyPrinterSymtab: PrinterSymtab = PrinterSymtab.fromTextDocument(s.TextDocument())

  private def properties(props: s.SymbolInformation.Property*): Int = props
    .foldLeft(0)((acc, prop) => acc | prop.value)

  private def ref(sym: String, args: s.Type*): s.Type = s.TypeRef(s.NoType, sym, args.toList)

  private def typeParam(sym: String, name: String): s.SymbolInformation = s.SymbolInformation(
    symbol = sym,
    kind = s.SymbolInformation.Kind.TYPE_PARAMETER,
    displayName = name,
  )

  // Decode an `AppliedType` from its wire bytes, which a schema without the message also reads.
  private def applied(tpe: s.Type, args: s.Type*): s.Type = {
    def message(write: CodedOutputStream => Unit): Array[Byte] = {
      val bytes = new ByteArrayOutputStream
      val out = CodedOutputStream.newInstance(bytes)
      write(out)
      out.flush()
      bytes.toByteArray
    }
    val fields = message { out =>
      out.writeByteArray(1, tpe.asMessage.toByteArray)
      args.foreach(arg => out.writeByteArray(2, arg.asMessage.toByteArray))
    }
    s.TypeMessage.parseFrom(message(_.writeByteArray(27, fields))).toType
  }

  private val mapLambda = s.LambdaType(
    Some(s.Scope(hardlinks = List(typeParam("local0", "X")))),
    ref("scala/collection/immutable/Map#", ref("local0"), ref("local0")),
  )

  private def assertType(tpe: s.Type, expected: String): Unit =
    assertNoDiff(Print.tpe(Format.Compact, tpe, emptyPrinterSymtab), expected)

  private def param(sym: String, name: String, tpe: s.Type): s.SymbolInformation = s
    .SymbolInformation(
      symbol = sym,
      kind = s.SymbolInformation.Kind.PARAMETER,
      displayName = name,
      signature = s.ValueSignature(tpe),
    )

  private def scope(infos: s.SymbolInformation*): s.Scope = s.Scope(hardlinks = infos.toList)

  private def assertSignature(sig: s.Signature, expected: String): Unit =
    assertEquals(Print.signature(Format.Compact, sig, emptyPrinterSymtab), expected)

  private val returnType = ref("_empty_/R#")

  private def implicitParam(name: String, tpe: String, props: s.SymbolInformation.Property*) =
    param(s"_empty_/f().($name)", name, ref(tpe)).withProperties(properties(props: _*))

  /** `(x: B)(o: C, p: D): R`, where `o` and `p` have `props` */
  private def methodWithClause(props: s.SymbolInformation.Property*): s.Signature = s
    .MethodSignature(
      parameterLists = List(
        scope(param("_empty_/f().(x)", "x", ref("_empty_/B#"))),
        scope(
          implicitParam("o", "_empty_/C#", props: _*),
          implicitParam("p", "_empty_/D#", props: _*),
        ),
      ),
      returnType = returnType,
    )

  // `extension [T](t: A) def map[U](f: B)`
  private val mapGroups = List(
    s.ParamClauseGroup(
      Some(scope(typeParam("_empty_/Functor#map().[T]", "T"))),
      List(scope(param("_empty_/Functor#map().(t)", "t", ref("_empty_/A#")))),
    ),
    s.ParamClauseGroup(
      Some(scope(typeParam("_empty_/Functor#map().[U]", "U"))),
      List(scope(param("_empty_/Functor#map().(f)", "f", ref("_empty_/B#")))),
    ),
  )

  test("info printer declaration style renders SemanticDB modifier and kind words") {
    val p = s.SymbolInformation.Property
    val info = s.SymbolInformation(
      symbol = "_empty_/Declaration#",
      kind = s.SymbolInformation.Kind.CLASS,
      properties = properties(
        p.SYNTHETIC,
        p.ABSTRACT,
        p.OVERRIDE,
        p.FINAL,
        p.SEALED,
        p.IMPLICIT,
        p.LAZY,
        p.CASE,
        p.COVARIANT,
        p.CONTRAVARIANT,
        p.VAL,
        p.VAR,
        p.STATIC,
        p.PRIMARY,
        p.ENUM,
        p.DEFAULT,
        p.GIVEN,
        p.INLINE,
        p.OPEN,
        p.TRANSPARENT,
        p.INFIX,
        p.OPAQUE,
        p.EXTENSION,
        p.NATIVE,
        p.SYNCHRONIZED,
        p.TRANSIENT,
        p.VOLATILE,
      ),
      displayName = "Declaration",
    )
    val obtained = Print.info(Format.Compact, info, emptyPrinterSymtab)
    assertNoDiff(
      obtained,
      """_empty_/Declaration# => synthetic abstract final override sealed implicit lazy case covariant contravariant val var static primary enum default given inline open transparent infix opaque extension native synchronized transient volatile class Declaration""",
    )
  }

  test("info printer definition style renders inline type parameter variance") {
    val p = s.SymbolInformation.Property
    val covariant = s.SymbolInformation(
      symbol = "_empty_/Box#[A]",
      kind = s.SymbolInformation.Kind.TYPE_PARAMETER,
      properties = properties(p.SYNTHETIC, p.ABSTRACT, p.COVARIANT),
      displayName = "A",
    )
    val contravariant = s.SymbolInformation(
      symbol = "_empty_/Box#[B]",
      kind = s.SymbolInformation.Kind.TYPE_PARAMETER,
      properties = properties(p.SYNTHETIC, p.ABSTRACT, p.CONTRAVARIANT),
      displayName = "B",
    )
    val signature = s
      .ClassSignature(typeParameters = Some(s.Scope(hardlinks = List(covariant, contravariant))))
    val obtained = Print.signature(Format.Compact, signature, emptyPrinterSymtab)
    assertNoDiff(obtained, "[+A, -B]")
  }

  test("info printer definition style keeps class-only abstract and omits final objects") {
    val p = s.SymbolInformation.Property
    val abstractClass = s.SymbolInformation(
      symbol = "_empty_/AbstractClass#",
      kind = s.SymbolInformation.Kind.CLASS,
      properties = properties(p.ABSTRACT),
      displayName = "AbstractClass",
    )
    val abstractMethod = s.SymbolInformation(
      symbol = "_empty_/abstractMethod().",
      kind = s.SymbolInformation.Kind.METHOD,
      properties = properties(p.ABSTRACT),
      displayName = "abstractMethod",
    )
    val finalObject = s.SymbolInformation(
      symbol = "_empty_/FinalObject.",
      kind = s.SymbolInformation.Kind.OBJECT,
      properties = properties(p.FINAL),
      displayName = "FinalObject",
    )
    val tpe = s.StructuralType(declarations =
      Some(s.Scope(hardlinks = List(abstractClass, abstractMethod, finalObject))),
    )
    val obtained = Print.tpe(Format.Compact, tpe, emptyPrinterSymtab)
    assertNoDiff(
      obtained,
      "{ abstract class AbstractClass; def abstractMethod; object FinalObject }",
    )
  }

  test("info printer renders overriding members") {
    val p = s.SymbolInformation.Property
    def method(name: String, props: s.SymbolInformation.Property*) = s.SymbolInformation(
      symbol = s"_empty_/C#$name().",
      kind = s.SymbolInformation.Kind.METHOD,
      properties = properties(props: _*),
      displayName = name,
    )
    val infos = List(
      method("plain", p.OVERRIDE),
      method("finalMethod", p.OVERRIDE, p.FINAL),
      method("stacked", p.ABSTRACT, p.OVERRIDE),
    )
    assertNoDiff(
      infos.map(Print.info(Format.Compact, _, emptyPrinterSymtab)).mkString("\n"),
      """|_empty_/C#plain(). => override method plain
         |_empty_/C#finalMethod(). => final override method finalMethod
         |_empty_/C#stacked(). => abstract override method stacked""".stripMargin,
    )
    val tpe = s.StructuralType(declarations = Some(s.Scope(hardlinks = infos)))
    assertNoDiff(
      Print.tpe(Format.Compact, tpe, emptyPrinterSymtab),
      "{ override def plain; final override def finalMethod; override def stacked }",
    )
  }

  test("info printer definition style renders Java-only modifiers") {
    val p = s.SymbolInformation.Property
    val k = s.SymbolInformation.Kind
    def info(name: String, kind: s.SymbolInformation.Kind, prop: s.SymbolInformation.Property) = s
      .SymbolInformation(
        symbol = s"_empty_/J#$name.",
        kind = kind,
        properties = prop.value,
        displayName = name,
      )
    val tpe = s.StructuralType(declarations =
      Some(s.Scope(hardlinks =
        List(
          info("nativeMethod", k.METHOD, p.NATIVE),
          info("synchronizedMethod", k.METHOD, p.SYNCHRONIZED),
          info("transientField", k.FIELD, p.TRANSIENT),
          info("volatileField", k.FIELD, p.VOLATILE),
        ),
      )),
    )
    val obtained = Print.tpe(Format.Compact, tpe, emptyPrinterSymtab)
    assertNoDiff(
      obtained,
      "{ native def nativeMethod; synchronized def synchronizedMethod; transient transientField; volatile volatileField }",
    )
  }

  test("info printer renders <?> when the display name is empty") {
    val info = s.SymbolInformation(symbol = "local0", kind = s.SymbolInformation.Kind.LOCAL)
    val obtained = Print.info(Format.Compact, info, emptyPrinterSymtab)
    assertNoDiff(obtained, "local0 => local <?>")
  }

  test("match type") {
    val tpe = s.MatchType(
      ref("_empty_/T#"),
      List(
        s.MatchType.CaseType(ref("scala/Int#"), ref("scala/Predef.String#")),
        s.MatchType.CaseType(ref("scala/Any#"), ref("scala/Unit#")),
      ),
    )
    assertType(tpe, "T match { Int => String, Any => Unit }")
  }

  test("type lambda")(assertType(mapLambda, "[X] =>> Map[X, X]"))

  test("#1565: type application with a symbol constructor")(
    assertType(ref("scala/package.List#", ref("scala/Int#")), "List[Int]"),
  )

  test("#1565: type application with an annotated constructor") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val tpe = applied(s.AnnotatedType(List(ann), ref("scala/package.List#")), ref("scala/Int#"))
    assertType(tpe, "(List @ann)[Int]")
  }

  test("#1565: type application with a type lambda constructor and a wildcard argument") {
    val wildcard = s.SymbolInformation(
      symbol = "local1",
      kind = s.SymbolInformation.Kind.TYPE,
      displayName = "_",
      signature = s.TypeSignature(lowerBound = ref("scala/Nothing#"), upperBound = ref("scala/Any#")),
    )
    val tpe = s
      .ExistentialType(applied(mapLambda, ref("local1")), Some(s.Scope(hardlinks = List(wildcard))))
    assertType(tpe, "([X] =>> Map[X, X])[_] forSome { type _ }")
  }

  test("#1565: type application with an intersection constructor") {
    val tpe =
      applied(s.IntersectionType(List(ref("_empty_/F#"), ref("_empty_/G#"))), ref("_empty_/A#"))
    assertType(tpe, "(F & G)[A]")
  }

  test("#1565: type application with a union constructor") {
    val tpe = applied(s.UnionType(List(ref("_empty_/F#"), ref("_empty_/G#"))), ref("_empty_/A#"))
    assertType(tpe, "(F | G)[A]")
  }

  test("#1565: type application with an applied constructor")(
    assertType(applied(ref("_empty_/F#", ref("_empty_/A#")), ref("_empty_/B#")), "F[A][B]"),
  )

  test("#1565: type application with a singleton constructor")(
    assertType(applied(s.SingleType(s.NoType, "_empty_/x."), ref("_empty_/A#")), "x.type[A]"),
  )

  test("#1565: type application with a this constructor")(
    assertType(applied(s.ThisType("_empty_/C#"), ref("_empty_/A#")), "C.this.type[A]"),
  )

  test("#1565: type application with a super constructor")(
    assertType(applied(s.SuperType(s.NoType, "_empty_/C#"), ref("_empty_/A#")), "super[C].type[A]"),
  )

  test("#1565: type application with a constant constructor")(
    assertType(applied(s.ConstantType(s.IntConstant(1)), ref("_empty_/A#")), "1[A]"),
  )

  test("#1565: type application with a nested type application constructor") {
    val tpe =
      applied(applied(ref("_empty_/F#", ref("_empty_/A#")), ref("_empty_/B#")), ref("_empty_/C#"))
    assertType(tpe, "F[A][B][C]")
  }

  test("#1565: type application of a curried application, three levels") {
    val fab = applied(ref("_empty_/F#", ref("_empty_/A#")), ref("_empty_/B#"))
    assertType(applied(applied(fab, ref("_empty_/C#")), ref("_empty_/D#")), "F[A][B][C][D]")
  }

  test("#1565: type application of an applied annotated constructor") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val inner = applied(s.AnnotatedType(List(ann), ref("_empty_/F#")), ref("_empty_/A#"))
    assertType(applied(inner, ref("_empty_/B#")), "(F @ann)[A][B]")
  }

  test("#1565: type application of an applied type lambda") {
    val tpe = applied(applied(mapLambda, ref("_empty_/A#")), ref("_empty_/B#"))
    assertType(tpe, "([X] =>> Map[X, X])[A][B]")
  }

  test("#1565: type application of an intersection with an application") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val inner = applied(s.AnnotatedType(List(ann), ref("_empty_/F#")), ref("_empty_/A#"))
    val tpe = applied(s.IntersectionType(List(inner, ref("_empty_/G#"))), ref("_empty_/B#"))
    assertType(tpe, "((F @ann)[A] & G)[B]")
  }

  test("#1565: type application of a singleton, two levels") {
    val inner = applied(s.SingleType(s.NoType, "_empty_/x."), ref("_empty_/A#"))
    assertType(applied(inner, ref("_empty_/B#")), "x.type[A][B]")
  }

  test("#1565: type application as a type argument") {
    val arg =
      applied(s.IntersectionType(List(ref("_empty_/G#"), ref("_empty_/H#"))), ref("_empty_/A#"))
    assertType(ref("_empty_/F#", arg), "F[(G & H)[A]]")
  }

  test("#1565: annotated type application") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val inner =
      applied(s.IntersectionType(List(ref("_empty_/F#"), ref("_empty_/G#"))), ref("_empty_/A#"))
    assertType(s.AnnotatedType(List(ann), inner), "(F & G)[A] @ann")
  }

  test("#2450: method signature with one group") {
    val sig = s.MethodSignature(
      Some(scope(typeParam("_empty_/f().[T]", "T"))),
      List(
        scope(param("_empty_/f().(x)", "x", ref("_empty_/A#"))),
        scope(param("_empty_/f().(y)", "y", ref("_empty_/B#"))),
      ),
      ref("_empty_/C#"),
      List(ref("_empty_/E#")),
    )
    assertSignature(sig, "[T](x: A)(y: B) throws E: C")
  }

  test("#2450: extension method with two groups") {
    val sig = s.ExtendedMethodSignature(mapGroups, ref("_empty_/C#"))
    assertSignature(sig, "[T](t: A)[U](f: B): C")
  }

  test("#2450: method with a type clause after a term clause") {
    val m = "_empty_/DB#getOrElse()."
    val v = typeParam(s"$m[V]", "V").withSignature(s.TypeSignature(lowerBound = ref("_empty_/L#")))
    val groups = List(
      s.ParamClauseGroup(None, List(scope(param(s"$m(k)", "k", ref("_empty_/K#"))))),
      s.ParamClauseGroup(Some(scope(v)), List(scope(param(s"$m(default)", "default", ref(v.symbol))))),
    )
    val sig = s.ExtendedMethodSignature(groups, ref(v.symbol))
    assertSignature(sig, "(k: K)[V >: L](default: V): V")
  }

  test("#2450: right-associative extension method") {
    val m = "_empty_/ops.`+:`()."
    val groups = List(
      s.ParamClauseGroup(Some(scope(typeParam(s"$m[A]", "A")))),
      s.ParamClauseGroup(
        Some(scope(typeParam(s"$m[B]", "B"))),
        List(
          scope(param(s"$m(as)", "as", ref("scala/package.List#", ref(s"$m[B]")))),
          scope(param(s"$m(a)", "a", ref(s"$m[A]"))),
        ),
      ),
    )
    val sig = s.ExtendedMethodSignature(groups, ref("_empty_/R#"))
    assertSignature(sig, "[A][B](as: List[B])(a: A): R")
  }

  test("#2450: method with two groups and throws") {
    val sig = s.ExtendedMethodSignature(mapGroups, ref("_empty_/C#"), List(ref("_empty_/E#")))
    assertSignature(sig, "[T](t: A)[U](f: B) throws E: C")
  }

  test("#2450: extended method signature with one group") {
    val groups = List(s.ParamClauseGroup(
      Some(scope(typeParam("_empty_/f().[T]", "T"))),
      List(scope(param("_empty_/f().(x)", "x", ref("_empty_/A#")))),
    ))
    val sig = s.ExtendedMethodSignature(groups, ref("_empty_/C#"))
    assertSignature(sig, "[T](x: A): C")
  }

  test("#2450: extended method signature without groups") {
    val sig = s.ExtendedMethodSignature(Nil, ref("scala/Nothing#"))
    assertSignature(sig, ": Nothing")
  }

  test("#2450: info of an extension method with two groups") {
    val info = s.SymbolInformation(
      symbol = "_empty_/Functor#map().",
      kind = s.SymbolInformation.Kind.METHOD,
      properties = s.SymbolInformation.Property.EXTENSION.value,
      displayName = "map",
      signature = s.ExtendedMethodSignature(mapGroups, ref("_empty_/C#")),
    )
    assertEquals(
      Print.info(Format.Compact, info, emptyPrinterSymtab),
      "_empty_/Functor#map(). => extension method map[T](t: A)[U](f: B): C",
    )
  }

  test("parameter clauses: one implicit parameter") {
    val a = implicitParam("a", "_empty_/A#", s.SymbolInformation.Property.IMPLICIT)
    assertSignature(
      s.MethodSignature(parameterLists = List(scope(a)), returnType = returnType),
      "(implicit a: A): R",
    )
  }

  test("parameter clauses: two implicit parameters after a normal clause") {
    val sig = methodWithClause(s.SymbolInformation.Property.IMPLICIT)
    assertSignature(sig, "(x: B)(implicit o: C, p: D): R")
  }

  test("parameter clauses: two using parameters after a normal clause") {
    val sig =
      methodWithClause(s.SymbolInformation.Property.GIVEN, s.SymbolInformation.Property.IMPLICIT)
    assertSignature(sig, "(x: B)(using o: C, p: D): R")
  }

  test("parameter clauses: annotated implicit parameter") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val a = implicitParam("a", "_empty_/A#", s.SymbolInformation.Property.IMPLICIT)
      .withAnnotations(List(ann))
    assertSignature(
      s.MethodSignature(parameterLists = List(scope(a)), returnType = returnType),
      "(implicit @ann a: A): R",
    )
  }

  test("parameter clauses: implicit val parameters of a constructor") {
    val p = s.SymbolInformation.Property
    val a = implicitParam("a", "_empty_/A#", p.IMPLICIT, p.VAL)
    val b = implicitParam("b", "_empty_/B#", p.IMPLICIT, p.VAL)
    assertSignature(
      s.MethodSignature(parameterLists = List(scope(a, b))),
      "(implicit val a: A, val b: B)",
    )
  }

  test("parameter clauses: extension method with using clauses") {
    val p = s.SymbolInformation.Property
    val groups = List(
      s.ParamClauseGroup(
        None,
        List(
          scope(param("_empty_/f().(t)", "t", ref("_empty_/A#"))),
          scope(implicitParam("o", "_empty_/C#", p.GIVEN, p.IMPLICIT)),
        ),
      ),
      s.ParamClauseGroup(
        None,
        List(
          scope(param("_empty_/f().(x)", "x", ref("_empty_/B#"))),
          scope(implicitParam("p", "_empty_/D#", p.GIVEN, p.IMPLICIT)),
        ),
      ),
    )
    val sig = s.ExtendedMethodSignature(groups, returnType)
    assertSignature(sig, "(t: A)(using o: C)(x: B)(using p: D): R")
  }

  test("parameter clauses: using parameters by symlinks") {
    val p = s.SymbolInformation.Property
    val params = List(
      implicitParam("o", "_empty_/C#", p.GIVEN, p.IMPLICIT),
      implicitParam("p", "_empty_/D#", p.GIVEN, p.IMPLICIT),
    )
    val symtab = PrinterSymtab.fromTextDocument(s.TextDocument(symbols = params))
    val sig = s.MethodSignature(
      parameterLists = List(s.Scope(symlinks = params.map(_.symbol))),
      returnType = returnType,
    )
    assertEquals(Print.signature(Format.Compact, sig, symtab), "(using o: C, p: D): R")
  }

  test("parameter clauses: symlinks without infos") {
    val sig = s.MethodSignature(
      parameterLists = List(s.Scope(symlinks = List("_empty_/f().(o)", "_empty_/f().(p)"))),
      returnType = returnType,
    )
    assertSignature(sig, "(unknown o, unknown p): R")
  }

  test("parameter clauses: info of an implicit and of a using parameter") {
    val p = s.SymbolInformation.Property
    val infos = List(
      implicitParam("a", "_empty_/A#", p.IMPLICIT),
      implicitParam("o", "_empty_/C#", p.GIVEN, p.IMPLICIT),
    )
    assertNoDiff(
      infos.map(Print.info(Format.Compact, _, emptyPrinterSymtab)).mkString("\n"),
      """|_empty_/f().(a) => implicit param a: A
         |_empty_/f().(o) => implicit given param o: C""".stripMargin,
    )
  }

  test("parameter clauses: implicit method of a structural type") {
    val f = s.SymbolInformation(
      symbol = "_empty_/S#f().",
      kind = s.SymbolInformation.Kind.METHOD,
      properties = s.SymbolInformation.Property.IMPLICIT.value,
      displayName = "f",
      signature = s.MethodSignature(returnType = ref("_empty_/A#")),
    )
    assertType(s.StructuralType(declarations = Some(scope(f))), "{ implicit def f: A }")
  }

  /**
   * Returns the info of a symbol as dotc writes it: a term without parameter clauses has a value
   * signature
   */
  private def dotc(
      sym: String,
      name: String,
      kind: s.SymbolInformation.Kind,
      sig: s.Signature,
      props: s.SymbolInformation.Property*,
  ) = s.SymbolInformation(
    symbol = sym,
    kind = kind,
    properties = properties(props: _*),
    displayName = name,
    signature = sig,
  )

  private def assertInfo(info: s.SymbolInformation, expected: String): Unit =
    assertEquals(Print.info(Format.Compact, info, emptyPrinterSymtab), expected)

  private def assertMember(info: s.SymbolInformation, decl: String, defn: String): Unit = {
    assertInfo(info, decl)
    assertType(s.StructuralType(declarations = Some(scope(info))), defn)
  }

  private val byName = s.ValueSignature(s.ByNameType(returnType))

  test("dotc: def without parameter clauses")(assertMember(
    dotc("_empty_/C#f().", "f", s.SymbolInformation.Kind.METHOD, byName),
    "_empty_/C#f(). => method f: R",
    "{ def f: R }",
  ))

  test("dotc: val")(assertMember(
    dotc(
      "_empty_/C#x.",
      "x",
      s.SymbolInformation.Kind.METHOD,
      s.ValueSignature(returnType),
      s.SymbolInformation.Property.VAL,
    ),
    "_empty_/C#x. => val method x: R",
    "{ val x: R }",
  ))

  test("dotc: inline def without parameter clauses")(assertMember(
    dotc(
      "_empty_/C#g().",
      "g",
      s.SymbolInformation.Kind.MACRO,
      byName,
      s.SymbolInformation.Property.INLINE,
    ),
    "_empty_/C#g(). => inline macro g: R",
    "{ inline def g: R }",
  ))

  test("dotc: var")(assertMember(
    dotc(
      "_empty_/C#v().",
      "v",
      s.SymbolInformation.Kind.METHOD,
      s.ValueSignature(returnType),
      s.SymbolInformation.Property.VAR,
    ),
    "_empty_/C#v(). => var method v: R",
    "{ var v: R }",
  ))

  test("dotc: inline def with a parameter")(assertMember(
    dotc(
      "_empty_/C#h().",
      "h",
      s.SymbolInformation.Kind.MACRO,
      s.MethodSignature(
        parameterLists = List(scope(param("_empty_/C#h().(y)", "y", ref("_empty_/B#")))),
        returnType = returnType,
      ),
      s.SymbolInformation.Property.INLINE,
    ),
    "_empty_/C#h(). => inline macro h(y: B): R",
    "{ inline def h(y: B): R }",
  ))

  test("dotc: transparent inline def")(assertMember(
    dotc(
      "_empty_/C#t().",
      "t",
      s.SymbolInformation.Kind.MACRO,
      byName,
      s.SymbolInformation.Property.INLINE,
      s.SymbolInformation.Property.TRANSPARENT,
    ),
    "_empty_/C#t(). => inline transparent macro t: R",
    "{ inline transparent def t: R }",
  ))

  test("dotc: inline def with an inline parameter") {
    assertMember(
      dotc(
        "_empty_/C#p().",
        "p",
        s.SymbolInformation.Kind.MACRO,
        s.MethodSignature(
          parameterLists = List(scope(
            param("_empty_/C#p().(y)", "y", ref("_empty_/B#"))
              .copy(properties = s.SymbolInformation.Property.INLINE.value),
          )),
          returnType = returnType,
        ),
        s.SymbolInformation.Property.INLINE,
      ),
      "_empty_/C#p(). => inline macro p(inline y: B): R",
      "{ inline def p(inline y: B): R }",
    )
  }

  test("dotc: inline def with an inline using parameter") {
    assertMember(
      dotc(
        "_empty_/C#u().",
        "u",
        s.SymbolInformation.Kind.MACRO,
        s.MethodSignature(
          parameterLists = List(
            scope(param("_empty_/C#u().(y)", "y", ref("_empty_/B#")).copy(properties =
              properties(
                s.SymbolInformation.Property.GIVEN,
                s.SymbolInformation.Property.IMPLICIT,
                s.SymbolInformation.Property.INLINE,
              ),
            )),
          ),
          returnType = returnType,
        ),
        s.SymbolInformation.Property.INLINE,
      ),
      "_empty_/C#u(). => inline macro u(using inline y: B): R",
      "{ inline def u(using inline y: B): R }",
    )
  }

  test("dotc: infix def")(assertMember(
    dotc(
      "_empty_/C#op().",
      "op",
      s.SymbolInformation.Kind.METHOD,
      s.MethodSignature(
        parameterLists = List(scope(param("_empty_/C#op().(y)", "y", ref("_empty_/B#")))),
        returnType = returnType,
      ),
      s.SymbolInformation.Property.INFIX,
    ),
    "_empty_/C#op(). => infix method op(y: B): R",
    "{ infix def op(y: B): R }",
  ))

  private def selfRefined(member: s.SymbolInformation) = s.SymbolInformation(
    symbol = "_empty_/O.",
    kind = s.SymbolInformation.Kind.OBJECT,
    properties = properties(s.SymbolInformation.Property.FINAL),
    displayName = "O",
    signature = s.ClassSignature(self =
      s.StructuralType(s.SingleType(s.NoType, "_empty_/O."), Some(scope(member))),
    ),
  )

  test("dotc: opaque type in a self type")(assertInfo(
    selfRefined(dotc(
      "_empty_/O.A#",
      "A",
      s.SymbolInformation.Kind.TYPE,
      s.TypeSignature(),
      s.SymbolInformation.Property.OPAQUE,
    )),
    "_empty_/O. => final object O { self: O.type { opaque type A } =>  }",
  ))

  test("dotc: opaque infix type in a self type")(assertInfo(
    selfRefined(dotc(
      "_empty_/O.Or#",
      "Or",
      s.SymbolInformation.Kind.TYPE,
      s.TypeSignature(Some(
        scope(typeParam("_empty_/O.Or#[A]", "A"), typeParam("_empty_/O.Or#[B]", "B")),
      )),
      s.SymbolInformation.Property.INFIX,
      s.SymbolInformation.Property.OPAQUE,
    )),
    "_empty_/O. => final object O { self: O.type { infix opaque type Or[A, B] } =>  }",
  ))

  test("scalac: implicit val") {
    val info = s.SymbolInformation(
      symbol = "_empty_/C#i().",
      kind = s.SymbolInformation.Kind.METHOD,
      properties =
        properties(s.SymbolInformation.Property.IMPLICIT, s.SymbolInformation.Property.VAL),
      displayName = "i",
      signature = s.MethodSignature(returnType = returnType),
    )
    assertMember(info, "_empty_/C#i(). => implicit val method i: R", "{ implicit val i: R }")
  }

  test("scalac: macro def")(assertMember(
    s.SymbolInformation(
      symbol = "_empty_/C#m().",
      kind = s.SymbolInformation.Kind.MACRO,
      displayName = "m",
      signature = s.MethodSignature(returnType = returnType),
    ),
    "_empty_/C#m(). => macro m: R",
    "{ def m: R }",
  ))

  test("dotc: local def without parameter clauses") {
    val info = dotc("local0", "f", s.SymbolInformation.Kind.LOCAL, byName)
    assertInfo(info, "local0 => local f: R")
  }

  test("dotc: local def with a parameter") {
    val sig = s.MethodSignature(
      parameterLists = List(scope(param("local2", "x", ref("_empty_/B#")))),
      returnType = returnType,
    )
    val info = dotc("local1", "g", s.SymbolInformation.Kind.LOCAL, sig)
    assertInfo(info, "local1 => local g(x: B): R")
  }

  test("dotc: local inline def with an inline parameter") {
    val x = param("local7", "x", ref("_empty_/B#"))
      .copy(properties = s.SymbolInformation.Property.INLINE.value)
    val sig = s.MethodSignature(parameterLists = List(scope(x)), returnType = returnType)
    val info =
      dotc("local6", "g", s.SymbolInformation.Kind.LOCAL, sig, s.SymbolInformation.Property.INLINE)
    assertInfo(info, "local6 => inline local g(inline x: B): R")
  }

  test("dotc: local val")(assertInfo(
    dotc(
      "local3",
      "x",
      s.SymbolInformation.Kind.LOCAL,
      s.ValueSignature(returnType),
      s.SymbolInformation.Property.VAL,
    ),
    "local3 => val local x: R",
  ))

  test("dotc: local def with a type parameter") {
    val sig = s.MethodSignature(Some(scope(typeParam("local5", "T"))), returnType = ref("local5"))
    val info = dotc("local4", "g", s.SymbolInformation.Kind.LOCAL, sig)
    assertInfo(info, "local4 => local g[T]: T")
  }

  test("val with a type parameter")(assertMember(
    dotc(
      "_empty_/C#w().",
      "w",
      s.SymbolInformation.Kind.METHOD,
      s.MethodSignature(Some(scope(typeParam("_empty_/C#w().[T]", "T"))), returnType = returnType),
      s.SymbolInformation.Property.VAL,
    ),
    "_empty_/C#w(). => val method w[T]: R",
    "{ def w[T]: R }",
  ))

  test("val with an extended method signature")(assertMember(
    dotc(
      "_empty_/C#e().",
      "e",
      s.SymbolInformation.Kind.METHOD,
      s.ExtendedMethodSignature(
        List(s.ParamClauseGroup(None, List(scope(param("_empty_/C#e().(y)", "y", ref("_empty_/B#")))))),
        returnType,
      ),
      s.SymbolInformation.Property.VAL,
    ),
    "_empty_/C#e(). => val method e(y: B): R",
    "{ def e(y: B): R }",
  ))

  test("dotc: inline val")(assertMember(
    dotc(
      "_empty_/C#n.",
      "n",
      s.SymbolInformation.Kind.METHOD,
      s.ValueSignature(returnType),
      s.SymbolInformation.Property.VAL,
      s.SymbolInformation.Property.INLINE,
    ),
    "_empty_/C#n. => val inline method n: R",
    "{ inline val n: R }",
  ))

  test("by-name parameter") {
    val x = param("_empty_/f().(x)", "x", s.ByNameType(returnType))
    assertInfo(x, "_empty_/f().(x) => param x: => R")
    assertSignature(s.MethodSignature(parameterLists = List(scope(x))), "(x: => R)")
  }

}

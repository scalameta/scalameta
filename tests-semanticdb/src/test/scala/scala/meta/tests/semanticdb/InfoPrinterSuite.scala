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

  test("info printer declaration style renders SemanticDB modifier and kind words") {
    val p = s.SymbolInformation.Property
    val info = s.SymbolInformation(
      symbol = "_empty_/Declaration#",
      kind = s.SymbolInformation.Kind.CLASS,
      properties = properties(
        p.SYNTHETIC,
        p.ABSTRACT,
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
      """_empty_/Declaration# => synthetic abstract final sealed implicit lazy case covariant contravariant val var static primary enum default given inline open transparent infix opaque extension native synchronized transient volatile class Declaration""",
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
    assertType(tpe, "<?>")
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
    assertType(tpe, "forSome { type _ }")
  }

  test("#1565: type application with an intersection constructor") {
    val tpe =
      applied(s.IntersectionType(List(ref("_empty_/F#"), ref("_empty_/G#"))), ref("_empty_/A#"))
    assertType(tpe, "<?>")
  }

  test("#1565: type application with a union constructor") {
    val tpe = applied(s.UnionType(List(ref("_empty_/F#"), ref("_empty_/G#"))), ref("_empty_/A#"))
    assertType(tpe, "<?>")
  }

  test("#1565: type application with an applied constructor")(
    assertType(applied(ref("_empty_/F#", ref("_empty_/A#")), ref("_empty_/B#")), "<?>"),
  )

  test("#1565: type application with a singleton constructor")(
    assertType(applied(s.SingleType(s.NoType, "_empty_/x."), ref("_empty_/A#")), "<?>"),
  )

  test("#1565: type application with a this constructor")(
    assertType(applied(s.ThisType("_empty_/C#"), ref("_empty_/A#")), "<?>"),
  )

  test("#1565: type application with a super constructor")(
    assertType(applied(s.SuperType(s.NoType, "_empty_/C#"), ref("_empty_/A#")), "<?>"),
  )

  test("#1565: type application with a constant constructor")(
    assertType(applied(s.ConstantType(s.IntConstant(1)), ref("_empty_/A#")), "<?>"),
  )

  test("#1565: type application with a nested type application constructor") {
    val tpe =
      applied(applied(ref("_empty_/F#", ref("_empty_/A#")), ref("_empty_/B#")), ref("_empty_/C#"))
    assertType(tpe, "<?>")
  }

  test("#1565: type application of a curried application, three levels") {
    val fab = applied(ref("_empty_/F#", ref("_empty_/A#")), ref("_empty_/B#"))
    assertType(applied(applied(fab, ref("_empty_/C#")), ref("_empty_/D#")), "<?>")
  }

  test("#1565: type application of an applied annotated constructor") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val inner = applied(s.AnnotatedType(List(ann), ref("_empty_/F#")), ref("_empty_/A#"))
    assertType(applied(inner, ref("_empty_/B#")), "<?>")
  }

  test("#1565: type application of an applied type lambda") {
    val tpe = applied(applied(mapLambda, ref("_empty_/A#")), ref("_empty_/B#"))
    assertType(tpe, "<?>")
  }

  test("#1565: type application of an intersection with an application") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val inner = applied(s.AnnotatedType(List(ann), ref("_empty_/F#")), ref("_empty_/A#"))
    val tpe = applied(s.IntersectionType(List(inner, ref("_empty_/G#"))), ref("_empty_/B#"))
    assertType(tpe, "<?>")
  }

  test("#1565: type application of a singleton, two levels") {
    val inner = applied(s.SingleType(s.NoType, "_empty_/x."), ref("_empty_/A#"))
    assertType(applied(inner, ref("_empty_/B#")), "<?>")
  }

  test("#1565: type application as a type argument") {
    val arg =
      applied(s.IntersectionType(List(ref("_empty_/G#"), ref("_empty_/H#"))), ref("_empty_/A#"))
    assertType(ref("_empty_/F#", arg), "F[<?>]")
  }

  test("#1565: annotated type application") {
    val ann = s.AnnotationTree(ref("_empty_/ann#"))
    val inner =
      applied(s.IntersectionType(List(ref("_empty_/F#"), ref("_empty_/G#"))), ref("_empty_/A#"))
    assertType(s.AnnotatedType(List(ann), inner), "@ann")
  }

}

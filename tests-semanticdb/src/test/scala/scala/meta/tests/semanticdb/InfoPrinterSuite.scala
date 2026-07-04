package scala.meta.tests.semanticdb

import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.semanticdb.Print
import scala.meta.internal.{semanticdb => s}
import scala.meta.metap.Format

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
      ),
      displayName = "Declaration",
    )
    val obtained = Print.info(Format.Compact, info, emptyPrinterSymtab)
    assertNoDiff(
      obtained,
      """_empty_/Declaration# => synthetic abstract final sealed implicit lazy case covariant contravariant val var static primary enum default given inline open transparent infix opaque class Declaration""",
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

  test("info printer renders <?> when the display name is empty") {
    val info = s.SymbolInformation(symbol = "local0", kind = s.SymbolInformation.Kind.LOCAL)
    val obtained = Print.info(Format.Compact, info, emptyPrinterSymtab)
    assertNoDiff(obtained, "local0 => local <?>")
  }
}

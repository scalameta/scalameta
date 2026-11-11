package scala.meta.tests.semanticdb.dotc

import scala.meta.internal.{semanticdb => s}

import dotty.tools.dotc.semanticdb.internal.{SemanticdbGeneratedMessage,
  SemanticdbGeneratedMessageCompanion}
import dotty.tools.dotc.{semanticdb => d}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}

/**
 * Each test encodes a message with the module's classes and decodes it with semanticdb-shared's,
 * and the reverse. The test then encodes each decoded message again, and decodes it with the
 * classes that encoded the original.
 */
class DotcRoundTripSuite extends munit.FunSuite {

  private def check[D <: SemanticdbGeneratedMessage, S <: GeneratedMessage](
      name: String,
  )(dValue: D, sValue: S)(implicit
      dc: SemanticdbGeneratedMessageCompanion[D],
      sc: GeneratedMessageCompanion[S],
  ): Unit = test(name) {
    val sFromD = sc.parseFrom(dValue.toByteArray)
    assertEquals(sFromD, sValue)
    assertEquals(dc.parseFrom(sFromD.toByteArray), dValue)
    val dFromS = dc.parseFrom(sValue.toByteArray)
    assertEquals(dFromS, dValue)
    assertEquals(sc.parseFrom(dFromS.toByteArray), sValue)
  }

  check("string: empty")(d.Scope(symlinks = List("")), s.Scope(symlinks = List("")))
  check("string: ASCII")(d.TextDocument(uri = "a/B.scala"), s.TextDocument(uri = "a/B.scala"))
  check("string: non-ASCII")(d.TextDocument(text = "é → 𝄞"), s.TextDocument(text = "é → 𝄞"))
  check("string: U+FFFD")(d.TextDocument(text = "a\uFFFDb"), s.TextDocument(text = "a\uFFFDb"))

  check("int32: 0")(d.Range(startLine = 0), s.Range(startLine = 0))
  check("int32: 128")(d.Range(startLine = 128), s.Range(startLine = 128))
  check("int32: -1")(d.IntConstant(-1), s.IntConstant(-1))
  check("int64: -1")(d.LongConstant(-1L), s.LongConstant(-1L))
  check("int64: MaxValue")(d.LongConstant(Long.MaxValue), s.LongConstant(Long.MaxValue))

  check("float: 0")(d.FloatConstant(0f), s.FloatConstant(0f))
  check("float: 1.5")(d.FloatConstant(1.5f), s.FloatConstant(1.5f))
  check("double: 0")(d.DoubleConstant(0d), s.DoubleConstant(0d))
  check("double: 1.5")(d.DoubleConstant(1.5), s.DoubleConstant(1.5))

  check("bool")(d.BooleanConstant(true), s.BooleanConstant(true))

  check("enum: zero")(
    d.TextDocument(language = d.Language.UNKNOWN_LANGUAGE),
    s.TextDocument(language = s.Language.UNKNOWN_LANGUAGE),
  )

  check("enum: another value")(
    d.TextDocument(language = d.Language.SCALA),
    s.TextDocument(language = s.Language.SCALA),
  )

  check("enum: undeclared number")(
    d.TextDocument(language = d.Language.Unrecognized(42)),
    s.TextDocument(language = s.Language.Unrecognized(42)),
  )

  check("oneof: unset")(
    d.SymbolInformation(signature = d.Signature.Empty),
    s.SymbolInformation(signature = s.Signature.Empty),
  )

  check("oneof: set")(
    d.SymbolInformation(signature = d.ValueSignature(d.TypeRef(symbol = "a/B#"))),
    s.SymbolInformation(signature = s.ValueSignature(s.TypeRef(symbol = "a/B#"))),
  )

  check("repeated: none")(d.Scope(symlinks = Nil), s.Scope(symlinks = Nil))
  check("repeated: many")(
    d.Scope(symlinks = List("a", "b", "c")),
    s.Scope(symlinks = List("a", "b", "c")),
  )

  check("message in a message of its own type")(
    d.TypeRef(prefix = d.TypeRef(prefix = d.TypeRef(symbol = "a/"), symbol = "a/B#")),
    s.TypeRef(prefix = s.TypeRef(prefix = s.TypeRef(symbol = "a/"), symbol = "a/B#")),
  )

  check("field number of 16 or more")(
    d.SymbolInformation(language = d.Language.SCALA, overriddenSymbols = List("a/B#c().")),
    s.SymbolInformation(language = s.Language.SCALA, overriddenSymbols = List("a/B#c().")),
  )

}

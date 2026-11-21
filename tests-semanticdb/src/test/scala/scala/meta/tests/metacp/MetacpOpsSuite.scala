package scala.meta.tests.metacp

import scala.meta.internal.{semanticdb => s}

import munit.FunSuite

class MetacpOpsSuite extends FunSuite {

  private def ref(sym: String): s.Type = s.TypeRef(s.NoType, sym, Nil)

  private def references(sig: s.Signature): List[String] = {
    val refs = List.newBuilder[String]
    MetacpOps.collectAllSymbolReferences(s.SymbolInformation("a/C#m().", signature = sig))(refs += _)
    refs.result()
  }

  private val typeParams = Some(s.Scope(hardlinks = List(s.SymbolInformation("a/C#m().[X]"))))
  private val params = s.Scope(symlinks = List("a/C#m().(x)"))
  private val throws = List(ref("a/C#m().[X]"), ref("a/E#"))

  test("method signature with throws") {
    val sig = s.MethodSignature(typeParams, List(params), ref("a/R#"), throws)
    assertEquals(references(sig), List("a/C#m().(x)", "a/R#"))
  }

  test("extended method signature with throws") {
    val sig = s.ExtendedMethodSignature(
      List(s.ParamClauseGroup(typeParams), s.ParamClauseGroup(None, List(params))),
      ref("a/R#"),
      throws,
    )
    val e = intercept[RuntimeException](references(sig))
    assertEquals(e.getMessage.takeWhile(_ != ':'), "unexpected signature")
  }

}

package scala.meta.tests
package prettyprinters

import org.scalatest._
import scala.meta._
import scala.meta.internal.semantic._
import scala.meta.internal.prettyprinters.Attributes
import scala.meta.internal.semantic.Typing.Recursive

class SemanticSuite extends FunSuite {
  test("comprehensive show[Attributes]") {
    val symbolroot = Symbol.RootPackage
    val prefixroot = Prefix.None
    val denotroot = Denotation.Single(prefixroot, symbolroot)
    val termroot = Term.Name("_root_").withAttrs(denotroot, Recursive).setTypechecked
    val typeroot = Type.Singleton(termroot).setTypechecked

    val symbolBar = Symbol.Global(symbolroot, ScalaSig.Type("Bar"), BinarySig.JvmErasure("Bar"))
    val prefixBar = Prefix.Type(typeroot)
    val denotBar = Denotation.Single(prefixBar, symbolBar)
    val typeBar = Type.Name("Bar").withAttrs(denotBar).setTypechecked

    val symbolx = Symbol.Local("x")
    val prefixx = Prefix.None
    val denotx = Denotation.Single(prefixx, symbolx)
    val typex = typeBar
    val x = Term.Name("x").withAttrs(denotx, typex)

    assert(x.show[Attributes] === """
      |Term.Name("x")[1]{1}*
      |[1] {0}::local#x
      |[2] {2}::_root_#Bar
      |[3] {0}::_root_
      |{1} Type.Name("Bar")[2]
      |{2} Type.Singleton(Term.Name("_root_")[3]{2})
    """.trim.stripMargin)
  }
}

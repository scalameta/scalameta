package scala.meta.tests
package scalameta
package prettyprinters

// TODO: Uncommenting this test results in a stackoverflow in typer.
// We need to investigate and fix this unfortunate problem.
//
// import org.scalatest._
// import scala.meta._
// import scala.meta.internal.semantic._
// import scala.meta.internal.ui.Attributes
// import scala.meta.internal.semantic.Typing.Recursive
//
// class SemanticSuite extends ParseSuite {
//   test("comprehensive show[Attributes]") {
//     val symbolroot = Symbol.RootPackage
//     val prefixroot = Prefix.Zero
//     val denotroot = Denotation.Single(prefixroot, symbolroot)
//     val termroot = Term.Name("_root_").withAttrs(denotroot, Recursive).setTypechecked
//     val typeroot = Type.Singleton(termroot).setTypechecked
//
//     val symbolBar = Symbol.Global(symbolroot, "Bar", Signature.Type)
//     val prefixBar = Prefix.Type(typeroot)
//     val denotBar = Denotation.Single(prefixBar, symbolBar)
//     val typeBar = Type.Name("Bar").withAttrs(denotBar).setTypechecked
//
//     val symbolbar = Symbol.Local("bar")
//     val prefixbar = Prefix.Zero
//     val denotbar = Denotation.Single(prefixbar, symbolbar)
//     val typebar = typeBar
//     val termbar = Term.Name("bar").withAttrs(denotbar, typebar).setTypechecked
//
//     val symbolx = Symbol.Local("x")
//     val prefixx = Prefix.Zero
//     val denotx = Denotation.Single(prefixx, symbolx)
//     val typex = typeBar
//     val x = Term.Name("x").withAttrs(denotx, typex)
//
//     assert(x.show[Attributes] === """
//       |Term.Name("x")[1]{1}<1>*
//       |[1] {0}::local#x
//       |[2] {0}::local#bar
//       |[3] {2}::_root_#Bar
//       |[4] {0}::_root_
//       |{1} Type.Name("Bar")[3]
//       |{2} Type.Singleton(Term.Name("_root_")[4]{3})
//     """.trim.stripMargin)
//   }
// }

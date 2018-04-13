package scala.meta.tests.tokenizers

import scala.meta._

class TokenizerCoverageSuite() extends BaseTokenizerCoverageSuite {

  // Lit
  // not required

  // Enumerator

  checkEnumerator[Enumerator.Generator]("→`a`← <- →b←")
  checkEnumerator[Enumerator.Val]("→a← = →1←")
  checkEnumerator[Enumerator.Guard]("if →x←")

  // Case
  checkCase0("case →`a`← =>")
  checkCase0("case →`a` :: `b` :: _← =>")
  checkCase0("case →a b `c`← =>")
  checkCase0("case →_ op (a | b)← =>")
  checkCase0("case →x `.y` ()← =>")
  checkCase0("case →a← if →p← =>")
  checkCase0("case →_← => →()←")
  checkCase0("case →_← => →{}←")

  // Type
  checkType[Type.Name]("B")
  checkType[Type.Select]("→a←.→B←")
  checkType[Type.Project]("→a←#→B←")
  checkType[Type.Singleton]("→this←.type")
  checkType[Type.Singleton]("→t←.type")
  checkType[Type.Apply]("→F←[→T←]")
  checkType[Type.ApplyInfix]("→K← →Map← →V←")
  checkType[Type.Function]("() => →B←")
  checkType[Type.Function]("→A← => →B←")
  checkType[Type.Function]("(→A←, →B←) => →C←")
  checkType[Type.ImplicitFunction]("implicit →A← => →B←", dotty)
  checkType[Type.Tuple]("(→A←, →B←)")
  checkType[Type.With]("→A← with →B←")
  checkType[Type.And]("→A← →&← →B←")
  checkType[Type.And]("→A← & →B←", dotty)
  checkType[Type.Or]("→A← | →B←", dotty)
  checkType[Type.Refine]("→A← { →def f: B← }")
  checkType[Type.Refine]("→A←{}")
  checkType[Type.Refine]("{ →def f: B← }")
  checkType[Type.Existential]("→A← forSome { →type T← }")
  checkType[Type.Annotate]("→T← →@A←")
  checkType[Type.Lambda]("[→X←] => →(X, X)←", dotty)
  checkType[Type.Placeholder]("_")
  checkType[Type.Bounds]("_ →>: A <: B←")
  checkType[Type.Bounds]("_ →<: B←")
  checkType[Type.Bounds]("_ →>: A←")
  checkType[Type.ByName]("=> →T←")
  checkType[Type.Repeated]("→Any←*")
  checkType[Type.Param, Decl.Def]("def f[→A← <% →B[A]←]: C")
  checkType[Type.Param, Decl.Def]("def f[→A←: →B←]: C")
  checkType[Type.Param, Decl.Def]("def f[→A← : →B← : →C←]: D")

  // Term
  check[Term.Annotate]("→(a)←: →@A←")
  check[Term.Apply]("→(f)←(→(((a)))←)")
  check[Term.Apply]("→(f)←(→(a)←)")
  check[Term.Apply]("→(f)←(→{ case a => a }←)")
  check[Term.Apply]("→(f)←(→{ x }←)")
  check[Term.ApplyInfix]("→(a)← →op← →(b)←")
  check[Term.ApplyInfix]("→(a, b)← →op← (→c←, →d←)")
  check[Term.ApplyInfix]("→(a, b)← →op← →((c, d))←")
  check[Term.ApplyInfix]("→1← →+← →1←")
  check[Term.ApplyInfix]("→a← →f← ()")
  check[Term.ApplyInfix]("→a← →f← →(b)←")
  check[Term.ApplyType]("→(f)← [→A←,→B←]")
  check[Term.ApplyType]("→(f)← [→A←]")
  check[Term.ApplyUnary]("→-← →(a)←")
  check[Term.Ascribe]("→(a)←: →(A)←")
  check[Term.Assign]("→(a)← = →(b)←")
  check[Term.Block]("{ →(a)←; →(b)← }")
  check[Term.Do]("do →{d}← while (→p←)")
  check[Term.Eta]("→(f)← _")
  check[Term.For]("for { →x <- xs← } →(f)←")
  check[Term.ForYield]("for { →x <- xs← } yield →(f)←")
  check[Term.Function]("(→(a)←, →(b)←) => →(a)←")
  check[Term.If]("if (→p←) →(t)← else →(f)←")
  check[Term.If]("if (→p←) →(t)←")
  check[Term.If]("if (→p←) →if (p2) t←")
  check[Term.If]("if (→p←) →{}←")
  check[Term.Interpolate](""" →s←"→start ←${→(a)←}→ end←" """)
  check[Term.Match]("→(a)← match { →case x => x← }")
  check[Term.New]("new →(A)←")
  check[Term.NewAnonymous]("new →(A){}←")
  check[Term.Param, Decl.Def]("def f(→a←: →A← = →(da)←): A")
  check[Term.PartialFunction]("{ →case x => x;← →case y => y← }")
  check[Term.Repeated, Term.Apply]("f(→(x)←: _*)")
  check[Term.Return]("return →(a)←")
  check[Term.Select]("→(a)←.→b←")
  check[Term.Super]("→a.super[B]←.→c←")
  check[Term.This]("→a←.this")
  check[Term.Throw]("throw →(e)←")
  check[Term.Try]("try (→f←) catch { →case x => x;← →case y => y← } finally →{ }←")
  check[Term.TryWithHandler]("try (→f←) catch →(h)← finally →{ }←")
  check[Term.Tuple]("(→(a)←, →(b)←)")
  check[Term.While]("while (→p←) →{d}←")
  check[Term.Xml]("→<a>b←{→c←}→d</a>←")
  checkNone[Term.Name]("(x)")
  checkNone[Term.Placeholder]("(_)")

  // Pkg
  checkSource[Pkg]("package →a←")
  checkSource[Pkg]("package →a.b←")

  // Import
  check[Import]("import →a.b←")
  check[Import]("import →a.b←, →c.d←")
  check[Importer, Import]("import →a←.→_←")          // Wildcard
  check[Importer, Import]("import →a←.{ →b←, →c← }") // Name
  check[Importer, Import]("import →a←.{ →b => c← }") // Rename
  check[Importer, Import]("import →a←.{ →b => _← }") // Unimport

  // Decl
  check[Decl.Val]("val →a←: →Int←")
  check[Decl.Var]("var →b←: →Long←")
  check[Decl.Def]("def →f←: →String←")
  check[Decl.Type]("type →T←")

  // Defn
  check[Defn.Val]("val →a← = →1←")
  check[Defn.Var]("var →a← = →1←")
  check[Defn.Var]("var →a←: →A← = _")
  check[Defn.Def]("def →a← = →1←")
  check[Defn.Macro]("def →f← = macro →m←")
  check[Defn.Type]("type →T← = →Int←")
  check[Defn.Class]("class →A←")
  check[Defn.Class]("class →A←→(b: B)←")
  check[Defn.Class]("class →A← →private (b: B)←")
  check[Defn.Trait]("trait →A←")
  check[Defn.Object]("object →A←")
  check[Ctor.Secondary, Defn.Class]("class A { def →this←(→a: A←) = →this()← }")

  // Mod
  checkSelf[Mod.Annot, Defn.Def]("→@tailrec← def f = 1")
  check[Mod.Annot, Defn.Def]("@→tailrec← def f = 1")
  check[Mod.Annot, Defn.Def]("@→a← def b = 1")
  check[Mod.Annot, Defn.Def]("@→a(1)← def b = 1")
  check[Mod.Annot, Defn.Def]("@→(a @b)← def x = 1")
  check[Mod.Annot, Defn.Def]("@→(a @b(1, 2)(3))← def x = 1")
  checkSelf[Mod.Private, Defn.Val]("→private[foo]← val a = 1")
  check[Mod.Private, Defn.Val]("private[→foo←] val a = 1")
  checkSelf[Mod.Protected, Defn.Val]("→protected[foo]← val a = 1")
  check[Mod.Protected, Defn.Val]("protected[→foo←] val a = 1")
  checkSelf[Mod.Implicit, Defn.Val]("→implicit← val a = 1")
  checkSelf[Mod.Final, Defn.Val]("→final← val a = 1")
  checkSelf[Mod.Sealed, Defn.Trait]("→sealed← trait a")
  checkSelf[Mod.Override, Defn.Def]("→override← def f = 1")
  checkSelf[Mod.Case, Defn.Object]("→case← object B")
  checkSelf[Mod.Abstract, Defn.Class]("→abstract← class A")
  checkSelf[Mod.Covariant, Defn.Class]("class A[→+← T]")
  checkSelf[Mod.Contravariant, Defn.Class]("class A[→-← T]")
  checkSelf[Mod.Lazy, Defn.Val]("→lazy← val a = 1")
  checkSelf[Mod.ValParam, Defn.Class]("class A(→val← b: B)")
  checkSelf[Mod.VarParam, Defn.Class]("class A(→var← b: B)")
  checkSelf[Mod.Inline, Defn.Def]("→inline← def f = 1", dotty)

  // Misc
  check[Self, Defn.Trait]("trait A { →self←: →B← => }")
  check[Self, Defn.Trait]("trait A { →_←: →B← => }")
  check[Self, Defn.Trait]("trait A { →self← => }")
  check[Self, Defn.Trait]("trait A { →this←: →B← => }")

  check[Template, Term.NewAnonymous]("new →A← {}")
  check[Template, Term.NewAnonymous]("new { →val a = 1← } with →A← {}")
  check[Template, Defn.Class]("class A extends →B← with →C← with →D←")
  check[Template, Defn.Class]("class Y extends { →val a = 1← } with →X←")

  // Pat
  checkCase[Lit]("case →`1`← =>")
  checkPat[Pat.Bind]("→a← @ →A←")
  checkPat[Pat.Wildcard]("_")
  checkPat[Pat.SeqWildcard]("_*")
  checkPat[Pat.Alternative]("→a← | →b←")
  checkPat[Pat.Tuple]("(→a←, →b←)")
  checkPat[Pat.Extract]("→E←(→a←, →b←)")
  checkPat[Pat.ExtractInfix]("→a← →E← →b←")
  checkPat[Pat.Interpolate](""" →s←"→start ←${→(a)←}→ end←" """)
  checkPat[Pat.Xml]("→<h1>a←{→b←}→c←{→d←}→e←{→f←}→g</h1>←")
  checkPat[Pat.Typed]("→x←: →T←")
  checkPat[Pat.Typed]("→y←: →T←")
}
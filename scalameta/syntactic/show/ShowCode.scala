package scala.meta.syntactic
package show

import org.scalameta.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n, meta => m, adorn => a, function => fn }
import scala.meta.syntactic.parsers.SyntacticInfo._
import scala.meta.syntactic.parsers.Chars._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.syntactic.show.internal._
import scala.meta.syntactic.ast._
import scala.{meta => api}
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.compat.Platform.EOL

// TODO: fix occasional incorrectness when semicolons are omitted
// TODO: soft wrapping
// TODO: one mega instance for tree isn't nice, maybe separate instances for leafs and inferred instances for branches

trait Code[T] extends Show[T]
object Code {
  def apply[T](f: T => Show.Result): Code[T] = new Code[T] { def apply(input: T) = f(input) }

  // NOTE: these groups closely follow non-terminals in the grammar spec from SLS, except for:
  // 1) we don't care about tracking non-terminals (with m() and/or p()) when that doesn't affect parenthesization
  // 2) `InfixType ::= CompoundType {id [nl] CompoundType}` is incorrect. Should be `CompoundType | InfixType {id [nl] InfixType}`
  // 3) `Pattern2 ::= varid ['@' Pattern3]` has become `Pattern2 ::= varid ['@' AnyPattern3]` due to implementational reasons
  // 4) `Type ::= ... | InfixType [ExistentialClause]` has become `Type ::= ... | AnyInfixType [ExistentialClause]` due to implementational reasons
  // 5) `FunctionArgTypes ::= InfixType | ...` has become `Type ::= AnyInfixType | ...` due to implementational reasons
  @root trait SyntacticGroup {
    def categories: List[String]
    def precedence: Double
  }
  object SyntacticGroup {
    @branch trait Type extends SyntacticGroup { def categories = List("Type") }
    object Type {
      @leaf object ParamTyp extends Type { def precedence = 0 }
      @leaf object Typ extends Type { def precedence = 1 }
      @leaf object AnyInfixTyp extends Type { def precedence = 1.5 }
      @leaf class InfixTyp(op: String) extends Type { def precedence = 2 }
      @leaf object CompoundTyp extends Type { def precedence = 3 }
      @leaf object AnnotTyp extends Type { def precedence = 4 }
      @leaf object SimpleTyp extends Type { def precedence = 6 }
    }
    @branch trait Term extends SyntacticGroup { def categories = List("Term") }
    object Term {
      @leaf object Expr extends Term { def precedence = 0 }
      @leaf object Expr1 extends Term { def precedence = 1 }
      @leaf object PostfixExpr extends Term { def precedence = 2 }
      @leaf class InfixExpr(op: String) extends Term { def precedence = 3 }
      @leaf object PrefixExpr extends Term { def precedence = 4 }
      @leaf object SimpleExpr extends Term { def precedence = 5 }
      @leaf object SimpleExpr1 extends Term { def precedence = 6 }
    }
    @branch trait Pat extends SyntacticGroup { def categories = List("Pat") }
    object Pat {
      @leaf object Pattern extends Pat { def precedence = 0 }
      @leaf object Pattern1 extends Pat { def precedence = 1 }
      @leaf object Pattern2 extends Pat { def precedence = 2 }
      @leaf object AnyPattern3 extends Pat { def precedence = 2.5 }
      @leaf class Pattern3(op: String) extends Pat { def precedence = 3 }
      @leaf object SimplePattern extends Pat { def precedence = 6 }
    }
    @leaf object Literal extends Term with Pat { override def categories = List("Term", "Pat"); def precedence = 6 }
    require(Literal.precedence == Term.SimpleExpr1.precedence && Literal.precedence == Pat.SimplePattern.precedence)
    @leaf object Path extends Type with Term with Pat { override def categories = List("Type", "Term", "Pat"); def precedence = 6 }
    require(Path.precedence == Type.SimpleTyp.precedence && Path.precedence == Term.SimpleExpr1.precedence && Path.precedence == Pat.SimplePattern.precedence)
  }
  import SyntacticGroup.Type._, SyntacticGroup.Term._, SyntacticGroup.Pat._, SyntacticGroup.Literal, SyntacticGroup.Path

  def p(og: SyntacticGroup, t: Tree, left: Boolean = false, right: Boolean = false) = {
    def opNeedsParens(oo: String, io: String, customAssoc: Boolean, customPrecedence: Boolean): Boolean = {
      implicit class MySyntacticInfo(name: String) {
        def isleftassoc: Boolean = if (customAssoc) name.last != ':' else true
        def isrightassoc: Boolean = !isleftassoc
        def precedence: Int = if (customPrecedence) Term.Name(name).precedence else 0
      }
      require(left != right)
      val (ol, il) = (oo.isleftassoc, io.isleftassoc)
      if (ol ^ il) true
      else {
        val (l, r) = (ol, !ol)
        val (op, ip) = (oo.precedence, io.precedence)
        if (op < ip) r
        else if (op > ip) l
        else l ^ left
      }
    }
    def groupNeedsParens(og: SyntacticGroup, ig: SyntacticGroup): Boolean = {
      val result = {
        require(og.categories.intersect(ig.categories).nonEmpty)
        (og, ig) match {
          case (InfixExpr(oo), InfixExpr(io)) => opNeedsParens(oo, io, customAssoc = true, customPrecedence = true)
          case (InfixTyp(oo), InfixTyp(io)) => opNeedsParens(oo, io, customAssoc = true, customPrecedence = false)
          case (Pattern3(oo), Pattern3(io)) => opNeedsParens(oo, io, customAssoc = true, customPrecedence = true)
          case _ => og.precedence > ig.precedence
        }
      }
      // println((og, ig, left, right) + " => " + result)
      result
    }
    s(t) match {
      case Show.Meta(ig: SyntacticGroup, res) if groupNeedsParens(og, ig) => s("(", res, ")")
      case res => res
    }
  }

  def kw(keyword: String) = fn(sb => {
    val prelast = if (sb.length > 1) sb.charAt(sb.length - 2) else ' '
    val last = if (sb.length > 0) sb.charAt(sb.length - 1) else ' '
    val next = if (keyword.length > 0) keyword(0) else ' '
    val danger = {
      val opThenOp = isOperatorPart(last) && isOperatorPart(next)
      val underscoreThenOp = isIdentifierPart(prelast) && last == '_' && isOperatorPart(next)
      opThenOp || underscoreThenOp
    }
    if (danger) s(" " +keyword) else s(keyword)
  })

  def templ(templ: Templ) =
    // TODO: consider XXX.isEmpty
    if (templ.early.isEmpty && templ.parents.isEmpty && templ.self.name.isEmpty && templ.self.decltpe.isEmpty && templ.stats.isEmpty) s()
    else if (templ.parents.nonEmpty || templ.early.nonEmpty) s(" extends ", templ)
    else s(" ", templ)

  // Branches
  // TODO: this match is not exhaustive: if I remove Mod.Package, then I get no warning
  implicit def codeTree[T <: api.Tree]: Code[T] = Code { x => (x: api.Tree) match {
    case t: Name => m(Path, if (t.isBackquoted) s("`", t.value, "`") else s(t.value))

    // Type.Arg
    case t: Type.Arg.Repeated => m(ParamTyp, s(p(Typ, t.tpe), kw("*")))
    case t: Type.Arg.ByName   => m(ParamTyp, s(kw("=>"), " ", p(Typ, t.tpe)))

    // Type
    case t: Type.Project     => m(SimpleTyp, s(t.qual, kw("#"), t.selector))
    case t: Type.Select      => m(SimpleTyp, s(t.qual, kw("."), t.selector))
    case t: Type.Singleton   => m(SimpleTyp, s(p(SimpleExpr1, t.ref), ".", kw("type")))
    case t: Type.Annotate    => m(AnnotTyp, s(p(SimpleTyp, t.tpe), " ", t.annots))
    case t: Type.Apply       => m(SimpleTyp, s(p(SimpleTyp, t.tpe), kw("["), r(t.args.map(arg => p(Typ, arg)), ", "), kw("]")))
    case t: Type.ApplyInfix  => m(InfixTyp(t.op.value), s(p(InfixTyp(t.op.value), t.lhs, left = true), " ", t.op, " ", p(InfixTyp(t.op.value), t.rhs, right = true)))
    case t: Type.Compound    => m(CompoundTyp, s(r(t.tpes.map(tpe => p(AnnotTyp, tpe)), " with "), a(" {", a(" ", r(t.refinement, "; "), " "), "}", t.hasRefinement)))
    case t: Type.Existential => m(Typ, s(p(AnyInfixTyp, t.tpe), " ", kw("forSome"), " { ", r(t.quants, "; "), " }"))
    case t: Type.Placeholder => m(SimpleTyp, s(kw("_"), t.bounds))
    case t: Type.Tuple       => m(SimpleTyp, s("(", r(t.elements, ", "), ")"))
    case t: Type.Function    =>
      val params = if (t.params.size == 1) s(p(AnyInfixTyp, t.params.head)) else s("(", r(t.params.map(param => p(ParamTyp, param)), ", "), ")")
      m(Typ, s(params, " ", kw("=>"), " ", p(Typ, t.res)))
    case t: Type.Bounds =>
      s(t.lo.map(lo => s(" ", kw(">:"), " ", p(Typ, lo))).getOrElse(s()), t.hi.map(hi => s(" ", kw("<:"), " ", p(Typ, hi))).getOrElse(s()))

    // Lit
    case t: Lit.Bool     => m(Literal, s(t.value.toString))
    case t: Lit.Int      => m(Literal, s(t.value.toString))
    case t: Lit.Long     => m(Literal, s(t.value.toString + "l"))
    case t: Lit.Float    => m(Literal, s(t.value.toString + "f"))
    case t: Lit.Double   => m(Literal, s(t.value.toString + "d"))
    case t: Lit.Char     => m(Literal, s(enquote(t.value.toString, SingleQuotes)))
    case t: Lit.String   => m(Literal, s(enquote(t.value.toString, if (t.value.contains(EOL)) TripleQuotes else DoubleQuotes)))
    case t: Lit.Symbol   => m(Literal, s("'", t.value.name))
    case _: Lit.Null     => m(Literal, s(kw("null")))
    case _: Lit.Unit     => m(Literal, s("()"))

    // Term.Arg
    case t: Term.Arg.Named    => s(t.name, " ", kw("="), " ", p(Expr, t.rhs))
    case t: Term.Arg.Repeated => s(p(PostfixExpr, t.arg), kw(":"), " ", kw("_*"))

    // Term
    case t: Term.This     => m(SimpleExpr1, s(t.qual.map(s(_, ".")).getOrElse(s()), kw("this")))
    case t: Term.Select   => m(Path, s(p(SimpleExpr, t.qual), if (t.isPostfix) " " else ".", t.selector))
    case t: Term.Assign   => m(Expr1, s(p(SimpleExpr1, t.lhs), " ", kw("="), " ", p(Expr, t.rhs)))
    case t: Term.Update   => m(Expr1, s(p(SimpleExpr1, t.fun), t.argss, " ", kw("="), " ", p(Expr, t.rhs)))
    case t: Term.Return   => m(Expr1, s(kw("return"), if (t.hasExpr) s(" ", p(Expr, t.expr)) else s()))
    case t: Term.Throw    => m(Expr1, s(kw("throw"), " ", p(Expr, t.expr)))
    case t: Term.Ascribe  => m(Expr1, s(p(PostfixExpr, t.expr), kw(":"), " ", t.tpe))
    case t: Term.Annotate => m(Expr1, s(p(PostfixExpr, t.expr), kw(":"), " ", t.annots))
    case t: Term.Tuple    => m(SimpleExpr1, s("(", r(t.elements, ", "), ")"))
    case t: Term.Block    =>
      import Term.{Block, Function}
      def pstats(s: Seq[Stat]) = r(s.map(i(_)), "")
      t match {
        case Block(Function(Term.Param(mods, Some(name), tptopt, _) :: Nil, Block(stats)) :: Nil) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          m(SimpleExpr, s("{ ", kw("implicit"), " ", name, tptopt.map(s(kw(":"), " ", _)).getOrElse(s()), " ", kw("=>"), " ", pstats(stats), n("}")))
        case Block(Function(Term.Param(mods, Some(name), None, _) :: Nil, Block(stats)) :: Nil) =>
          m(SimpleExpr, s("{ ", name, " ", kw("=>"), " ", pstats(stats), n("}")))
        case Block(Function(Term.Param(_, None, _, _) :: Nil, Block(stats)) :: Nil) =>
          m(SimpleExpr, s("{ ", kw("_"), " ", kw("=>"), " ", pstats(stats), n("}")))
        case Block(Function(params, Block(stats)) :: Nil) =>
          m(SimpleExpr, s("{ (", r(params, ", "), ") => ", pstats(stats), n("}")))
        case _ =>
          m(SimpleExpr, if (t.stats.isEmpty) s("{}") else s("{", pstats(t.stats), n("}")))
      }
    case t: Term.PartialFunction => m(SimpleExpr, s("{", r(t.cases.map(i(_)), ""), n("}")))
    case t: Term.While           => m(Expr1, s(kw("while"), " (", t.expr, ") ", p(Expr, t.body)))
    case t: Term.Do              => m(Expr1, s(kw("do"), " ", p(Expr, t.body), " ", kw("while"), " (", t.expr, ")"))
    case t: Term.For             => m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", t.body))
    case t: Term.ForYield        => m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", kw("yield"), " ", t.body))
    case t: Term.New             => m(SimpleExpr, s(kw("new"), " ", t.templ))
    case _: Term.Placeholder     => m(SimpleExpr1, kw("_"))
    case t: Term.Eta             => m(SimpleExpr, s(p(SimpleExpr1, t.term), " ", kw("_")))
    case t: Term.Match           => m(Expr1, s(p(PostfixExpr, t.scrut), " ", kw("match"), " {", r(t.cases.map(i(_)), ""), n("}")))
    case t: Term.Apply           => m(SimpleExpr1, s(p(SimpleExpr1, t.fun), t.args))
    case t: Term.ApplyType       => m(SimpleExpr1, s(p(SimpleExpr, t.fun), t.targs))
    case t: Term.ApplyUnary      => m(PrefixExpr, s(t.op, p(SimpleExpr, t.arg)))
    case t: Term.ApplyInfix      =>
      m(InfixExpr(t.op.value)    , s(p(InfixExpr(t.op.value), t.lhs, left = true), " ", t.op, t.targs, " ", t.args match {
        case (arg: Term) :: Nil => s(p(InfixExpr(t.op.value), arg, right = true))
        case args               => s(args)
      }))
    case t: Term.TryWithCases    =>
      m(Expr1, s(kw("try"), " ", p(Expr, t.expr),
        if (t.catchp.nonEmpty) s(" ", kw("catch"), " {", r(t.catchp.map(i(_)), ""), n("}")) else s(""),
        t.finallyp.map { finallyp => s(" ", kw("finally"), " ", finallyp) }.getOrElse(s())))
    case t: Term.TryWithTerm     =>
      m(Expr1, s(kw("try"), " ", p(Expr, t.expr), " ", kw("catch"), " ", t.catchp,
        t.finallyp.map { finallyp => s(" ", kw("finally"), " ", finallyp) }.getOrElse(s())))
    case t: Term.If       => m(Expr1, s(kw("if"), " (", t.cond, ") ", p(Expr, t.thenp), if (t.hasElsep) s(" ", kw("else"), " ", p(Expr, t.elsep)) else s()))
    case t: Term.Function =>
      t match {
        case Term.Function(Term.Param(mods, Some(name), tptopt, _) :: Nil, body) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          m(Expr, s(kw("implicit"), " ", name, tptopt.map(s(kw(":"), " ", _)).getOrElse(s()), " ", kw("=>"), " ", p(Expr, body)))
        case Term.Function(Term.Param(mods, Some(name), None, _) :: Nil, body) =>
          m(Expr, s(name, " ", kw("=>"), " ", p(Expr, body)))
        case Term.Function(Term.Param(_, None, _, _) :: Nil, body) =>
          m(Expr, s(kw("_"), " ", kw("=>"), " ", p(Expr, body)))
        case Term.Function(params, body) =>
          m(Expr, s("(", r(params, ", "), ") ", kw("=>"), " ", p(Expr, body)))
      }
    case t: Term.Interpolate =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => s(part.value, "$", id.value)
        case (part, arg)                          => s(part.value, "${", p(Expr, arg), "}")
      }
      val quote = if (t.parts.map(_.value).exists(s => s.contains(EOL) || s.contains("\""))) "\"\"\"" else "\""
      m(SimpleExpr1, s(t.prefix, quote, r(zipped), t.parts.last.value, quote))

    // Pat
    case t: Pat.Alternative      => m(Pattern, s(p(Pattern, t.lhs), " ", kw("|"), " ", p(Pattern, t.rhs)))
    case t: Pat.Bind             => m(Pattern2, s(p(SimplePattern, t.lhs), " ", kw("@"), " ", p(AnyPattern3, t.rhs)))
    case t: Pat.Tuple            => m(SimplePattern, s("(", r(t.elements, ", "), ")"))
    case _: Pat.Arg.SeqWildcard  => m(SimplePattern, kw("_*"))
    case t: Pat.Typed            => m(Pattern1, s(p(SimplePattern, t.lhs), kw(":"), " ", p(Typ, t.rhs)))
    case _: Pat.Wildcard         => m(SimplePattern, kw("_"))
    case t: Pat.Extract          => m(SimplePattern, s(t.ref, t.targs, t.elements))
    case t: Pat.ExtractInfix     =>
      m(Pattern3(t.ref.value), s(p(Pattern3(t.ref.value), t.lhs, left = true), " ", t.ref, " ", t.rhs match {
        case pat :: Nil => s(p(Pattern3(t.ref.value), pat, right = true))
        case pats       => s(pats)
      }))
    case t: Pat.Interpolate =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => s(part, "$", id.value)
        case (part, arg)                          => s(part, "${", arg, "}")
      }
      m(SimplePattern, s(t.prefix, "\"", r(zipped), t.parts.last, "\""))

    // Mod
    case t: Mod.Annot           => s(kw("@"), p(SimpleTyp, t.ref.tpe), t.ref.argss)
    case _: Mod.Abstract        => kw("abstract")
    case _: Mod.Case            => kw("case")
    case _: Mod.Covariant       => kw("+")
    case _: Mod.Contravariant   => kw("-")
    case _: Mod.Final           => kw("final")
    case _: Mod.Implicit        => kw("implicit")
    case _: Mod.Lazy            => kw("lazy")
    case _: Mod.Override        => kw("override")
    case _: Mod.Sealed          => kw("sealed")
    case t: Mod.Private         => s(kw("private"))
    case t: Mod.PrivateThis     => s(kw("private"), kw("["), kw("this"), kw("]"))
    case t: Mod.PrivateWithin   => s(kw("private"), kw("["), t.name.toString, kw("]"))
    case t: Mod.Protected       => s(kw("protected"))
    case t: Mod.ProtectedThis   => s(kw("protected"), kw("["), kw("this"), kw("]"))
    case t: Mod.ProtectedWithin => s(kw("protected"), kw("["), t.name.toString, kw("]"))

    // Defn
    case t: Defn.Val       => s(a(t.mods, " "), kw("val"), " ", r(t.pats, ", "), t.decltpe, " ", kw("="), " ", t.rhs)
    case t: Defn.Var       => s(a(t.mods, " "), kw("var"), " ", r(t.pats, ", "), t.decltpe, " ", kw("="), " ", t.rhs.map(s(_)).getOrElse(s(kw("_"))))
    case t: Defn.Type      => s(a(t.mods, " "), kw("type"), " ", t.name, t.tparams, " ", kw("="), " ", t.body)
    case t: Defn.Class     => s(a(t.mods, " "), kw("class"), " ", t.name, t.tparams, a(" ", t.ctor, t.ctor.mods.nonEmpty), templ(t.templ))
    case t: Defn.Trait     => s(a(t.mods, " "), kw("trait"), " ", t.name, t.tparams, templ(t.templ))
    case t: Defn.Object    => s(a(t.mods, " "), kw("object"), " ", t.name, templ(t.templ))
    case t: Defn.Def       =>
      s(a(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss, t.decltpe, " = ", t.body)
    case t: Defn.Procedure =>
      s(a(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss, " { ", r(t.stats.map(i(_)), ""), n("}"))
    case t: Defn.Macro     =>
      s(a(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss, kw(":"), " ", t.tpe, " ", kw("="), " ", kw("macro"), " ", t.body)

    // Decl
    case t: Decl.Val       => s(a(t.mods, " "), kw("val"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
    case t: Decl.Var       => s(a(t.mods, " "), kw("var"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
    case t: Decl.Type      => s(a(t.mods, " "), kw("type"), " ", t.name, t.tparams, t.bounds)
    case t: Decl.Def       => s(a(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss, kw(":"), " ", t.decltpe)
    case t: Decl.Procedure => s(a(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss)

    // Pkg
    case t: Source             => r(t.stats, EOL)
    case t: Pkg if t.hasBraces => s(kw("package"), " ", t.ref, " {", r(t.stats.map(i(_)), ""), n("}"))
    case t: Pkg                => s(kw("package"), " ", t.ref, r(t.stats.map(n(_))))
    case t: Pkg.Object         => s(kw("package"), " ", a(t.mods, " "), kw("object"), " ", t.name, templ(t.templ))

    // Ctor
    case t: Ctor.Primary   => s(a(t.mods, " ", t.mods.nonEmpty && t.paramss.nonEmpty), t.paramss)
    case t: Ctor.Secondary =>
      s(a(t.mods, " "), kw("def"), " ", kw("this"), t.paramss, t.stats match {
        case Nil   => s(" ", kw("="), " ", kw("this"), t.primaryCtorArgss)
        case stats => s(" { ", kw("this"), t.primaryCtorArgss, ";", a(" ", r(stats, "; ")), " }")
      })

    // Enum
    case t: Enum.Val       => s(p(Pattern1, t.pat), " = ", p(Expr, t.rhs))
    case t: Enum.Generator => s(p(Pattern1, t.pat), " <- ", p(Expr, t.rhs))
    case t: Enum.Guard     => s(kw("if"), " ", p(PostfixExpr, t.cond))

    // Import
    case t: Import.Name     => s(t.value)
    case t: Import.Rename   => s(t.from, " ", kw("=>"), " ", t.to)
    case t: Import.Unimport => s(t.name, " ", kw("=>"), " ", kw("_"))
    case _: Import.Wildcard => kw("_")
    case t: Import.Clause   => s(t.ref, ".", t.sels)
    case t: Import          => s(kw("import"), " ", r(t.clauses, ", "))

    // Aux
    case t: Ctor.Ref => s(p(AnnotTyp, t.tpe), t.argss)
    case t: Templ =>
      val isBodyEmpty = t.self.name.isEmpty && t.self.decltpe.isEmpty && !t.hasStats
      val isTemplateEmpty = t.early.isEmpty && t.parents.isEmpty && isBodyEmpty
      if (isTemplateEmpty) s()
      else {
        val isOneLiner = t.stats.length == 0 || (t.stats.length == 1 && !s(t.stats.head).toString.contains(EOL))
        val pearly = if (!t.early.isEmpty) s("{ ", r(t.early, "; "), " } with ") else s()
        val pparents = a(r(t.parents, " with "), " ", !t.parents.isEmpty && !isBodyEmpty)
        val pbody = (t.self.name.nonEmpty || t.self.decltpe.nonEmpty, t.hasStats, t.stats) match {
          case (false, false, _) => s()
          case (true, false, _) => s("{ ", t.self, " => }")
          case (false, true, List()) if isOneLiner => s("{}")
          case (false, true, List(stat)) if isOneLiner => s("{ ", stat, " }")
          case (false, true, stats) => s("{", r(stats.map(i(_)), ""), n("}"))
          case (true, true, List()) if isOneLiner => s("{ ", t.self, " => }")
          case (true, true, List(stat)) if isOneLiner => s("{ ", t.self, " => ", stat, " }")
          case (true, true, stats) => s("{ ", t.self, " =>", r(stats.map(i(_)), ""), n("}"))
        }
        s(pearly, pparents, pbody)
      }
    case t: Case  =>
      s("case ", p(Pattern, t.pat), t.cond.map { cond => s(" ", kw("if"), " ", p(PostfixExpr, cond)) }.getOrElse(s()), " ", kw("=>"), r(t.stats.map(i(_)), ""))
    case t: Templ.Param =>
      val keyword = t match { case t: Term.Param => ""; case t: Templ.Param.Val => "val"; case t: Templ.Param.Var => "var"; }
      val mods = t.mods.filter(!_.isInstanceOf[Mod.Implicit]) // NOTE: `implicit` in parameters is skipped in favor of `implicit` in the enclosing parameter list
      s(a(mods, " "), a(kw(keyword), " ", keyword.nonEmpty), t.name.map(_.value).getOrElse("_"), t.decltpe, t.default.map(s(" ", kw("="), " ", _)).getOrElse(s()))
    case t: Type.Param =>
      val cbounds = r(t.contextBounds.map { s(kw(":"), " ", _) })
      val vbounds = r(t.viewBounds.map { s(" ", kw("<%"), " ", _) })
      val tbounds = s(t.typeBounds)
      val variance = t.mods.foldLeft("")((curr, m) => if (m.isInstanceOf[Mod.Covariant]) "+" else if (m.isInstanceOf[Mod.Contravariant]) "-" else curr)
      val mods = t.mods.filter(m => !m.isInstanceOf[Mod.Covariant] && !m.isInstanceOf[Mod.Contravariant])
      require(t.mods.length - mods.length <= 1)
      s(a(mods, " "), variance, t.name.map(_.value).getOrElse("_"), t.tparams, cbounds, vbounds, tbounds)
    case t: Term.Super =>
      s(t.thisp.map { thisp => s(thisp, ".") }.getOrElse(s()),
        kw("super"), t.superp.map { st => s("[", st, "]") }.getOrElse(s()))
  } }

  // Multiples and optionals
  private implicit val codeArgs: Code[Seq[Term.Arg]] = Code {
    case (b: Term.Block) :: Nil => s(" ", b)
    case args                   => s("(", r(args, ", "), ")")
  }
  private implicit val codeArgss: Code[Seq[Seq[Term.Arg]]] = Code { r(_) }
  private implicit val codeTargs: Code[Seq[Type]] = Code { targs =>
    if (targs.isEmpty) s()
    else s("[", r(targs, ", "), "]")
  }
  private implicit val codePats: Code[Seq[Pat]] = Code { pats => s("(", r(pats, ", "), ")") }
  private implicit val codePatArgs: Code[Seq[Pat.Arg]] = Code { pats => s("(", r(pats, ", "), ")") }
  private implicit val codeMods: Code[Seq[Mod]] = Code { mods => if (mods.nonEmpty) r(mods, " ") else s() }
  private implicit val codeAnnots: Code[Seq[Mod.Annot]] = Code { annots => if (annots.nonEmpty) r(annots, " ") else s() }
  private implicit def codeParams[P <: Templ.Param]: Code[Seq[P]] = Code { params => s("(", r(params, ", "), ")") }
  private implicit def codeParamss[P <: Templ.Param]: Code[Seq[Seq[P]]] = Code { paramss => r(paramss.map(params =>
    s("(", a("implicit ", r(params, ", "), params.exists(_.mods.exists(_.isInstanceOf[Mod.Implicit]))), ")")
  ), "")}
  private implicit val codeTparams: Code[Seq[Type.Param]] = Code { tparams =>
    if (tparams.nonEmpty) s("[", r(tparams, ", "), "]") else s()
  }
  private implicit val codeTypeArgOpt: Code[Option[Type.Arg]] = Code { _.map { t => s(kw(":"), " ", t) }.getOrElse(s()) }
  private implicit val codeTypeOpt: Code[Option[Type]] = Code { _.map { t => s(kw(":"), " ", t) }.getOrElse(s()) }
  private implicit val codeTermNameOpt: Code[Option[Term.Name]] = Code { _.map(s(_)).getOrElse(s(")")) }
  private implicit val codeImportSels: Code[Seq[Selector]] = Code {
    case (t: Import.Name) :: Nil     => s(t)
    case (t: Import.Wildcard) :: Nil => s(t)
    case sels                        => s("{ ", r(sels, ", "), " }")
  }
}

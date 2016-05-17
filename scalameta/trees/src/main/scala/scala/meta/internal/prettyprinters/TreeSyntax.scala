package scala.meta
package internal
package prettyprinters

import scala.meta.classifiers._
import scala.meta.dialects.Quasiquote
import scala.meta.prettyprinters._
import scala.meta.prettyprinters.Syntax._
import Show.{ sequence => s, repeat => r, indent => i, newline => n, meta => m, wrap => w, function => fn }
import scala.meta.internal.ast.Origin
import scala.meta.internal.ast.Quasi
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.tokenizers.Chars._
import scala.meta.internal.tokenizers.keywords
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.adt._
import org.scalameta.collections._
import org.scalameta.invariants._
import org.scalameta.{unreachable, debug}
import scala.compat.Platform.EOL

object TreeSyntax {
  def apply[T <: Tree](dialect: Dialect, options: Options): Syntax[T] = {
    object syntaxInstances {
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
          implicit class XtensionMySyntacticInfo(name: String) {
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

      def templ(templ: Template) =
        // TODO: consider XXX.isEmpty
        if (templ.early.isEmpty && templ.parents.isEmpty && templ.self.name.is[Name.Anonymous] && templ.self.decltpe.isEmpty && templ.stats.isEmpty) s()
        else if (templ.parents.nonEmpty || templ.early.nonEmpty) s(" extends ", templ)
        else s(" ", templ)

      // TODO: revisit this once we have trivia in place
      def guessIsBackquoted(t: Name): Boolean = {
        def cantBeWrittenWithoutBackquotes(t: Name): Boolean = {
          // TODO: this requires a more thorough implementation
          // TODO: the `this` check is actually here to correctly prettyprint primary ctor calls in secondary ctors
          // this is purely an implementation artifact and will be fixed once we have tokens
          t.value != "this" && (keywords.contains(t.value) || t.value.contains(" "))
        }
        def isAmbiguousWithPatVarTerm(t: Term.Name, p: Tree): Boolean = {
          // TODO: the `eq` trick is very unreliable, but I can't come up with anything better at the moment
          // since the whole guessXXX business is going to be obsoleted by tokens very soon, I'm leaving this as is
          val looksLikePatVar = t.value.head.isLower && t.value.head.isLetter
          val thisLocationAlsoAcceptsPatVars = p match {
            case p: Term.Name => unreachable
            case p: Term.Select => false
            case p: Pat.Wildcard => unreachable
            case p: Pat.Var.Term => false
            case p: Pat.Bind => unreachable
            case p: Pat.Alternative => true
            case p: Pat.Tuple => true
            case p: Pat.Extract => p.args.exists(_ eq t)
            case p: Pat.ExtractInfix => (p.lhs eq t) || p.rhs.exists(_ eq t)
            case p: Pat.Interpolate => p.args.exists(_ eq t)
            case p: Pat.Typed => unreachable
            case p: Pat => unreachable
            case p: Case => p.pat eq t
            case p: Defn.Val => p.pats.exists(_ eq t)
            case p: Defn.Var => p.pats.exists(_ eq t)
            case p: Enumerator.Generator => p.pat eq t
            case p: Enumerator.Val => p.pat eq t
            case _ => false
          }
          looksLikePatVar && thisLocationAlsoAcceptsPatVars
        }
        def isAmbiguousWithPatVarType(t: Type.Name, p: Tree): Boolean = {
          // TODO: figure this out with Martin
          // `x match { case _: t => }` produces a Type.Name
          // `x match { case _: List[t] => }` produces a Pat.Var.Type
          // `x match { case _: List[`t`] => }` produces a Pat.Var.Type as well
          // the rules look really inconsistent and probably that's just an oversight
          false
        }
        (t, t.parent) match {
          case (t: Term.Name, Some(p: Tree)) => isAmbiguousWithPatVarTerm(t, p) || cantBeWrittenWithoutBackquotes(t)
          case (t: Type.Name, Some(p: Tree)) => isAmbiguousWithPatVarType(t, p) || cantBeWrittenWithoutBackquotes(t)
          case _ => cantBeWrittenWithoutBackquotes(t)
        }
      }
      def guessHasRefinement(t: Type.Compound): Boolean = t.refinement.nonEmpty
      def guessHasRefinement(t: Pat.Type.Compound): Boolean = t.refinement.nonEmpty
      def guessIsPostfix(t: Term.Select): Boolean = false
      def guessHasExpr(t: Term.Return): Boolean = t.expr match { case Lit(()) => false; case _ => true }
      def guessHasElsep(t: Term.If): Boolean = t.elsep match { case Lit(()) => false; case _ => true }
      def guessHasStats(t: Template): Boolean = t.stats.nonEmpty
      def guessHasBraces(t: Pkg): Boolean = {
        def isOnlyChildOfOnlyChild(t: Pkg): Boolean = t.parent match {
          case Some(pkg: Pkg) => isOnlyChildOfOnlyChild(pkg) && pkg.stats.length == 1
          case Some(source: Source) => source.stats.length == 1
          case None => true
          case _ => unreachable
        }
        !isOnlyChildOfOnlyChild(t)
      }

      // Branches
      // TODO: this match is not exhaustive: if I remove Mod.Package, then I get no warning
      implicit def syntaxTree[T <: Tree]: Syntax[T] = Syntax {
        // Bottom
        case t: Quasi =>
          if (!dialect.metalevel.isQuoted) throw new UnsupportedOperationException(s"$dialect doesn't support unquoting")
          if (t.rank > 0) {
            s("." * (t.rank + 1), w("{", t.tree, "}", !t.tree.is[Quasi]))
          } else {
            val allowBraceless = t.tree.is[Term.Name] || t.tree.is[Pat.Var.Term] || t.tree.is[Term.This] || t.tree.is[Pat.Wildcard]
            implicit val syntaxOptions = options
            implicit val syntaxDialect = dialect match { case dialect: Quasiquote => dialect.underlying; case _ => unreachable }
            s("$", w("{", t.tree.syntax, "}", !allowBraceless))
          }

        // Name
        case t: Name.Anonymous       => s("_")
        case t: Name.Indeterminate   => if (guessIsBackquoted(t)) s("`", t.value, "`") else s(t.value)

        // Term
        case t: Term if t.isCtorCall => if (t.is[Ctor.Ref.Function]) s("=>") else s(p(AnnotTyp, t.ctorTpe), t.ctorArgss)
        case t: Term.This            =>
          val qual = if (t.qual.is[Name.Anonymous]) s() else s(t.qual, ".")
          m(Path, qual, kw("this"))
        case t: Term.Super           =>
          val thisqual = if (t.thisp.is[Name.Anonymous]) s() else s(t.thisp, ".")
          val superqual = if (t.superp.is[Name.Anonymous]) s() else s("[", t.superp, "]")
          m(Path, s(thisqual, kw("super"), superqual))
        case t: Term.Name            => m(Path, if (guessIsBackquoted(t)) s("`", t.value, "`") else s(t.value))
        case t: Term.Select          => m(Path, s(p(SimpleExpr, t.qual), if (guessIsPostfix(t)) " " else ".", t.name))
        case t: Term.Interpolate     =>
          val parts = t.parts.map{ case Lit(part: String) => part }
          val zipped = parts.zip(t.args).map {
            case (part, id: Name) if !guessIsBackquoted(id) => s(part, "$", id.value)
            case (part, arg) => s(part, "${", p(Expr, arg), "}")
          }
          val quote = if (parts.exists(s => s.contains(EOL) || s.contains("\""))) "\"\"\"" else "\""
          m(SimpleExpr1, s(t.prefix, quote, r(zipped), parts.last, quote))
        case t: Term.Xml             =>
          val parts = t.parts.map{ case Lit(part: String) => part }
          val zipped = parts.zip(t.args).map{ case (part, arg) => s(part, "{", p(Expr, arg), "}") }
          m(SimpleExpr1, s(r(zipped), parts.last))
        case t: Term.Apply           => m(SimpleExpr1, s(p(SimpleExpr1, t.fun), t.args))
        case t: Term.ApplyType       => m(SimpleExpr1, s(p(SimpleExpr, t.fun), t.targs))
        case t: Term.ApplyInfix      =>
          m(InfixExpr(t.op.value), s(p(InfixExpr(t.op.value), t.lhs, left = true), " ", t.op, t.targs, " ", t.args match {
            case (arg: Term) :: Nil => s(p(InfixExpr(t.op.value), arg, right = true))
            case args               => s(args)
          }))
        case t: Term.ApplyUnary      => m(PrefixExpr, s(t.op, p(SimpleExpr, t.arg)))
        case t: Term.Assign          => m(Expr1, s(p(SimpleExpr1, t.lhs), " ", kw("="), " ", p(Expr, t.rhs)))
        case t: Term.Update          => m(Expr1, s(p(SimpleExpr1, t.fun), t.argss, " ", kw("="), " ", p(Expr, t.rhs)))
        case t: Term.Return          => m(Expr1, s(kw("return"), if (guessHasExpr(t)) s(" ", p(Expr, t.expr)) else s()))
        case t: Term.Throw           => m(Expr1, s(kw("throw"), " ", p(Expr, t.expr)))
        case t: Term.Ascribe         => m(Expr1, s(p(PostfixExpr, t.expr), kw(":"), " ", t.decltpe))
        case t: Term.Annotate        => m(Expr1, s(p(PostfixExpr, t.expr), kw(":"), " ", t.annots))
        case t: Term.Tuple           => m(SimpleExpr1, s("(", r(t.elements, ", "), ")"))
        case t: Term.Block           =>
          import Term.{Block, Function}
          def pstats(s: Seq[Stat]) = r(s.map(i(_)), "")
          t match {
            case Block(Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, Block(stats)) :: Nil) if mods.exists(_.is[Mod.Implicit]) =>
              m(SimpleExpr, s("{ ", kw("implicit"), " ", name, tptopt.map(s(kw(":"), " ", _)).getOrElse(s()), " ", kw("=>"), " ", pstats(stats), n("}")))
            case Block(Function(Term.Param(mods, name: Term.Name, None, _) :: Nil, Block(stats)) :: Nil) =>
              m(SimpleExpr, s("{ ", name, " ", kw("=>"), " ", pstats(stats), n("}")))
            case Block(Function(Term.Param(_, _: Name.Anonymous, _, _) :: Nil, Block(stats)) :: Nil) =>
              m(SimpleExpr, s("{ ", kw("_"), " ", kw("=>"), " ", pstats(stats), n("}")))
            case Block(Function(params, Block(stats)) :: Nil) =>
              m(SimpleExpr, s("{ (", r(params, ", "), ") => ", pstats(stats), n("}")))
            case _ =>
              m(SimpleExpr, if (t.stats.isEmpty) s("{}") else s("{", pstats(t.stats), n("}")))
          }
        case t: Term.If              => m(Expr1, s(kw("if"), " (", t.cond, ") ", p(Expr, t.thenp), if (guessHasElsep(t)) s(" ", kw("else"), " ", p(Expr, t.elsep)) else s()))
        case t: Term.Match           => m(Expr1, s(p(PostfixExpr, t.scrut), " ", kw("match"), " {", r(t.cases.map(i(_)), ""), n("}")))
        case t: Term.TryWithCases    =>
          m(Expr1, s(kw("try"), " ", p(Expr, t.expr),
            if (t.catchp.nonEmpty) s(" ", kw("catch"), " {", r(t.catchp.map(i(_)), ""), n("}")) else s(""),
            t.finallyp.map { finallyp => s(" ", kw("finally"), " ", finallyp) }.getOrElse(s())))
        case t: Term.TryWithTerm     =>
          m(Expr1, s(kw("try"), " ", p(Expr, t.expr), " ", kw("catch"), " ", t.catchp,
            t.finallyp.map { finallyp => s(" ", kw("finally"), " ", finallyp) }.getOrElse(s())))
        case t: Term.Function        =>
          t match {
            case Term.Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, body) if mods.exists(_.is[Mod.Implicit]) =>
              m(Expr, s(kw("implicit"), " ", name, tptopt.map(s(kw(":"), " ", _)).getOrElse(s()), " ", kw("=>"), " ", p(Expr, body)))
            case Term.Function(Term.Param(mods, name: Term.Name, None, _) :: Nil, body) =>
              m(Expr, s(name, " ", kw("=>"), " ", p(Expr, body)))
            case Term.Function(Term.Param(_, _: Name.Anonymous, _, _) :: Nil, body) =>
              m(Expr, s(kw("_"), " ", kw("=>"), " ", p(Expr, body)))
            case Term.Function(params, body) =>
              m(Expr, s("(", r(params, ", "), ") ", kw("=>"), " ", p(Expr, body)))
          }
        case t: Term.PartialFunction => m(SimpleExpr, s("{", r(t.cases.map(i(_)), ""), n("}")))
        case t: Term.While           => m(Expr1, s(kw("while"), " (", t.expr, ") ", p(Expr, t.body)))
        case t: Term.Do              => m(Expr1, s(kw("do"), " ", p(Expr, t.body), " ", kw("while"), " (", t.expr, ")"))
        case t: Term.For             => m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", t.body))
        case t: Term.ForYield        => m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", kw("yield"), " ", t.body))
        case t: Term.New             => m(SimpleExpr, s(kw("new"), " ", t.templ))
        case _: Term.Placeholder     => m(SimpleExpr1, kw("_"))
        case t: Term.Eta             => m(SimpleExpr, s(p(SimpleExpr1, t.term), " ", kw("_")))
        case t: Term.Arg.Named       => s(t.name, " ", kw("="), " ", p(Expr, t.rhs))
        case t: Term.Arg.Repeated    => s(p(PostfixExpr, t.arg), kw(":"), " ", kw("_*"))
        case t: Term.Param           =>
          val mods = t.mods.filter(!_.is[Mod.Implicit]) // NOTE: `implicit` in parameters is skipped in favor of `implicit` in the enclosing parameter list
          s(w(mods, " "), t.name, t.decltpe, t.default.map(s(" ", kw("="), " ", _)).getOrElse(s()))

        // Type
        case t: Type.Name         => m(Path, if (guessIsBackquoted(t)) s("`", t.value, "`") else s(t.value))
        case t: Type.Select       => m(SimpleTyp, s(t.qual, kw("."), t.name))
        case t: Type.Project      => m(SimpleTyp, s(t.qual, kw("#"), t.name))
        case t: Type.Singleton    => m(SimpleTyp, s(p(SimpleExpr1, t.ref), ".", kw("type")))
        case t: Type.Apply        => m(SimpleTyp, s(p(SimpleTyp, t.tpe), kw("["), r(t.args.map(arg => p(Typ, arg)), ", "), kw("]")))
        case t: Type.ApplyInfix   => m(InfixTyp(t.op.value), s(p(InfixTyp(t.op.value), t.lhs, left = true), " ", t.op, " ", p(InfixTyp(t.op.value), t.rhs, right = true)))
        case t: Type.Function     =>
          val params = if (t.params.size == 1) s(p(AnyInfixTyp, t.params.head)) else s("(", r(t.params.map(param => p(ParamTyp, param)), ", "), ")")
          m(Typ, s(params, " ", kw("=>"), " ", p(Typ, t.res)))
        case t: Type.Tuple        => m(SimpleTyp, s("(", r(t.elements, ", "), ")"))
        case t: Type.Compound     => m(CompoundTyp, s(r(t.tpes.map(tpe => p(AnnotTyp, tpe)), " with "), w(" {", w(" ", r(t.refinement, "; "), " "), "}", guessHasRefinement(t))))
        case t: Type.Existential  => m(Typ, s(p(AnyInfixTyp, t.tpe), " ", kw("forSome"), " { ", r(t.quants, "; "), " }"))
        case t: Type.Annotate     => m(AnnotTyp, s(p(SimpleTyp, t.tpe), " ", t.annots))
        case t: Type.Placeholder  => m(SimpleTyp, s(kw("_"), t.bounds))
        case t: Type.Bounds =>
          s(t.lo.map(lo => s(" ", kw(">:"), " ", p(Typ, lo))).getOrElse(s()), t.hi.map(hi => s(" ", kw("<:"), " ", p(Typ, hi))).getOrElse(s()))
        case t: Type.Arg.Repeated => m(ParamTyp, s(p(Typ, t.tpe), kw("*")))
        case t: Type.Arg.ByName   => m(ParamTyp, s(kw("=>"), " ", p(Typ, t.tpe)))
        case t: Type.Param        =>
          val mods = t.mods.filter(m => !m.is[Mod.Covariant] && !m.is[Mod.Contravariant])
          require(t.mods.length - mods.length <= 1)
          val variance = t.mods.foldLeft("")((curr, m) => if (m.is[Mod.Covariant]) "+" else if (m.is[Mod.Contravariant]) "-" else curr)
          val tbounds = s(t.tbounds)
          val vbounds = r(t.vbounds.map { s(" ", kw("<%"), " ", _) })
          val cbounds = r(t.cbounds.map { s(kw(":"), " ", _) })
          s(w(mods, " "), variance, t.name, t.tparams, tbounds, vbounds, cbounds)

        // Pat
        case t: Pat.Var.Term         => m(SimplePattern, s(t.name.value))
        case _: Pat.Wildcard         => m(SimplePattern, kw("_"))
        case t: Pat.Bind             =>
          val separator = if (t.rhs.is[Pat.Arg.SeqWildcard] && dialect.bindToSeqWildcardDesignator == ":") ""  else " "
          val designator = if (t.rhs.is[Pat.Arg.SeqWildcard]) dialect.bindToSeqWildcardDesignator else "@"
          m(Pattern2, s(p(SimplePattern, t.lhs), separator, kw(designator), " ", p(AnyPattern3, t.rhs)))
        case t: Pat.Alternative      => m(Pattern, s(p(Pattern, t.lhs), " ", kw("|"), " ", p(Pattern, t.rhs)))
        case t: Pat.Tuple            => m(SimplePattern, s("(", r(t.elements, ", "), ")"))
        case t: Pat.Extract          => m(SimplePattern, s(t.ref, t.targs, t.args))
        case t: Pat.ExtractInfix     =>
          m(Pattern3(t.ref.value), s(p(Pattern3(t.ref.value), t.lhs, left = true), " ", t.ref, " ", t.rhs match {
            case pat :: Nil => s(p(Pattern3(t.ref.value), pat, right = true))
            case pats       => s(pats)
          }))
        case t: Pat.Interpolate      =>
          val zipped = t.parts.zip(t.args).map {
            case (part, id: Name) if !guessIsBackquoted(id) => s(part, "$", id.value)
            case (part, arg)                                => s(part, "${", arg, "}")
          }
          m(SimplePattern, s(t.prefix, "\"", r(zipped), t.parts.last, "\""))
        case t: Pat.Xml              =>
          val zipped = t.parts.zip(t.args).map{ case (part, arg) => s(part, "${", arg, "}") }
          m(SimplePattern, s(r(zipped), t.parts.last))
        case t: Pat.Typed            => m(Pattern1, s(p(SimplePattern, t.lhs), kw(":"), " ", p(CompoundTyp, t.rhs)))
        case _: Pat.Arg.SeqWildcard  => m(SimplePattern, kw("_*"))

        // Pat.Type
        // TODO: fix copy/paste with Type
        case t: Pat.Type.Wildcard    => m(SimpleTyp, s("_"))
        case t: Pat.Var.Type         => m(SimpleTyp, s(t.name.value))
        case t: Pat.Type.Project     => m(SimpleTyp, s(t.qual, kw("#"), t.name))
        case t: Pat.Type.Apply       => m(SimpleTyp, s(p(SimpleTyp, t.tpe), kw("["), r(t.args.map(arg => p(Typ, arg)), ", "), kw("]")))
        case t: Pat.Type.ApplyInfix  => m(InfixTyp(t.op.value), s(p(InfixTyp(t.op.value), t.lhs, left = true), " ", t.op, " ", p(InfixTyp(t.op.value), t.rhs, right = true)))
        case t: Pat.Type.Function    =>
          val params = if (t.params.size == 1) s(p(AnyInfixTyp, t.params.head)) else s("(", r(t.params.map(param => p(ParamTyp, param)), ", "), ")")
          m(Typ, s(params, " ", kw("=>"), " ", p(Typ, t.res)))
        case t: Pat.Type.Tuple       => m(SimpleTyp, s("(", r(t.elements, ", "), ")"))
        case t: Pat.Type.Compound    => m(CompoundTyp, s(r(t.tpes.map(tpe => p(AnnotTyp, tpe)), " with "), w(" {", w(" ", r(t.refinement, "; "), " "), "}", guessHasRefinement(t))))
        case t: Pat.Type.Existential => m(Typ, s(p(AnyInfixTyp, t.tpe), " ", kw("forSome"), " { ", r(t.quants, "; "), " }"))
        case t: Pat.Type.Annotate    => m(AnnotTyp, s(p(SimpleTyp, t.tpe), " ", t.annots))

        // Lit
        case Lit(value: Boolean) => m(Literal, s(value.toString))
        case Lit(value: Byte)    => m(Literal, s("ByteLiterals.", if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value))
        case Lit(value: Short)   => m(Literal, s("ShortLiterals.", if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value))
        case Lit(value: Int)     => m(Literal, s(value.toString))
        case Lit(value: Long)    => m(Literal, s(value.toString + "L"))
        case Lit(value: Float)   => m(Literal, s(value.toString + "F"))
        case Lit(value: Double)  => m(Literal, s(value.toString))
        case Lit(value: Char)    => m(Literal, s(enquote(value.toString, SingleQuotes)))
        case Lit(value: String)  => m(Literal, s(enquote(value.toString, if (value.contains(EOL)) TripleQuotes else DoubleQuotes)))
        case Lit(value: Symbol)  => m(Literal, s("'", value.name))
        case Lit(null)           => m(Literal, s(kw("null")))
        case Lit(())             => m(Literal, s("()"))

        // Member
        case t: Decl.Val       => s(w(t.mods, " "), kw("val"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
        case t: Decl.Var       => s(w(t.mods, " "), kw("var"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
        case t: Decl.Type      => s(w(t.mods, " "), kw("type"), " ", t.name, t.tparams, t.bounds)
        case t: Decl.Def       => s(w(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss, kw(":"), " ", t.decltpe)
        case t: Defn.Val       => s(w(t.mods, " "), kw("val"), " ", r(t.pats, ", "), t.decltpe, " ", kw("="), " ", t.rhs)
        case t: Defn.Var       => s(w(t.mods, " "), kw("var"), " ", r(t.pats, ", "), t.decltpe, " ", kw("="), " ", t.rhs.map(s(_)).getOrElse(s(kw("_"))))
        case t: Defn.Type      => s(w(t.mods, " "), kw("type"), " ", t.name, t.tparams, " ", kw("="), " ", t.body)
        case t: Defn.Class     => s(w(t.mods, " "), kw("class"), " ", t.name, t.tparams, w(" ", t.ctor, t.ctor.mods.nonEmpty), templ(t.templ))
        case t: Defn.Trait     => s(w(t.mods, " "), kw("trait"), " ", t.name, t.tparams, templ(t.templ))
        case t: Defn.Object    => s(w(t.mods, " "), kw("object"), " ", t.name, templ(t.templ))
        case t: Defn.Def       => s(w(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss, t.decltpe, " = ", t.body)
        case t: Defn.Macro     => s(w(t.mods, " "), kw("def"), " ", t.name, t.tparams, t.paramss, kw(":"), " ", t.decltpe, " ", kw("="), " ", kw("macro"), " ", t.body)
        case t: Pkg            =>
          if (options.isLazy && t.stats.isLazy) s(kw("package"), " ", t.ref, " { ... }")
          else if (guessHasBraces(t)) s(kw("package"), " ", t.ref, " {", r(t.stats.map(i(_)), ""), n("}"))
          else s(kw("package"), " ", t.ref, r(t.stats.map(n(_))))
        case t: Pkg.Object     => s(kw("package"), " ", w(t.mods, " "), kw("object"), " ", t.name, templ(t.templ))
        case t: Ctor.Primary   => s(w(t.mods, " ", t.mods.nonEmpty && t.paramss.nonEmpty), t.paramss)
        case t: Ctor.Secondary => s(w(t.mods, " "), kw("def"), " ", kw("this"), t.paramss, if (t.body.is[Term.Block]) " " else " = ", t.body)

        // Template
        case t: Template =>
          val isSelfEmpty = t.self.name.is[Name.Anonymous] && t.self.decltpe.isEmpty
          val isSelfNonEmpty = !isSelfEmpty
          val isBodyEmpty = isSelfEmpty && t.stats.isEmpty
          val isTemplateEmpty = t.early.isEmpty && t.parents.isEmpty && isBodyEmpty
          if (isTemplateEmpty) s()
          else {
            val pearly = if (!t.early.isEmpty) s("{ ", r(t.early, "; "), " } with ") else s()
            val pparents = w(r(t.parents, " with "), " ", !t.parents.isEmpty && !isBodyEmpty)
            val pbody = {
              if (options.isLazy && t.stats.getOrElse(Nil).isLazy) {
                if (isSelfNonEmpty) s("{ ", t.self, " => ... }")
                else s("{ ... }")
              } else {
                val isOneLiner = t.stats.map(stats => stats.length == 0 || (stats.length == 1 && !s(stats.head).toString.contains(EOL))).getOrElse(true)
                (isSelfNonEmpty, t.stats.nonEmpty, t.stats.getOrElse(Nil)) match {
                  case (false, false, _) => s()
                  case (true, false, _) => s("{ ", t.self, " => }")
                  case (false, true, Seq()) if isOneLiner => s("{}")
                  case (false, true, Seq(stat)) if isOneLiner => s("{ ", stat, " }")
                  case (false, true, stats) => s("{", r(stats.map(i(_)), ""), n("}"))
                  case (true, true, Seq()) if isOneLiner => s("{ ", t.self, " => }")
                  case (true, true, Seq(stat)) if isOneLiner => s("{ ", t.self, " => ", stat, " }")
                  case (true, true, stats) => s("{ ", t.self, " =>", r(stats.map(i(_)), ""), n("}"))
                }
              }
            }
            s(pearly, pparents, pbody)
          }

        // Mod
        case Mod.Annot(tree)                 => s(kw("@"), p(SimpleTyp, tree.ctorTpe), tree.ctorArgss)
        case Mod.Private(Name.Anonymous())   => s(kw("private"))
        case Mod.Private(name)               => s(kw("private"), kw("["), s(name), kw("]"))
        case Mod.Protected(Name.Anonymous()) => s(kw("protected"))
        case Mod.Protected(name)             => s(kw("protected"), kw("["), s(name), kw("]"))
        case _: Mod.Implicit                 => kw("implicit")
        case _: Mod.Final                    => kw("final")
        case _: Mod.Sealed                   => kw("sealed")
        case _: Mod.Override                 => kw("override")
        case _: Mod.Case                     => kw("case")
        case _: Mod.Abstract                 => kw("abstract")
        case _: Mod.Covariant                => kw("+")
        case _: Mod.Contravariant            => kw("-")
        case _: Mod.Lazy                     => kw("lazy")
        case _: Mod.ValParam                 => kw("val")
        case _: Mod.VarParam                 => kw("var")

        // Enumerator
        case t: Enumerator.Val           => s(p(Pattern1, t.pat), " = ", p(Expr, t.rhs))
        case t: Enumerator.Generator     => s(p(Pattern1, t.pat), " <- ", p(Expr, t.rhs))
        case t: Enumerator.Guard         => s(kw("if"), " ", p(PostfixExpr, t.cond))

        // Import
        case t: Importee.Name     => s(t.value)
        case t: Importee.Rename   => s(t.from, " ", kw("=>"), " ", t.to)
        case t: Importee.Unimport => s(t.name, " ", kw("=>"), " ", kw("_"))
        case _: Importee.Wildcard => kw("_")
        case t: Importer          => s(t.ref, ".", t.importees)
        case t: Import            => s(kw("import"), " ", r(t.importers, ", "))

        // Case
        case t: Case  =>
          val ppat = p(Pattern, t.pat)
          val pcond = t.cond.map(cond => s(" ", kw("if"), " ", p(PostfixExpr, cond))).getOrElse(s())
          val isOneLiner = {
            def isOneLiner(t: Case) = t.stats.length == 0 || (t.stats.length == 1 && !s(t.stats.head).toString.contains(EOL))
            t.parent match {
              case Some(Term.Match(_, cases)) => cases.forall(isOneLiner)
              case Some(Term.PartialFunction(cases)) => cases.forall(isOneLiner)
              case _ => isOneLiner(t)
            }
          }
          val pbody = (t.stats, isOneLiner) match {
            case (Nil, true) => s("")
            case (List(stat), true) => s(" ", stat)
            case (stats, _) => r(stats.map(i(_)), "")
          }
          s("case ", ppat, pcond, " ", kw("=>"), pbody)

        // Source
        case t: Source                   => r(t.stats, EOL)
      }

      // Multiples and optionals
      implicit def syntaxArgs: Syntax[Seq[Term.Arg]] = Syntax {
        case (b: Term.Block) :: Nil => s(" ", b)
        case args                   => s("(", r(args, ", "), ")")
      }
      implicit def syntaxArgss: Syntax[Seq[Seq[Term.Arg]]] = Syntax {
        r(_)
      }
      implicit def syntaxTargs: Syntax[Seq[Type]] = Syntax { targs =>
        if (targs.isEmpty) s()
        else s("[", r(targs, ", "), "]")
      }
      implicit def syntaxPtargs: Syntax[Seq[Pat.Type]] = Syntax { ptargs =>
        if (ptargs.isEmpty) s()
        else s("[", r(ptargs, ", "), "]")
      }
      implicit def syntaxPats: Syntax[Seq[Pat]] = Syntax { pats =>
        s("(", r(pats, ", "), ")")
      }
      implicit def syntaxPatArgs: Syntax[Seq[Pat.Arg]] = Syntax { pats =>
        s("(", r(pats, ", "), ")")
      }
      implicit def syntaxMods: Syntax[Seq[Mod]] = Syntax { mods =>
        if (mods.nonEmpty) r(mods, " ") else s()
      }
      implicit def syntaxAnnots: Syntax[Seq[Mod.Annot]] = Syntax { annots =>
        if (annots.nonEmpty) r(annots, " ") else s()
      }
      implicit def syntaxParams[P <: Term.Param]: Syntax[Seq[P]] = Syntax { params =>
        s("(", r(params, ", "), ")")
      }
      implicit def syntaxParamss[P <: Term.Param]: Syntax[Seq[Seq[P]]] = Syntax { paramss =>
        r(paramss.map(params => {
          s("(", w("implicit ", r(params, ", "), params.exists(_.mods.exists(_.is[Mod.Implicit]))), ")")
        }), "")
      }
      implicit def syntaxTparams: Syntax[Seq[Type.Param]] = Syntax { tparams =>
        if (tparams.nonEmpty) s("[", r(tparams, ", "), "]") else s()
      }
      implicit def syntaxTypeArgOpt: Syntax[Option[Type.Arg]] = Syntax {
        _.map { t => s(kw(":"), " ", t) }.getOrElse(s())
      }
      implicit def syntaxTypeOpt: Syntax[Option[Type]] = Syntax {
        _.map { t => s(kw(":"), " ", t) }.getOrElse(s())
      }
      implicit def syntaxTermNameOpt: Syntax[Option[Term.Name]] = Syntax {
        _.map(s(_)).getOrElse(s(")"))
      }
      implicit def syntaxImportee: Syntax[Seq[Importee]] = Syntax {
        case (t: Importee.Name) :: Nil     => s(t)
        case (t: Importee.Wildcard) :: Nil => s(t)
        case importees                     => s("{ ", r(importees, ", "), " }")
      }
    }
    // NOTE: This is the current state of the art of smart prettyprinting.
    // If we prettyprint a tree that's just been parsed with the same dialect,
    // then we retain formatting. Otherwise, we don't, even in the tiniest.
    // I expect to improve on this in the nearest future, because we had it much better until recently.
    Syntax { (x: T) =>
      x.origin match {
        // NOTE: Options don't really matter,
        // because if we've parsed a tree, it's not gonna contain lazy seqs anyway.
        // case Origin.Parsed(_, originalDialect, _) if dialect == originalDialect && options == Options.Eager =>
        case Origin.Parsed(_, originalDialect, _) if dialect == originalDialect =>
          s(new String(x.pos.input.chars, x.pos.start.offset, x.pos.end.offset - x.pos.start.offset))
        case _ =>
          syntaxInstances.syntaxTree[T].apply(x)
      }
    }
  }
}

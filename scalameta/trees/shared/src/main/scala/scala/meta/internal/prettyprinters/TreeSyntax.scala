package scala.meta
package internal
package prettyprinters

import org.scalameta.internal.ScalaCompat.EOL
import org.scalameta.invariants._
import org.scalameta.{debug, unreachable}
import scala.meta.classifiers._
import scala.meta.inputs.Position
import scala.meta.internal.tokens.Chars._
import scala.meta.internal.trees._
import scala.meta.prettyprinters._
import scala.meta.tokens._
import scala.meta.trees.Origin

import scala.annotation.tailrec
import scala.reflect.{ClassTag, classTag}

object TreeSyntax {
  import Show.{alt, blank, function => fn, indent => i, meta => m, newline => n, nosplit => nosp,
    opt => o, repeat => r, sequence => s, space => sp, wrap => w}

  private final class SyntaxInstances(implicit dialect: Dialect) {
    val keywords = tokenizers.keywords(dialect)
    import TreeSyntacticGroup._

    private val escapableSoftKeywords: Map[String, Seq[Class[_]]] = {
      val seq = Seq.newBuilder[(String, Seq[ClassTag[_]])]

      seq += "|" -> Seq(classTag[Pat.ExtractInfix])

      if (dialect.allowExtensionMethods) seq += "extension" ->
        Seq(classTag[Term.Apply], classTag[Term.ApplyUsing])

      if (dialect.allowInlineMods) seq += "inline" ->
        Seq(classTag[Term.Apply], classTag[Term.ApplyUsing], classTag[Term.ApplyInfix])

      if (dialect.allowStarWildcardImport) seq += "*" -> Seq(classTag[Importee])
      if (dialect.allowStarAsTypePlaceholder) {
        seq += "*" -> Seq(classTag[Type.ArgClause])
        seq += "-*" -> Seq(classTag[Type.ArgClause])
        seq += "+*" -> Seq(classTag[Type.ArgClause])
      }
      seq += "*" -> Seq(classTag[Term.SelectPostfix])

      if (dialect.allowQuestionMarkAsTypeWildcard) seq += "?" -> Seq(classTag[Type.ArgClause])

      seq.result().groupBy(_._1).map { case (k, vs) =>
        k -> vs.flatMap(_._2).distinct.map(_.runtimeClass)
      }
    }

    def p(og: TreeSyntacticGroup, t: Tree, force: Boolean = false) = s(t) match {
      case x: Show.Meta =>
        val needParens = force ||
          (x.data match {
            case ig: TreeSyntacticGroup => groupNeedsParens(og, ig)
            case _ => false
          })
        w("(", x.res, ")", needParens)
      case res => res
    }

    def kw(keyword: String) = fn { sb =>
      val sbLen = sb.length
      val prelast = if (sbLen >= 2) sb.charAt(sbLen - 2) else ' '
      val last = if (sbLen >= 1) sb.charAt(sbLen - 1) else ' '
      val next = if (keyword.nonEmpty) keyword(0) else ' '
      val danger = {
        val opThenOp = isOperatorPart(last) && isOperatorPart(next)
        val underscoreThenOp = isIdentifierPart(prelast) && last == '_' && isOperatorPart(next)
        opThenOp || underscoreThenOp
      }
      if (danger) s(" " + keyword) else s(keyword)
    }

    def guessIsBackquoted(t: Name): Boolean = {
      def cantBeWrittenWithoutBackquotes(t: Name): Boolean = {
        // Fold over codepoints for a given string
        def foldCodepoints[T](value: String, start: T)(f: (Int, T, Int) => T): T = {
          val length = value.length
          @annotation.tailrec
          def work(offset: Int, acc: T): T =
            if (offset >= length) acc
            else {
              val codepoint = value.codePointAt(offset)
              work(offset + Character.charCount(codepoint), f(offset, acc, codepoint))
            }
          work(0, start)
        }
        // These rules are transcribed from
        // https://github.com/scala/scala/blob/2.13.x/spec/01-lexical-syntax.md#lexical-syntax
        def lexicalWhitespace(codepoint: Int): Boolean =
          Set[Int]('\u0020', '\u0009', '\u000D', '\u000A').contains(codepoint)
        def lexicalLetter(codepoint: Int): Boolean = isNameStart(codepoint) || codepoint == '$'
        def lexicalDigit(codepoint: Int): Boolean = '0' <= codepoint && codepoint <= '9'
        def lexicalParentheses(codepoint: Int): Boolean = Set[Int]('(', ')', '[', ']', '{', '}')
          .contains(codepoint)
        def lexicalDelimiter(codepoint: Int): Boolean = Set[Int]('`', '\'', '"', '.', ';', ',')
          .contains(codepoint)
        def lexicalOperator(codepoint: Int): Boolean = isSpecial(codepoint) ||
          '\u0020' <= codepoint && codepoint <= '\u007E' && !(lexicalWhitespace(codepoint) ||
            lexicalLetter(codepoint) || lexicalDigit(codepoint) || lexicalParentheses(codepoint) ||
            lexicalDelimiter(codepoint))

        sealed trait OperatorState
        case object Accepted extends OperatorState
        case object Required extends OperatorState
        case object Forbidden extends OperatorState

        sealed trait ValidityState
        case object Valid extends ValidityState
        case object Invalid extends ValidityState

        def validPlainid(string: String): Boolean = {
          val (_, validity) =
            foldCodepoints[(OperatorState, ValidityState)](string, (Accepted, Valid)) {
              // Any invalid state is invalid
              case (_, (_, Invalid), _) => (Forbidden, Invalid)
              // Must start with either a letter or an operator
              case (0, (Accepted, Valid), next) if lexicalLetter(next) => (Forbidden, Valid)
              case (0, (Accepted, Valid), next) if lexicalOperator(next) => (Required, Valid)
              // Non-leading underscores reset operator validity
              case (_, (Forbidden, Valid), '_') => (Accepted, Valid)
              // Non-leading operators are accepted only after underscores
              case (_, (Accepted, Valid), next) if lexicalOperator(next) => (Required, Valid)
              // Operators must not be followed by non-operators
              case (_, (Required, Valid), next) if lexicalOperator(next) => (Required, Valid)
              // Lexical letters and digits can follow underscores
              case (_, (Accepted, Valid), next) if lexicalLetter(next) => (Forbidden, Valid)
              case (_, (Accepted, Valid), next) if lexicalDigit(next) => (Forbidden, Valid)
              // Non-operators must not be followed by operators
              case (_, (Forbidden, Valid), next) if lexicalLetter(next) => (Forbidden, Valid)
              case (_, (Forbidden, Valid), next) if lexicalDigit(next) => (Forbidden, Valid)
              // Bail on anything not matched here
              case _ => (Forbidden, Invalid)
            }

          validity == Valid
        }

        @tailrec
        def withinMacroQuote(tree: Tree): Boolean = tree.parent match {
          case Some(_: Term.QuotedMacroExpr) => true
          case Some(p: Stat) => withinMacroQuote(p)
          case _ => false
        }

        val name = t.value
        name.nonEmpty &&
        (keywords.contains(name) || name.contains("//") || name.contains("/*") ||
          name.contains("*/") || !validPlainid(name) || {
            val ch = name.codePointAt(0)
            lexicalDigit(ch) ||
            ch == '$' && dialect.allowSpliceAndQuote && name.length > 1 && withinMacroQuote(t)
          })
      }
      def thisLocationAlsoAcceptsPatVars(p: Tree): Boolean = p match {
        case p: Term.Name => unreachable
        case _: Term.SelectLike => false
        case p: Pat.Wildcard => unreachable
        case p: Pat.Var => false
        case p: Pat.Repeated => false
        case p: Pat.Bind => true
        case p: Pat.Alternative => true
        case p: Pat.ArgClause => p.values.contains(t)
        case p: Pat.Tuple => true
        case p: Pat.Extract => false
        case p: Pat.ExtractInfix => p.lhs eq t
        case p: Pat.Assign => false
        case p: Pat.Interpolate => p.args.contains(t)
        case p: Pat.Typed => unreachable
        case p: Pat => unreachable
        case p: Case => p.pat eq t
        case p: Defn.Val => p.pats.contains(t)
        case p: Defn.Var => p.pats.contains(t)
        case p: Enumerator.Assign => p.pat eq t
        case _ => false
      }
      @tailrec
      def thisLocationAlsoAcceptsTypeVars(p: Tree): Boolean = p match {
        case _: Type.Var | _: Pat.Typed => false
        case _: Pat => true
        case p @ (_: Type | _: Type.ArgClause) => p.parent match {
            case Some(pp) => thisLocationAlsoAcceptsTypeVars(pp)
            case _ => false
          }
        case _ => false
      }
      /* Soft keywords might need to be written with backquotes in some places.
       * Previously used match clause fails due to:
       * https://github.com/scala-native/scala-native/issues/2187
       * instead we went with if clause to work around the issue.
       */
      def isEscapableSoftKeyword(t: Name, parent: Tree): Boolean = escapableSoftKeywords.get(t.value)
        .exists(_.exists(_.isInstance(parent)))

      def isAmbiguousInParent(t: Tree)(parent: Tree): Boolean = t match {
        case t: Name =>
          def patVarIsOk = t match {
            case _: Term => thisLocationAlsoAcceptsPatVars(parent)
            case _: Type => thisLocationAlsoAcceptsTypeVars(parent)
            case _ => false
          }
          t.value.head.isLower && patVarIsOk || isEscapableSoftKeyword(t, parent)
        case _ => false
      }
      cantBeWrittenWithoutBackquotes(t) || t.parent.exists(isAmbiguousInParent(t))
    }
    def guessHasExpr(t: Term.Return): Boolean = t.expr match {
      case Lit.Unit() => false
      case _ => true
    }
    def guessHasElsep(t: Term.If): Boolean = t.elsep match {
      case Lit.Unit() => false
      case e => true
    }
    def guessHasBraces(t: Pkg): Boolean = {
      @tailrec
      def isOnlyChildOfOnlyChild(t: Tree): Boolean = t.parent match {
        case Some(p: Pkg.Body) => p.stats.lengthCompare(1) == 0 && isOnlyChildOfOnlyChild(p)
        case Some(p: Pkg) => isOnlyChildOfOnlyChild(p)
        case Some(p: Source) => p.stats.lengthCompare(1) == 0
        case Some(p) => unreachable(debug(p))
        case None => true
      }
      !isOnlyChildOfOnlyChild(t)
    }
    @annotation.tailrec
    def guessNeedsLineSep(t: Tree): Boolean = t match {
      case t: Term.ApplyUnary => guessNeedsLineSep(t.arg)
      case _: Decl.Val | _: Decl.Var | _: Decl.Def | _: Defn.Type | _: Term.Ref | _: Member.Apply |
          _: Term.Ascribe | _: Term.Tuple | _: Term.New | _: Term.Interpolate | _: Term.Xml |
          _: Lit => !dialect.allowSignificantIndentation
      case t: Term.Do => false
      case t: Tree.WithBody => guessNeedsLineSep(t.body)
      case t: Stat.WithTemplate => t.templ.self.isEmpty && t.templ.stats.isEmpty
      case t: Term.ApplyInfix =>
        val args = t.argClause.values
        args.lengthCompare(1) != 0 || guessNeedsLineSep(args.head)
      case t: Term.Return => guessNeedsLineSep(t.expr)
      case t: Term.Throw => guessNeedsLineSep(t.expr)
      case t: Term.If => guessNeedsLineSep(if (guessHasElsep(t)) t.elsep else t.thenp)
      case t: Term.Try => t.finallyp match {
          case Some(x) => guessNeedsLineSep(x)
          case _ => t.catchClause.isEmpty && guessNeedsLineSep(t.expr)
        }
      case t: Term.TryWithHandler => guessNeedsLineSep(t.finallyp.getOrElse(t.catchp))
      case _ => false
    }

    // Branches
    implicit def syntaxTree[T <: Tree]: Syntax[T] = AsTreeSyntax {
      // Bottom
      case t: Quasi =>
        implicit val unquoteDialect: Dialect = dialect.unquoteParentDialect
        if (null eq unquoteDialect)
          throw new UnsupportedOperationException(s"$dialect doesn't support unquoting")
        if (t.rank > 0) s("." * (t.rank + 1), w("{", t.tree, "}", !t.tree.is[Quasi]))
        else {
          val allowBraceless = t.tree.is[Term.Name] || t.tree.is[Pat.Var] || t.tree.is[Term.This] ||
            t.tree.is[Pat.Wildcard]
          s("$", w("{", s(t.tree), "}", !allowBraceless))
        }

      // Name
      case _: Name.This => s("this")
      case _: Name.Anonymous => s()
      case _: Name.Placeholder => s("_")
      case _: Term.Anonymous => s("")
      case t: Name.Indeterminate => printMaybeBackquoted(t)

      // Term
      case t: Term.This => m(Path, w(t.qual, "."), kw("this"))
      case t: Term.Super => m(Path, s(w(t.thisp, "."), kw("super"), w("[", t.superp, "]")))
      case t: Term.Name => m(Path, printMaybeBackquoted(t))
      case t: Term.CapSetName => m(Path, s(printMaybeBackquoted(t), "^"))
      case t: Term.Select => printSelect(t, ".")
      case t: Term.SelectPostfix =>
        val (sep, backquoteExpr) = t.qual match {
          case q: Term.Name // if it looks like a leading infix, don't use space
              if dialect.allowInfixOperatorAfterNL &&
                q.value.isIdentSymbolicNonBackquotedInfixOperator =>
            "" -> t.name.value.isIdentSymbolicNonBackquotedInfixOperator
          case _ => " " -> false
        }
        printSelect(t, sep, bqExpr = backquoteExpr)
      case t: Term.Interpolate =>
        /** @see LegacyScanner.getStringPart, when ch == '$' */
        def needBraces(id: String, nextPart: String): Boolean =
          !Character.isUnicodeIdentifierStart(id.head) ||
            nextPart.headOption.exists(Character.isUnicodeIdentifierPart)

        val parts = t.parts.map { case Lit(part: String) => part.replace("$", "$$") }
        val zipped = parts.zip(t.args).zip(parts.tail).map {
          case ((part, id: Name), next) if !guessIsBackquoted(id) && !needBraces(id.value, next) =>
            s(part, "$", id.value)
          case ((part, arg), _) => s(part, "$", w("{", p(Expr, arg), "}", !arg.is[Term.Block]))
        }
        val quote = if (parts.exists(s => s.contains("\n") || s.contains("\""))) "\"\"\"" else "\""
        m(SimpleExpr1, s(t.prefix, quote, r(zipped), parts.last, quote))
      case t: Term.Xml =>
        if (!dialect.allowXmlLiterals)
          throw new UnsupportedOperationException(s"$dialect doesn't support xml literals")
        val parts = t.parts.map { case Lit(part: String) => part }
        val zipped = parts.zip(t.args).map { case (part, arg) =>
          s(part, w("{", p(Expr, arg), "}", !arg.is[Term.Block]))
        }
        m(SimpleExpr1, s(r(zipped), parts.last))

      case t: Term.ArgClause => nosp(s("(", o(t.mod, " "), r(t.values, ", "), ")"))
      case t: Term.Apply => m(SimpleExpr1, s(p(SimpleExpr1, t.fun), printApplyArgs(t.argClause, " ")))
      case t: Term.ApplyUsing =>
        val args = s("(", kw("using"), " ", r(t.argClause.values, ", "), ")")
        m(SimpleExpr1, s(p(SimpleExpr1, t.fun), nosp(withComments(t.argClause)(args))))
      case t: Term.ApplyType => m(SimpleExpr1, s(p(SimpleExpr, t.fun), t.targClause))
      case t: Term.ApplyInfix =>
        val sg = InfixExpr(t)
        val args = t.argClause.values match {
          case (arg: Term) :: Nil if (arg match {
                case _: Lit.Unit => false
                case _: Lit | _: Term.Ref | _: Term.Function | _: Term.Apply | _: Term.Placeholder |
                    _: Term.MatchLike | _: Term.ApplyInfix | _: Term.MacroLike | _: Term.If => true
                case _ => false
              }) => p(sg, arg)
          case _ => printApplyArgs(t.argClause, "")
        }
        m(sg, s(p(sg, t.lhs), sp(s(t.op, t.targClause, sp(args)))))
      case t: Term.ApplyUnary => m(PrefixExpr, s(t.op, p(SimpleExpr, t.arg)))
      case t: Term.Assign => m(Expr1, s(p(SimpleExpr1, t.lhs), " ", kw("="), " ", p(Expr, t.rhs)))
      case t: Term.Return =>
        m(Expr1, s(kw("return"), if (guessHasExpr(t)) s(" ", p(Expr, t.expr)) else s()))
      case t: Term.Throw => m(Expr1, s(kw("throw"), " ", p(Expr, t.expr)))
      case t: Term.Ascribe => m(Expr1, s(p(PostfixExpr, t.expr), kw(":"), " ", t.tpe))
      case t: Term.Annotate => m(Expr1, s(p(PostfixExpr, t.expr), kw(":"), " ", t.annots))
      case t: Term.Tuple => m(SimpleExpr1, s("(", r(t.args, ", "), ")"))
      case t: Term.Block =>
        import Term.{Block, Function}
        def block(pre: Show.Result, x: Seq[Stat]) = {
          val res = if (pre == Show.None && x.isEmpty) s("{}") else s("{", w(" ", pre), x, n("}"))
          m(SimpleExpr, res)
        }
        t match {
          case Block(
                Function
                  .Initial(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, Block(stats)) :: Nil
              ) if mods.exists(_.is[Mod.Implicit]) =>
            block(s("implicit ", name, o(": ", tptopt), " =>"), stats)
          case Block(
                Function.Initial(Term.Param(_, name: Name, None, _) :: Nil, Block(stats)) :: Nil
              ) => block(s(name, " =>"), stats)
          case Block(Function.Initial(params, Block(stats)) :: Nil) =>
            block(s("(", r(params, ", "), ") =>"), stats)
          case _ => block(s(), t.stats)
        }
      case t: Term.If =>
        val needParens: Boolean = t.thenp match {
          case innerIf: Term.If => !guessHasElsep(innerIf) && guessHasElsep(t)
          case _ => false
        }
        m(
          Expr1,
          s(
            w(t.mods, " "),
            kw("if"),
            " (",
            t.cond,
            ") ",
            p(Expr, t.thenp, force = needParens),
            if (guessHasElsep(t)) s(" ", kw("else"), " ", p(Expr, t.elsep)) else s()
          )
        )
      case t: Term.SelectMatch if dialect.allowMatchAsOperator =>
        val mods = t.mods
        val pref = if (mods.isEmpty) Path else Expr1
        m(pref, s(w(mods, " "), printSelectLhs(t.expr), ".", kw("match"), " ", t.casesBlock))
      case t: Term.MatchLike =>
        m(Expr1, s(w(t.mods, " "), p(PostfixExpr, t.expr), " ", kw("match"), " ", t.casesBlock))
      case t: Term.TryClause =>
        val showExpr = p(Expr, t.expr)
        val needParensAroundExpr = t.expr match {
          case e: Term.TryClause if e.finallyp.isEmpty =>
            if (e.catchClause.isEmpty) t.catchClause.nonEmpty || t.finallyp.isDefined
            else t.catchClause.isEmpty && t.finallyp.isDefined
          case _ => false
        }
        m(
          Expr1,
          s(
            kw("try"),
            " ",
            if (needParensAroundExpr) s("(", i(showExpr), n(")")) else showExpr,
            t.catchClause.fold(s())(x => s(" ", kw("catch"), " ", x)),
            t.finallyp.fold(s())(finallyp => s(" ", kw("finally"), " ", finallyp))
          )
        )
      case t: Term.AnonymousFunction => s(t.body)
      case t: Term.FunctionTerm =>
        val arrow = t match {
          case _: Term.Function => "=>"
          case _: Term.ContextFunction => "?=>"
        }
        val paramsSyntax = printParams(t.paramClause, needParens = false)
        m(Expr, s(paramsSyntax, " ", kw(arrow), " ", p(Expr, t.body)))
      case t: Term.PolyFunction => m(Expr, t.tparamClause, " ", kw("=>"), " ", p(Expr, t.body))
      case t: Term.QuotedMacroType => s("'[ ", t.tpe, " ]")
      case t: Term.QuotedMacroExpr => printMacroExprBody(t.body, "'")
      case t: Term.SplicedMacroExpr => printMacroExprBody(t.body, "$")
      case t: Term.SplicedMacroPat => s("${ ", t.pat, " }")
      case t: Pat.Macro => m(SimplePattern, s(t.body))
      case t: Type.Macro => m(SimpleTyp, s(t.body))
      case t: Term.PartialFunction => m(SimpleExpr, s("{", t.cases, n("}")))
      case t: Term.While => m(Expr1, s(kw("while"), " (", t.expr, ") ", p(Expr, t.body)))
      case t: Term.Do =>
        m(Expr1, s(kw("do"), " ", p(Expr, t.body), " ", kw("while"), " (", t.expr, ")"))
      case t: Term.For => m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", t.body))
      case t: Term.ForYield =>
        m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", kw("yield"), " ", t.body))
      case t: Term.New => m(SimpleExpr, s(kw("new"), " ", t.init))
      case t: Term.EndMarker => s(kw("end"), " ", t.name.value)
      case t: Term.NewAnonymous => m(SimpleExpr, s(kw("new"), " ", alt(t.templ, "{}")))
      case _: Term.Placeholder => m(SimpleExpr1, kw("_"))
      case t: Term.Eta => m(PostfixExpr, s(p(SimpleExpr1, t.expr), " ", kw("_")))
      case t: Term.Repeated =>
        if (dialect.allowPostfixStarVarargSplices) s(p(PostfixExpr, t.expr), kw("*"))
        else s(p(PostfixExpr, t.expr), kw(":"), " ", kw("_*"))
      case t: Term.Param =>
        // NOTE: `implicit/using` in parameters is skipped as it applies to whole list
        printParam(t)
      case t: Term.ParamClause => nosp(printParams(t, needParens = !t.parent.is[Term]))

      // Type
      case t: Type.AnonymousName => m(Path, s(""))
      case t: Type.Name => m(Path, printMaybeBackquoted(t))
      case t: Type.CapSetName => m(Path, s(printMaybeBackquoted(t), "^"))
      case t: Type.Select => m(SimpleTyp, s(t.qual, kw("."), t.name))
      case t: Type.Project => m(SimpleTyp, s(p(SimpleTyp, t.qual), kw("#"), t.name))
      case t: Type.Singleton => m(SimpleTyp, s(p(SimpleExpr1, t.ref), ".", kw("type")))
      case t: Type.ArgClause => nosp(r("[", ", ", "]")(t.values.map(p(Typ, _)): _*))
      case t: Type.Apply => m(SimpleTyp, s(p(SimpleTyp, t.tpe), t.argClause))
      case t: Type.ApplyInfix =>
        val sg = InfixTyp(t)
        m(sg, s(p(sg, t.lhs), " ", t.op, " ", p(sg, t.rhs)))
      case t: Type.FuncParamClause => t.values match {
          case arg :: Nil if (arg match {
                case _: Type.Tuple | _: Type.ByNameType | _: Type.FunctionParamOrArg |
                    _: Type.ParamFunctionType | _: Type.PolyFunction => false
                case _ => true
              }) => s(arg)
          case args => s("(", r(args, ", "), ")")
        }
      case t: Type.Function => printFunctionType(t, "=>")
      case t: Type.ContextFunction => printFunctionType(t, "?=>")
      case t: Type.PureFunction => printFunctionType(t, Token.pureFunctionArrow)
      case t: Type.PureContextFunction => printFunctionType(t, Token.pureContextFunctionArrow)
      case t: Type.Tuple => m(SimpleTyp, s("(", r(t.args, ", "), ")"))
      case t: Type.With => m(WithTyp, s(p(WithTyp, t.lhs), " with ", p(WithTyp, t.rhs)))
      case t: Type.And => s(Type.ApplyInfix(t.lhs, Type.Name("&"), t.rhs))
      case t: Type.Or => s(Type.ApplyInfix(t.lhs, Type.Name("|"), t.rhs))
      case t: Type.Refine =>
        m(RefineTyp, t.tpe.map(tpe => s(p(WithTyp, tpe), " ")).getOrElse(s("")), t.body)
      case t: Type.Existential => m(Typ, s(p(AnyInfixTyp, t.tpe), " ", kw("forSome"), " ", t.body))
      case t: Type.Annotate => m(AnnotTyp, s(p(SimpleTyp, t.tpe), " ", t.annots))
      case t: Type.Lambda => m(Typ, t.tparamClause, " ", kw("=>>"), " ", p(Typ, t.tpe))
      case t: Type.PolyFunction => m(Typ, t.tparamClause, " ", kw("=>"), " ", p(Typ, t.tpe))
      case t: Type.Match => m(Type, s(p(AnyInfixTyp, t.tpe), " ", kw("match"), " {", t.cases, n("}")))
      case t: Type.AnonymousLambda => s(t.tpe)
      case t: Type.AnonymousParam =>
        val useStar = dialect.allowStarAsTypePlaceholder &&
          t.origin.tokensOpt.exists(!_.lastOption.is[Token.Underscore])
        val ph = if (useStar) "*" else "_"
        m(SimpleTyp, o(t.variant), ph)
      case t: Type.Wildcard =>
        /* In order not to break existing tools `.syntax` should still return
         * `_` instead `?` unless specifically used.
         */
        def questionMarkUsed = t.origin.tokensOpt.exists(!_.headOption.is[Token.Underscore])
        val useQM = dialect.allowQuestionMarkAsTypeWildcard &&
          (dialect.allowUnderscoreAsTypePlaceholder || questionMarkUsed)
        m(SimpleTyp, s(kw(if (useQM) "?" else "_")), t.bounds)
      case t: Type.PatWildcard => m(SimpleTyp, kw("_"))
      case t: Type.Placeholder =>
        /* In order not to break existing tools `.syntax` should still return
         * `_` instead `?` unless specifically used.
         */
        def questionMarkUsed = t.origin.tokensOpt.exists(!_.exists(_.is[Token.Underscore]))
        val useQM = dialect.allowQuestionMarkAsTypeWildcard &&
          (dialect.allowUnderscoreAsTypePlaceholder || questionMarkUsed)
        m(SimpleTyp, s(kw(if (useQM) "?" else "_"), t.bounds))
      case t: Type.Bounds => printBounds(t)
      case t: Type.Repeated => t.tpe match {
          case tByName: Type.ByName => m(ParamTyp, s(kw("=>"), " ", p(Typ, tByName.tpe), kw("*")))
          case _ => m(ParamTyp, s(p(Typ, t.tpe), kw("*")))
        }
      case t: Type.ByName => printByNameType(t, "=>")
      case t: Type.PureByName => printByNameType(t, Token.pureFunctionArrow)
      case t: Type.Var => m(SimpleTyp, s(t.name.value))
      case t: Type.FunctionArg => m(ParamTyp, w(t.mods, " "), p(Typ, t.tpe))
      case t: Type.TypedParam => m(SimpleTyp, w(t.mods, " "), s(t.name.value), ": ", p(Typ, t.typ))
      case t: Type.Assign => m(SimpleTyp, s(t.name.value), " ", kw("="), " ", p(Typ, t.rhs))
      case t: Type.ParamClause => r("[", ", ", "]")(t.values: _*)
      case t: Type.BoundsAlias => m(SimpleTyp, s(t.bounds, " ", "as", " ", t.name))
      case t: Type.Param =>
        def isVariant(m: Mod) = m.is[Mod.Variant]
        val mods = t.mods.filterNot(isVariant)
        require(t.mods.length - mods.length <= 1)
        val variance = o(t.mods.find(isVariant))
        val bounds = printBounds(t.bounds, t.vbounds)
        s(w(mods, " "), variance, t.name, t.tparamClause, bounds)
      case t: Type.Block => s(w(r(t.typeDefs, "; "), "; "), t.tpe)
      case _: Type.CapturesAny => s()
      case t: Type.CapturesSet => s("{", r(t.values, ", "), "}")
      case t: Type.Capturing => t.tpe match {
          case tpe: Type.FunctionLikeType => s(tpe)
          case tpe => s(tpe, "^", t.caps)
        }

      // Pat
      case t: Pat.Var => m(SimplePattern, t.name)
      case _: Pat.Wildcard => m(SimplePattern, kw("_"))
      case a: Pat.Assign => m(SimplePattern, s(a.name, " ", kw("="), " ", a.rhs))
      case _: Pat.SeqWildcard => m(SimplePattern, kw("_*"))
      case t: Pat.Repeated => m(SimplePattern, t.name, kw("*"))
      case pat: Pat.Given => m(AnyPattern3, s(kw("given"), " ", p(RefineTyp, pat.tpe)))
      case t: Pat.Bind =>
        val separator = t.rhs match {
          case Pat.SeqWildcard() =>
            if (dialect.allowAtForExtractorVarargs) s(" ", kw("@"))
            else if (dialect.allowColonForExtractorVarargs) s(kw(":"))
            else
              throw new UnsupportedOperationException(s"$dialect doesn't support extractor varargs")
          case _ => s(" ", kw("@"))
        }
        m(Pattern2, s(p(SimplePattern, t.lhs), separator, " ", p(AnyPattern3, t.rhs)))
      case t: Pat.Alternative => m(Pattern, s(p(Pattern, t.lhs), " ", kw("|"), " ", p(Pattern, t.rhs)))
      case t: Pat.Tuple => m(SimplePattern, s("(", r(t.args, ", "), ")"))
      case t: Pat.ArgClause => m(SimplePattern, s("(", r(t.values, ", "), ")"))
      case t: Pat.Extract => m(SimplePattern, s(t.fun, t.argClause))
      case t: Pat.ExtractInfix =>
        val pop = InfixPat(t)
        val rhs = t.argClause match {
          case Pat.ArgClause(pat :: Nil) if !pat.is[Lit.Unit] => s(p(pop, pat))
          case pats => s(pats)
        }
        m(pop, s(p(pop, t.lhs), " ", t.op, " ", rhs))
      case t: Pat.Interpolate =>
        /** @see LegacyScanner.getStringPart, when ch == '$' */
        def needBraces(id: String): Boolean = !Character.isUnicodeIdentifierStart(id.head)
        val parts = t.parts.map { case Lit(part: String) => part }
        val zipped = parts.zip(t.args).map {
          case (part, id: Name) if !guessIsBackquoted(id) && !needBraces(id.value) =>
            s(part, "$", id.value)
          case (part, arg) => s(part, "$", w("{", arg, "}", !arg.is[Term.Block]))
        }
        m(SimplePattern, s(t.prefix, "\"", r(zipped), parts.last, "\""))
      case t: Pat.Xml =>
        if (!dialect.allowXmlLiterals)
          throw new UnsupportedOperationException(s"$dialect doesn't support xml literals")
        val parts = t.parts.map { case Lit(part: String) => part }
        val zipped = parts.zip(t.args).map { case (part, arg) => s(part, "{", arg, "}") }
        m(SimplePattern, s(r(zipped), parts.last))
      case Pat.Typed(lhs, rhs: Lit) =>
        if (dialect.allowLiteralTypes)
          m(Pattern1, s(p(SimplePattern, lhs), kw(":"), " ", p(Literal, rhs)))
        else throw new UnsupportedOperationException(s"$dialect doesn't support literal types")
      case t: Pat.Typed => m(Pattern1, s(p(SimplePattern, t.lhs), kw(":"), " ", p(RefineTyp, t.rhs)))

      // Lit
      case Lit.Boolean(value) => m(Literal, s(value.toString))
      case Lit.Byte(value) => m(
          Literal,
          s(
            "ByteLiterals.",
            if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value
          )
        )
      case Lit.Short(value) => m(
          Literal,
          s(
            "ShortLiterals.",
            if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value
          )
        )
      case Lit.Int(value) => m(Literal, s(value.toString))
      case Lit.Long(value) => m(Literal, s(value.toString + "L"))
      case t: Lit.Float =>
        val format = t.format
        w(s(format), "f", Character.toLowerCase(format.last) != 'f')
      case t: Lit.Double =>
        val format = t.format
        val suffix =
          if (Character.toLowerCase(format.last) == 'd') None
          else if (t.origin.textOpt.exists(x => Character.toLowerCase(x.last) == 'd')) Some("d")
          else if (format.forall(Character.isDigit)) Some(".0")
          else None
        s(format, o(suffix))
      case t: Lit.IntXL => s(t.value.toString)
      case t: Lit.FloatXL => s(t.digits, w("e", s(t.exponent)))
      case t: Lit.WithUnary => s(t.op, t.arg)
      case t @ Lit.Char(value) =>
        val syntax = t.pos match {
          case Position.None => SingleQuotes(value)
          case pos => pos.text
        }
        m(Literal, s(syntax))
      case t @ Lit.String(value) =>
        val syntax = t.pos match {
          case Position.None => DoubleQuotes.orTriple(value)
          case pos => pos.text
        }
        m(Literal, s(syntax))
      case Lit.Symbol(value) => m(Literal, s("'", value.name))
      case Lit.Null() => m(Literal, s(kw("null")))
      case Lit.Unit() => m(Literal, s("()"))

      // Member
      case t: Member.ParamClauseGroup => s(t.tparamClause, t.paramClauses)
      case t: Decl.Val => s(w(t.mods, " "), kw("val"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
      case t: Decl.Var => s(w(t.mods, " "), kw("var"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
      case t: Decl.Type => s(w(t.mods, " "), kw("type"), " ", t.name, t.tparamClause, t.bounds)
      case t: Decl.Def =>
        s(w(t.mods, " "), kw("def "), t.name, t.paramClauseGroups, kw(": "), t.decltpe)
      case t: Decl.GivenLike =>
        val sig = givenSig(t.name, t.paramClauseGroups)
        r(" ")(t.mods, sig, givenDeclTpe(t.decltpe))
      case t: Defn.Val =>
        s(w(t.mods, " "), kw("val"), " ", r(t.pats, ", "), t.decltpe, " ", kw("="), " ", t.rhs)
      case t: Defn.Var =>
        s(w(t.mods, " "), kw("var"), " ", r(t.pats, ", "), t.decltpe, " ", kw("="), " ", t.body)
      case t: Defn.Type => s(
          w(t.mods, " "),
          kw("type"),
          " ",
          t.name,
          t.tparamClause,
          t.bounds,
          " ",
          kw("="),
          " ",
          t.body
        )
      case t: Defn.Class => r(" ")(
          t.mods,
          kw("class"),
          s(t.name, t.tparamClause, w(" ", t.ctor, t.ctor.mods.nonEmpty)),
          t.templ
        )
      case t: Defn.Trait =>
        if (dialect.allowTraitParameters || t.ctor.mods.isEmpty) r(" ")(
          t.mods,
          kw("trait"),
          s(t.name, t.tparamClause, w(" ", t.ctor, t.ctor.mods.nonEmpty)),
          t.templ
        )
        else throw new UnsupportedOperationException(s"$dialect doesn't support trait parameters")

      case t: Defn.GivenAlias =>
        val sig = givenSig(t.name, t.paramClauseGroups)
        r(" ")(t.mods, sig, givenDeclTpe(t.decltpe), kw("="), t.body)
      case t: Defn.Given =>
        val sig = givenSig(t.name, t.paramClauseGroups)
        r(" ")(t.mods, sig, t.templ)

      case t: Defn.Enum => r(" ")(t.mods, kw("enum"), s(t.name, t.tparamClause, t.ctor), t.templ)
      case t: Defn.RepeatedEnumCase => s(w(t.mods, " "), kw("case"), " ", r(t.cases, ", "))
      case t: Defn.EnumCase =>
        def init() = if (t.inits.nonEmpty) s(" extends ", r(t.inits, ", ")) else s("")
        s(w(t.mods, " "), kw("case"), " ", t.name, t.tparamClause, t.ctor, init())

      case t: Defn.ExtensionGroup => s(kw("extension"), " ", o(t.paramClauseGroup, " "), t.body)
      case t: Defn.Object => r(" ")(t.mods, kw("object"), t.name, t.templ)
      case t: Defn.Def =>
        s(w(t.mods, " "), kw("def "), t.name, t.paramClauseGroups, t.decltpe, " = ", t.body)
      case t: Defn.Macro =>
        s(w(t.mods, " "), kw("def "), t.name, t.paramClauseGroups, t.decltpe, " = macro ", t.body)
      case t: Pkg.Body => if (t.stats.isEmpty) s("{}") else s("{", t.stats, n("}"))
      case t: Pkg =>
        val body = if (guessHasBraces(t)) s(" ", t.body) else r(t.body.stats.map(n(_)))
        s(kw("package"), " ", t.ref, body)
      case t: Pkg.Object => r(" ")(kw("package"), t.mods, kw("object"), t.name, t.templ)
      case t: Ctor.Primary =>
        val paramClauses = r(t.paramClauses.map(printParams(_)))
        s(w(t.mods, " ", t.mods.nonEmpty && t.paramClauses.nonEmpty), paramClauses)
      case t: Ctor.Block => if (t.stats.isEmpty) s(t.init) else s("{", i(t.init), "", t.stats, n("}"))
      case t: Ctor.Secondary =>
        s(w(t.mods, " "), kw("def"), " ", kw("this"), t.paramClauses, " = ", t.body)

      // Init
      case t: Init =>
        s(if (t.tpe.is[Type.Singleton]) kw("this") else p(RefineTyp, t.tpe), t.argClauses)

      // Self
      case t: Self => w(s(t.name, t.decltpe), " =>")

      // Template
      case t: Template =>
        val pearly = w(o(t.earlyClause), " with")
        val pparents = r(t.inits, " with ")
        val derived = r("derives ", ", ", "")(t.derives: _*)
        val ptype = t.parent match {
          case Some(p: Defn.Given) =>
            val isNew = givenSigUsesNewSyntax(p.paramClauseGroups)(
              t.inits.forall(_.argClauses.isEmpty) && !t.body.origin.tokensOpt.forall(
                _.rfindWideNot(_.is[Token.Trivia], -1).is[Token.KwWith] // still old syntax
              )
            )
            if (isNew) Decl.Given else Defn.Given
          case Some(_: Term.NewAnonymous) => Term.NewAnonymous
          case _ => null
        }
        def isOldGiven = ptype eq Defn.Given
        def isNewGiven = ptype eq Decl.Given
        val pbody = s(t.body)
        val body =
          if (pbody.isEmpty) t.inits match {
            case x :: Nil if isOldGiven => o("with {}", x.argClauses.isEmpty)
            case _ :: xs if xs.nonEmpty => s()
            case _ => o("{}", (ptype eq Term.NewAnonymous) && pearly.isEmpty || isNewGiven)
          }
          else w("with ", pbody, isOldGiven)
        val noExtends = ptype != null || pparents.isEmpty && pearly.isEmpty
        r(" ")(o("extends", !noExtends), pearly, pparents, derived, body)
      case t: Template.Body =>
        val self = o(t.selfOpt)
        t.stats match {
          case Nil => w("{ ", self, " }")
          case stat :: Nil =>
            val statStr = s(stat).toString
            if (statStr.contains(EOL)) s("{", w(" ", self), i(stat), n("}"))
            else r(" ")("{", self, statStr, "}")
          case stats => s("{", w(" ", self), stats, n("}"))
        }

      // Mod
      case Mod.Annot(init) => s(kw("@"), p(SimpleTyp, init.tpe), init.argClauses)
      case Mod.Private(within) => s(kw("private"), w("[", within, "]"))
      case Mod.Protected(within) => s(kw("protected"), w("[", within, "]"))
      case _: Mod.Implicit => kw("implicit")
      case _: Mod.Final => kw("final")
      case _: Mod.Sealed => kw("sealed")
      case _: Mod.Open => kw("open")
      case _: Mod.Opaque => kw("opaque")
      case _: Mod.Using => kw("using")
      case _: Mod.Erased => kw("erased")
      case _: Mod.Tracked => kw("tracked")
      case _: Mod.Transparent => kw("transparent")
      case _: Mod.Override => kw("override")
      case _: Mod.Case => kw("case")
      case _: Mod.Abstract => kw("abstract")
      case _: Mod.Covariant => kw("+")
      case _: Mod.Contravariant => kw("-")
      case _: Mod.Lazy => kw("lazy")
      case _: Mod.ValParam => kw("val")
      case _: Mod.VarParam => kw("var")
      case _: Mod.Inline =>
        if (!dialect.allowInlineMods)
          throw new UnsupportedOperationException(s"$dialect doesn't support inline modifiers")
        kw("inline")
      case _: Mod.Infix =>
        if (!dialect.allowInfixMods)
          throw new UnsupportedOperationException(s"$dialect doesn't support infix modifiers")
        kw("infix")
      case _: Mod.Into => kw("into")

      // Enumerator
      case t: Enumerator.Val => s(p(Pattern1, t.pat), " = ", p(Expr, t.rhs))
      case t: Enumerator.Generator => s(p(Pattern1, t.pat), " <- ", p(Expr, t.rhs))
      case t: Enumerator.CaseGenerator => s(" case ", p(Pattern1, t.pat), " <- ", p(Expr, t.rhs))
      case t: Enumerator.Guard => s(kw("if"), " ", p(PostfixExpr, t.cond))

      // Import
      case t: Importee.Name => s(t.name)
      case t: Importee.Given => s(kw("given"), " ", t.tpe)
      case t: Importee.GivenAll => s(kw("given"))
      case t: Importee.Rename =>
        if (dialect.allowAsForImportRename) s(t.name, " ", kw("as"), " ", t.rename)
        else s(t.name, " ", kw("=>"), " ", t.rename)
      case t: Importee.Unimport =>
        if (dialect.allowAsForImportRename) s(t.name, " ", kw("as"), " ", kw("_"))
        else s(t.name, " ", kw("=>"), " ", kw("_"))
      case _: Importee.Wildcard => if (dialect.allowStarWildcardImport) kw("*") else kw("_")
      case t: Importer =>
        if (t.ref.isNot[Term.Anonymous]) s(t.ref, ".", t.importees) else s(t.importees)
      case t: Import => s(kw("import"), " ", r(t.importers, ", "))
      case t: Export => s(kw("export"), " ", r(t.importers, ", "))

      // Case
      case t: Term.CasesBlock => s("{", t.cases, n("}"))
      case t: Case =>
        val ppat = p(Pattern, t.pat)
        val pcond = t.cond.fold(s())(cond => s(" ", kw("if"), " ", p(PostfixExpr, cond)))
        def isOneLiner(t: Case) = t.stats match {
          case Nil => true
          case (_: Lit | _: Term.Name) :: Nil => true
          case _ => false
        }
        def allOneLiner = t.parent match {
          case Some(p: Term.CasesBlock) => p.cases.forall(isOneLiner)
          case _ => isOneLiner(t)
        }
        val pbody = t.stats match {
          case stat :: Nil if allOneLiner => s(" ", stat)
          case stats => s(stats)
        }
        s("case ", ppat, pcond, " ", kw("=>"), pbody)

      case t: TypeCase => s("case ", t.pat, " ", kw("=>"), " ", t.body)

      // Source
      case t: Source =>
        val shebang = t.origin.tokensOpt.flatMap { x =>
          val shebangOk = x.length >= 2 && x(0).is[Token.BOF] && x(1).is[Token.Shebang]
          if (shebangOk) Some(x(1).asInstanceOf[Token.Shebang].value) else None
        }
        s(o(shebang, EOL), r(t.stats, EOL))
      case t: MultiSource => r(t.sources, s"$EOL$EOL@$EOL$EOL")

      case t: Stat.Block => if (t.stats.isEmpty) s("{}") else s("{", t.stats, n("}"))

      case t: Tree.Comment => printComment(t)
      // in reality, this shouldn't really be called directly as it might be printed incorrectly
      case t: Tree.Comments =>
        val isTrailing = t.parent.exists(_.endComment.contains(t))
        if (isTrailing) printEndComments(t) else printBegComments(t)
    }

    private def givenCond(pcg: Member.ParamClauseGroup): Show.Result = {
      val clauses = pcg.paramClauses.map(x => s("(", r(x.values.map(printParam(_)), ", "), ")"))
      r("", " => ", " =>")(s(pcg.tparamClause) +: clauses: _*)
    }

    private def givenSigUsesNewSyntax(
        pcgs: List[Member.ParamClauseGroup]
    )(ifEmpty: => Boolean): Boolean = dialect.allowImprovedTypeClassesSyntax &&
      (pcgs match {
        case Nil => ifEmpty
        case pcg :: Nil =>
          val pcs = pcg.paramClauses
          pcs.nonEmpty && pcs.forall(_.mod.isEmpty)
        case _ => true
      })

    private def givenSig(name: meta.Name, pcgs: List[Member.ParamClauseGroup]): Show.Result = {
      val useNewSyntax = givenSigUsesNewSyntax(pcgs)(ifEmpty = true)
      val kwGiven = kw("given")
      def oldSyntax = s(kwGiven, w(" ", s(name, o(pcgs.headOption)), ":"))
      def newSyntax = r(" ")(kwGiven, w(name, ":"), r(pcgs.map(givenCond)))
      s(if (useNewSyntax) newSyntax else oldSyntax)
    }

    private def givenDeclTpe(decltpe: Type): Show.Result = decltpe match {
      case _: Type.TypedParam => w("(", decltpe, ")")
      case _ => p(AnnotTyp, decltpe)
    }

    // Multiples and optionals
    private def printApplyArgs(args: Term.ArgClause, beforeBrace: String): Show.Result =
      args.values match {
        case Seq(b @ (_: Term.Block | _: Term.PartialFunction)) => s(beforeBrace, b)
        case Seq(f: Term.Function) if f.paramClause.mod.isDefined => s(beforeBrace, "{ ", f, " }")
        case _ => s(args)
      }

    implicit def syntaxParamClauseGroups: Syntax[Seq[Member.ParamClauseGroup]] = Syntax(r(_))
    implicit def syntaxArgss: Syntax[Seq[Term.ArgClause]] = Syntax(r(_))
    implicit def syntaxMods: Syntax[Seq[Mod]] = Syntax(r(_, " "))
    private def isUsingOrImplicit(m: Mod): Boolean = m.is[Mod.ParamsType]
    private def printParam(t: Term.Param, keepImplicit: Boolean = false): Show.Result = {
      val mods = if (keepImplicit) t.mods else t.mods.filterNot(isUsingOrImplicit)
      val nameType = if (t.isNameAnonymous) o(t.decltpe) else s(t.name, t.decltpe)
      s(w(mods, " "), nameType, o(" = ", t.default))
    }
    implicit def syntaxAnnots: Syntax[Seq[Mod.Annot]] = Syntax(r(_, " "))
    private def printParams(t: Term.ParamClause, needParens: Boolean = true): Show.Result = {
      val v = t.values
      val (useParens, mod) = v match {
        case head +: tail =>
          val modOpt = t.mod
          val useParens = needParens || tail.nonEmpty ||
            modOpt.fold(head.decltpe.isDefined)(!_.is[Mod.Implicit])
          (useParens, o(modOpt, " "))
        case _ => (true, Show.None)
      }
      w("(", s(mod, r(v.map(printParam(_, mod.isEmpty)), ", ")), ")", useParens)
    }
    implicit def syntaxMemberParamss: Syntax[Seq[Member.ParamClause]] = Syntax(r(_))
    implicit def syntaxTypeOpt: Syntax[Option[Type]] = Syntax(o(kw(": "), _))
    implicit def syntaxImportee: Syntax[Seq[Importee]] = Syntax {
      case Seq(t: Importee.Name) => s(t)
      case Seq(t: Importee.Wildcard) => s(t)
      case Seq(t: Importee.GivenAll) => s(t)
      case Seq(t: Importee.Given) => s(t)
      case Seq(t: Importee.Rename) if dialect.allowAsForImportRename => s(t)
      case Seq(t: Importee.Unimport) if dialect.allowAsForImportRename => s(t)
      case Seq(t: Importee.Rename) => s("{", t, "}")
      case importees => s("{ ", r(importees, ", "), " }")
    }

    implicit def syntaxCases: Syntax[Seq[CaseTree]] = Syntax(cases => r(cases.map(i(_))))

    private def printStats(stats: Seq[Stat]) = r {
      var prevStat: Stat = null
      stats.map { stat =>
        val showStat = i(stat)
        if (showStat.isEmpty) showStat
        else {
          val needNL = null != prevStat && showStat.headChar.contains('{') &&
            guessNeedsLineSep(prevStat)
          prevStat = stat
          s(blank(needNL), showStat)
        }
      }
    }

    private def printFunctionLikeType(
        t: Type.FunctionLikeType,
        params: Show.Result,
        arrow: String
    ): Show.Result = {
      val caps = t.parent match {
        case Some(p: Type.Capturing) if p.tpe eq t => s(p.caps)
        case _ => Show.None
      }
      m(Typ, s(w(params, " "), kw(arrow), caps, " ", p(Typ, t.res)))
    }

    private def printFunctionType(t: Type.ParamFunctionType, arrow: String): Show.Result =
      printFunctionLikeType(t, s(t.paramClause), arrow)

    private def printContextBounds(cbounds: List[Type] = Nil): Show.Result = cbounds match {
      case Nil => s()
      case head :: Nil => s(kw(": "), head)
      case cbs @ x :: _
          if dialect.allowImprovedTypeClassesSyntax &&
            x.origin.tokensOpt.exists(_.rfindWideNot(_.is[Token.Trivia], -1).is[Token.LeftBrace]) =>
        s(kw(": "), "{", r(cbs, ", "), "}")
      case cbs => s(kw(": "), r(cbs, ": "))
    }

    private def printBounds(bounds: Type.Bounds, vbounds: List[Type] = Nil): Show.Result = {
      val lo = o(bounds.lo.map(x => s(" ", kw(">:"), " ", p(Typ, x))))
      val hi = o(bounds.hi.map(x => s(" ", kw("<:"), " ", p(Typ, x))))
      val vb = r(vbounds.map(s(" ", kw("<%"), " ", _)))
      val cb = printContextBounds(bounds.context)
      s(lo, hi, vb, cb)
    }

    private def printByNameType(t: Type.ByNameType, arrow: String): Show.Result =
      printFunctionLikeType(t, s(), arrow)

    private def printMaybeBackquoted(name: Name, backquote: Boolean = false) =
      printBackquoted(name, backquote || guessIsBackquoted(name))
    private def printBackquoted(name: Name, isBackquoted: Boolean) =
      w("`", name.value, "`", isBackquoted)

    private def printSelectLhs(expr: Term, backquote: Boolean = false) = expr match {
      case t: Term.New if t.init.argClauses.isEmpty => w("(", t, ")")
      case t: Term.Name if backquote => withComments(t)(printBackquoted(t, isBackquoted = true))
      case t => p(SimpleExpr, t)
    }

    private def printSelect(t: Term.SelectLike, sep: String, bqExpr: Boolean = false) = {
      val expr = printSelectLhs(t.qual, backquote = bqExpr)
      m(Path, s(expr, sep, t.name))
    }

    private def printMacroExprBody(t: Term, prefix: String) = {
      val body = t match {
        case Term.Block(stat :: Nil) => s("{ ", stat, " }")
        case body => p(Expr, body)
      }
      m(SimpleExpr, s(prefix, body))
    }

    implicit def syntaxStats: Syntax[Seq[Stat]] = Syntax(printStats)

    private def AsTreeSyntax[T <: Tree](f: T => Show.Result): Syntax[T] = new Syntax[T] {
      def apply(tree: T): Show.Result = withComments(tree)(f(tree))
    }

  }

  def apply[T <: Tree](implicit dialect: Dialect): Syntax[T] =
    // NOTE: This is the current state of the art of smart prettyprinting.
    // If we prettyprint a tree that's just been parsed with the same dialect,
    // then we retain formatting. Otherwise, we don't, even in the tiniest.
    // I expect to improve on this in the nearest future, because we had it much better until recently.
    Syntax((x: T) =>
      x.origin match {
        case o: Origin.Parsed if o.dialect.isEquivalentTo(dialect) => s(o.text)
        case _ => reprint(x)
      }
    )

  def reprint[T <: Tree](x: T)(implicit dialect: Dialect): Show.Result = (new SyntaxInstances)
    .syntaxTree[T].apply(x)

  private def printComments(
      tree: Tree,
      f: Tree => Option[Tree.Comments],
      out: Tree.Comments => Show.Result
  ): Show.Result = f(tree) match {
    case Some(tc) // skip if the parent refers to the same comments
        if tc.values.nonEmpty && tree.parent.flatMap(f).forall(ptc =>
          (tc ne ptc) &&
            ((tc.origin, ptc.origin) match {
              case (otc: Origin.Partial, optc: Origin.Partial) =>
                otc.begTokenIdx != optc.begTokenIdx || otc.endTokenIdx != optc.endTokenIdx
              case _ => true
            })
        ) => out(tc)
    case _ => s()
  }

  private def printComment(t: Tree.Comment): Show.Result = r(t.parts.map(_.value))
  private def printBegComments(t: Tree.Comments): Show.Result =
    r(s(), "", n())(t.values.map(x => n(printComment(x))): _*)
  private def printEndComments(t: Tree.Comments): Show.Result =
    r(" ", " ", n())(t.values.map(printComment): _*)

  private[prettyprinters] def withComments(tree: Tree)(syntax: Show.Result): Show.Result = s(
    printComments(tree, _.begComment, printBegComments),
    syntax,
    printComments(tree, _.endComment, printEndComments)
  )
}

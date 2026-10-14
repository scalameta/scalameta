package scala.meta
package internal
package prettyprinters

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.classifiers._
import scala.meta.inputs.Position
import scala.meta.internal.tokens.Chars._
import scala.meta.internal.trees._
import scala.meta.prettyprinters._
import scala.meta.tokens._
import scala.meta.tokens.{Token => T}
import scala.meta.trees.Origin

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

object TreeSyntax {

  import Show.{alt, blank, indent => i, keyword => kw, literal => lit, meta => m, newline => n,
    nosplit => nosp, opt => o, repeat => r, sequence => s, sequenceFiltered => sf, spaceOnly => spc,
    spacen => spn, wrap => w}

  private def collect[T](xs: Seq[T])(f: T => Show.Result): Show.Result = {
    val res = List.newBuilder[Show.Result]
    val iter = xs.iterator
    while (iter.hasNext) {
      val x = f(iter.next())
      if (x ne Show.None) res += x
    }
    sf(res.result(): _*)
  }

  private def sequenceWithLast[T](xs: Seq[T])(f: (T, Boolean) => Show.Result): Show.Result = {
    val res = List.newBuilder[Show.Result]
    val iter = xs.iterator
    while (iter.hasNext) {
      val x = f(iter.next(), !iter.hasNext)
      if (x ne Show.None) res += x
    }
    s(res.result(): _*)
  }

  private final class SyntaxInstances(comments: Boolean)(implicit dialect: Dialect) {

    val keywords = tokenizers.keywords(dialect)
    import TreeSyntacticGroup._

    private val escapableSoftKeywords: Map[String, Seq[Class[_]]] = {
      val seq = Seq.newBuilder[(String, Seq[ClassTag[_]])]

      seq += "|" -> Seq(classTag[Pat.ExtractInfix])

      if (dialect.allowExtensionMethods) seq +=
        "extension" -> Seq(classTag[Term.Apply], classTag[Term.ApplyUsing])

      if (dialect.allowInlineMods) seq +=
        "inline" -> Seq(classTag[Term.Apply], classTag[Term.ApplyUsing], classTag[Term.ApplyInfix])

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

    def p(og: TreeSyntacticGroup, t: Tree, force: Boolean = false) = parens(og, s(t), force)
    private def parens(og: TreeSyntacticGroup, res: Show.Result, force: Boolean = false) = res match {
      case x: Show.Meta =>
        val needParens = force ||
          (x.data match {
            case ig: TreeSyntacticGroup => groupNeedsParens(og, ig)
            case _ => false
          })
        w("(", x, ")", needParens)
      case res => res
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
          '\u0020' <= codepoint && codepoint <= '\u007E' &&
          !(lexicalWhitespace(codepoint) || lexicalLetter(codepoint) || lexicalDigit(codepoint) ||
            lexicalParentheses(codepoint) || lexicalDelimiter(codepoint))

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
        case _: Pat.Bind | _: Pat.Alternative | _: Pat.Tuple => true
        case p: Pat.ArgClause => p.values.contains(t)
        case p: Pat.ExtractInfix => p.lhs eq t
        case p: Pat.Interpolate => p.args.contains(t)
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
          t.value.isEmpty || t.value.head.isLower && patVarIsOk || isEscapableSoftKeyword(t, parent)
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
        case _ => true
      }
      !isOnlyChildOfOnlyChild(t)
    }
    def guessNeedsLineSep(t: Tree): Boolean = t match {
      // a brace after a type, on the next line, refines it even in a dialect with indentation
      case _: Decl.Val | _: Decl.Var | _: Decl.Def | _: Defn.Type => true
      case _: Term.Ref | _: Member.Apply | _: Term.Ascribe | _: Term.Tuple | _: Term.New |
          _: Term.Interpolate | _: Term.Xml | _: Lit => !dialect.allowSignificantIndentation
      case t: Stat.WithTemplate => t.templ.self.isEmpty && t.templ.stats.isEmpty
      case _: Term.ApplyInfix => true
      case _ => false
    }

    // the tree that ends the statement, past the bodies and operands that end it
    @tailrec
    private def lastExpr(t: Tree): Tree = t match {
      case t: Term.ApplyUnary => lastExpr(t.arg)
      case _: Decl.Val | _: Decl.Var | _: Decl.Def | _: Defn.Type | _: Term.Do => t
      case t: Tree.WithBody => lastExpr(t.body)
      case t: Term.ApplyInfix => t.argClause.values match {
          case arg :: Nil => lastExpr(arg)
          case _ => t
        }
      case t: Term.Return => lastExpr(t.expr)
      case t: Term.Throw => lastExpr(t.expr)
      case t: Term.If => lastExpr(if (guessHasElsep(t)) t.elsep else t.thenp)
      case t: Term.Try => t.finallyp match {
          case Some(x) => lastExpr(x)
          case _ => if (t.catchClause.isEmpty) lastExpr(t.expr) else t
        }
      case t: Term.TryWithHandler => lastExpr(t.finallyp.getOrElse(t.catchp))
      case t => t
    }

    // Branches
    private def showTree(t: Tree): Show.Result = t match {
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
      case t: Term.This => m(Path, s(w(t.qual, "."), kw("this")))
      case t: Term.Super => m(Path, s(w(t.thisp, "."), kw("super"), w("[", t.superp, "]")))
      case t: Term.Name => m(Path, printMaybeBackquoted(t))
      case t: Term.CapSetName => m(Path, s(printMaybeBackquoted(t), "^"))
      case t: Term.Select => m(Path, s(printSelectLhs(t.qual), beforeKeyword(t.name, ".")))
      case t: Term.SelectPostfix =>
        val (sep, backquoteExpr) = t.qual match {
          case q: Term.Name // if it looks like a leading infix, don't use space
              if dialect.allowInfixOperatorAfterNL &&
                q.value.isIdentSymbolicNonBackquotedInfixOperator =>
            "" -> t.name.value.isIdentSymbolicNonBackquotedInfixOperator
          case _ => " " -> false
        }
        m(PostfixExpr, s(printSelectLhs(t.qual, backquote = backquoteExpr), sep, t.name))
      case t: Term.Interpolate =>
        m(SimpleExpr1, printInterpolate(t.prefix, t.parts, t.args)(printInterpolateArg))
      case t: Term.Xml =>
        if (!dialect.allowXmlLiterals)
          throw new UnsupportedOperationException(s"$dialect doesn't support xml literals")
        val partsIter = t.parts.iterator.map { case Lit.String(part) => part }
        val partsArgs = printPartsArgs(partsIter, t.args, "xml literal")(printBracedExpr(_))
        m(SimpleExpr1, partsArgs)

      case t: Term.ArgClause => t.parent match {
          // an infix argument drops its parentheses unless its kind needs them
          case Some(ai: Term.ApplyInfix) if (t.values match {
                case (_: Lit.Unit) :: Nil => false
                case (_: Lit | _: Term.Ref | _: Term.Function | _: Term.Apply |
                    _: Term.Placeholder | _: Term.MatchLike | _: Term.ApplyInfix |
                    _: Term.MacroLike | _: Term.If) :: Nil => true
                case _ => false
              }) => p(new InfixExpr(ai), t.values.head)
          case _ => nosp(s("(", o(t.mod, " "), rsep(t.values), ")"))
        }
      case t: Term.Apply => m(SimpleExpr1, s(p(SimpleExpr1, t.fun), printApplyArgs(t.argClause, " ")))
      case t: Term.ApplyUsing =>
        val args = s("(", kw("using"), " ", rsep(t.argClause.values), ")")
        m(SimpleExpr1, s(p(SimpleExpr1, t.fun), nosp(withComments(t.argClause)(args))))
      case t: Term.ApplyType => m(SimpleExpr1, s(p(SimpleExpr, t.fun), t.targClause))
      case t: Term.ApplyInfix =>
        val sg = new InfixExpr(t)
        m(sg, s(p(sg, t.lhs), " ", t.op, t.targClause, " ", printApplyArgs(t.argClause, "")))
      case t: Term.ApplyUnary =>
        val needSpace = !t.op.value.lastOption.forall(isOperatorPart)
        val arg = p(SimpleExpr, t.arg)
        val needParens = t.arg match {
          case _: Lit.Unit => false
          // the parser reads an operator and the literal after it as one literal
          case _: Lit => true
          // a minus joins a number, and operator characters join the operator
          case _ =>
            val elem = Show.firstSyntaxElem(arg)
            (elem ne null) && {
              val c = elem.codePointAt(0)
              isOperatorPart(c) || t.op.value == "-" && (c == '.' || Character.isDigit(c))
            }
        }
        m(PrefixExpr, s(w(t.op, " ", needSpace), w("(", arg, ")", needParens)))
      case t: Term.Assign => m(Expr1, s(p(SimpleExpr1, t.lhs), " ", kw("="), " ", p(Expr, t.rhs)))
      case t: Term.Return =>
        m(Expr1, s(kw("return"), if (guessHasExpr(t)) s(" ", p(Expr, t.expr)) else s()))
      case t: Term.Throw => m(Expr1, s(kw("throw"), " ", p(Expr, t.expr)))
      case t: Term.Ascribe =>
        m(Expr1, s(p(PostfixExpr, t.expr), nosp(s(kw(": "), p(AnyInfixTyp, t.tpe)))))
      case t: Term.Annotate => m(Expr1, s(p(PostfixExpr, t.expr), kw(":"), " ", t.annots))
      case t: Term.Tuple => m(SimpleExpr1, s("(", rsep(t.args), ")"))
      case t: Term.Block =>
        def block(pre: Show.Result, x: Show.Result) = m(SimpleExpr, s("{", w(" ", pre), x, n("}")))
        def bare(stats: List[Stat]) = printStats(stats, indent = false)
        def plain(stats: List[Stat]) = // a commented block must keep its braces for idempotency
          if (!hasComments(t) && isCaseBody(t)) bare(stats) else m(SimpleExpr, printBlock(t, stats))
        t.stats match {
          case stats @ (f: Term.Function) :: Nil => f.body match {
              // a body of one statement keeps its braces: after `x =>`, one statement is no block
              case body: Term.Block if body.stats.lengthCompare(1) != 0 =>
                block(s(f.paramClause, " =>"), body.stats)
              case _ => plain(stats)
            }
          case stats => plain(stats)
        }
      case t: Term.If =>
        val needParens: Boolean = t.thenp match {
          case innerIf: Term.If => !guessHasElsep(innerIf) && guessHasElsep(t)
          case _ => false
        }
        val showThen = p(Expr, t.thenp, force = needParens)
        val showElse =
          if (!guessHasElsep(t)) s()
          else n(beforeKeyword(t.elsep, "else", p(Expr, t.elsep)), comments && breakBeforeElse(t))
        m(Expr1, s(printMods(t), kw("if"), " (", t.cond, ") ", showThen, showElse))
      case t: Term.SelectMatch if dialect.allowMatchAsOperator =>
        val mods = t.mods
        val pref = if (mods.isEmpty) Path else Expr1
        val casesBlock = beforeKeyword(t.casesBlock, ".match")
        m(pref, s(printMods(mods), printSelectLhs(t.expr), casesBlock))
      case t: Term.MatchLike =>
        val casesBlock = beforeKeyword(t.casesBlock, "match")
        m(Expr1, s(printMods(t.mods), p(PostfixExpr, t.expr), casesBlock))
      case t: Term.TryClause =>
        val showExpr = p(Expr, t.expr)
        val showCatch = t.catchClause.fold(s())(beforeKeyword(_, "catch"))
        val showFinally = t.finallyp.fold(s())(beforeKeyword(_, "finally"))
        val needParensAroundExpr = t.expr match {
          case e: Term.TryClause if e.finallyp.isEmpty =>
            if (e.catchClause.isEmpty) !showCatch.isEmpty || !showFinally.isEmpty
            else showCatch.isEmpty && !showFinally.isEmpty
          // a body on lines of its own: the keyword after it must not continue its last line
          case e => comments && (!showCatch.isEmpty || !showFinally.isEmpty) &&
            e.begComment.hasNewlinesBeforeOrAfter
        }
        val showTry = if (needParensAroundExpr) s("(", i(showExpr), n(")")) else showExpr
        m(Expr1, s(kw("try"), " ", showTry, showCatch, showFinally))
      case t: Term.AnonymousFunction => s(t.body)
      case t: Term.FunctionTerm =>
        val arrow = t match {
          case _: Term.Function => "=>"
          case _: Term.ContextFunction => "?=>"
        }
        m(Expr, s(t.paramClause, " ", kw(arrow), " ", p(Expr, t.body)))
      case t: Term.PolyFunction => m(Expr, s(t.tparamClause, " ", kw("=>"), " ", p(Expr, t.body)))
      case t: Term.QuotedMacroType => s("'[ ", t.tpe, spn("]"))
      case t: Term.QuotedMacroExpr => printMacroExprBody(t.body, "'")
      case t: Term.SplicedMacroExpr => printMacroExprBody(t.body, "$")
      case t: Term.SplicedMacroPat => s("${ ", t.pat, spn("}"))
      case t: Pat.Macro => m(SimplePattern, s(t.body))
      case t: Type.Macro => m(SimpleTyp, s(t.body))
      case t: Term.PartialFunction => m(SimpleExpr, s("{", t.cases, n("}")))
      case t: Term.While => m(Expr1, s(kw("while"), " (", t.expr, ") ", p(Expr, t.body)))
      case t: Term.Do =>
        val cond = beforeKeyword(t.expr, "while", t.expr, wrapPre = "(", wrapPost = ")")
        m(Expr1, s(kw("do"), " ", p(Expr, t.body), cond))
      case t: Term.For => m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", t.body))
      case t: Term.ForYield =>
        m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ")", beforeKeyword(t.body, "yield")))
      case t: Term.New => m(SimpleExpr, s(kw("new"), " ", t.init))
      case t: Term.EndMarker => s(kw("end"), " ", t.name.value)
      case t: Term.NewAnonymous => m(SimpleExpr, s(kw("new"), " ", alt(t.templ, "{}")))
      case _: Term.Placeholder => m(SimpleExpr1, kw("_"))
      case t: Term.Eta => m(PostfixExpr, s(p(SimpleExpr1, t.expr), " ", kw("_")))
      case t: Term.Repeated =>
        val varargs = if (dialect.allowPostfixStarVarargSplices) kw("*") else nosp(kw(": _*"))
        s(p(PostfixExpr, t.expr), varargs)
      case t: Term.Param =>
        // NOTE: `implicit/using` in parameters is skipped as it applies to whole list
        printParam(t)
      case t: Term.ParamClause =>
        val v = t.values
        val modOpt = t.mod
        val useParens = v match {
          case head :: tail => tail.nonEmpty || !t.parent.is[Term] ||
            modOpt.fold(head.decltpe.isDefined)(!_.is[Mod.Implicit])
          case _ => true
        }
        val mod = o(modOpt, " ")
        w("(", s(mod, rsepWith(v)(printParam(_, mod.isEmpty))), ")", useParens)

      // Type
      case t: Type.AnonymousName => m(Path, s(""))
      case t: Type.Name => m(Path, printMaybeBackquoted(t))
      case t: Type.CapSetName => m(Path, s(printMaybeBackquoted(t), "^"))
      case t: Type.Select => m(SimpleTyp, s(t.qual, kw("."), t.name))
      case t: Type.Project => m(SimpleTyp, s(p(SimpleTyp, t.qual), nosp(s(kw("#"), t.name))))
      case t: Type.Singleton => m(SimpleTyp, s(p(SimpleExpr1, t.ref), ".", kw("type")))
      case t: Type.ArgClause =>
        def arg(x: Type) = parens(Typ, showTree(x))
        if (t.isEmpty) s() else nosp(s("[", rsepWith(t.values)(arg), "]"))
      case t: Type.Apply => m(SimpleTyp, s(p(SimpleTyp, t.tpe), t.argClause))
      case t: Type.ApplyInfix =>
        val sg = new InfixTyp(t)
        m(sg, s(p(sg, t.lhs), " ", t.op, " ", p(sg, t.rhs)))
      case t: Type.FuncParamClause => t.values match {
          case arg :: Nil if (arg match {
                case _: Type.Tuple | _: Type.ByNameType | _: Type.FunctionParamOrArg |
                    _: Type.ParamFunctionType | _: Type.PolyFunction => false
                case _ => true
              }) => p(AnyInfixTyp, arg)
          case args => s("(", rsep(args), ")")
        }
      case t: Type.Function => printFunctionType(t, "=>")
      case t: Type.ContextFunction => printFunctionType(t, "?=>")
      case t: Type.PureFunction => printFunctionType(t, Token.pureFunctionArrow)
      case t: Type.PureContextFunction => printFunctionType(t, Token.pureContextFunctionArrow)
      case t: Type.Tuple => m(SimpleTyp, s("(", rsep(t.args), ")"))
      case t: Type.With => m(WithTyp, s(p(WithTyp, t.lhs), " with ", p(WithTyp, t.rhs)))
      case t: Type.And => s(Type.ApplyInfix(t.lhs, Type.Name("&"), t.rhs))
      case t: Type.Or => s(Type.ApplyInfix(t.lhs, Type.Name("|"), t.rhs))
      case t: Type.Refine =>
        m(RefineTyp, s(t.tpe.map(tpe => s(p(WithTyp, tpe), " ")).getOrElse(s("")), t.body))
      case t: Type.Existential => m(Typ, s(p(AnyInfixTyp, t.tpe), " ", kw("forSome"), " ", t.body))
      case t: Type.Annotate => m(AnnotTyp, s(p(SimpleTyp, t.tpe), " ", t.annots))
      case t: Type.Lambda => m(Typ, s(t.tparamClause, " ", kw("=>>"), " ", p(Typ, t.tpe)))
      case t: Type.PolyFunction => m(Typ, s(t.tparamClause, " ", kw("=>"), " ", p(Typ, t.tpe)))
      case t: Type.Match => m(Typ, s(p(AnyInfixTyp, t.tpe), " ", kw("match"), " {", t.cases, n("}")))
      case t: Type.AnonymousLambda => s(t.tpe)
      case t: Type.AnonymousParam =>
        val useStar = dialect.allowStarAsTypePlaceholder &&
          t.origin.tokensOpt.forall(!_.lastOption.is[Token.Underscore])
        val ph = if (useStar) "*" else "_"
        m(SimpleTyp, s(o(t.variant), ph))
      case t: Type.Wildcard =>
        /* In order not to break existing tools `.syntax` should still return
         * `_` instead `?` unless specifically used.
         */
        def questionMarkUsed = t.origin.tokensOpt.exists(!_.headOption.is[Token.Underscore])
        val useQM = dialect.allowQuestionMarkAsTypeWildcard &&
          (dialect.allowUnderscoreAsTypePlaceholder || questionMarkUsed)
        m(SimpleTyp, s(kw(if (useQM) "?" else "_"), spn(t.bounds)))
      case t: Type.PatWildcard => m(SimpleTyp, kw("_"))
      case t: Type.Placeholder =>
        /* In order not to break existing tools `.syntax` should still return
         * `_` instead `?` unless specifically used.
         */
        def questionMarkUsed = t.origin.tokensOpt.exists(!_.exists(_.is[Token.Underscore]))
        val useQM = dialect.allowQuestionMarkAsTypeWildcard &&
          (dialect.allowUnderscoreAsTypePlaceholder || questionMarkUsed)
        m(SimpleTyp, s(kw(if (useQM) "?" else "_"), spn(t.bounds)))
      case t: Type.Bounds =>
        val res = Seq.newBuilder[Show.Result]
        def add(str: String)(tpe: Type): Unit = res += " " += kw(str) += " " += p(Typ, tpe)
        t.lo.foreach(add(">:"))
        t.hi.foreach(add("<:"))
        t.view.foreach(add("<%"))
        t.context match {
          case cbs @ x :: xs =>
            val bounds =
              if (xs.isEmpty) s(x)
              else if (dialect.allowImprovedTypeClassesSyntax &&
                x.origin.tokensOpt.exists(_.rfindWideNotOrNull(_.is[T.Trivia], -1).is[T.LeftBrace]))
                s("{", rsep(cbs), "}")
              else r(cbs, ": ")
            res += nosp(s(kw(": "), bounds))
          case _ =>
        }
        s(res.result(): _*)
      case t: Type.Repeated => t.tpe match {
          case tByName: Type.ByName => m(ParamTyp, s(kw("=>"), " ", p(Typ, tByName.tpe), kw("*")))
          case _ => m(ParamTyp, s(p(Typ, t.tpe), kw("*")))
        }
      case t: Type.ByName => printByNameType(t, "=>")
      case t: Type.PureByName => printByNameType(t, Token.pureFunctionArrow)
      case t: Type.Var => m(SimpleTyp, s(t.name.value))
      case t: Type.FunctionArg => m(ParamTyp, s(w(t.mods, " "), p(Typ, t.tpe)))
      case t: Type.TypedParam => m(SimpleTyp, s(w(t.mods, " "), t.name.value, ": ", p(Typ, t.typ)))
      case t: Type.Assign => m(SimpleTyp, s(t.name.value, " ", kw("="), " ", p(Typ, t.rhs)))
      case t: Type.ParamClause => if (t.isEmpty) s() else s("[", rsep(t.values), "]")
      case t: Type.BoundsAlias => m(SimpleTyp, s(t.bounds, " ", "as", " ", t.name))
      case t: Type.Param =>
        def isVariant(m: Mod) = m.is[Mod.Variant]
        val mods = t.mods.filterNot(isVariant)
        val variance = o(t.mods.find(isVariant))
        s(w(mods, " "), variance, t.name, t.tparamClause, t.bounds)
      case t: Type.Block => s(w(r(t.typeDefs, "; "), "; "), t.tpe)
      case _: Type.CapturesAny => s()
      case t: Type.CapturesSet => s("{", rsep(t.values), "}")
      case t: Type.Capturing => t.tpe match {
          case tpe: Type.FunctionLikeType => s(tpe)
          case tpe => s(tpe, "^", t.caps)
        }

      // Pat
      case t: Pat.Var => m(SimplePattern, t.name)
      case _: Pat.Wildcard => m(SimplePattern, kw("_"))
      case a: Pat.Assign => m(SimplePattern, s(a.name, " ", kw("="), " ", a.rhs))
      case _: Pat.SeqWildcard => m(SimplePattern, kw("_*"))
      case t: Pat.Repeated => m(SimplePattern, s(t.name, kw("*")))
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
      case t: Pat.Alternative =>
        val indent = !comments || !isBarAfterDetachedBar(t)
        m(Pattern, s(p(Pattern, t.lhs), beforeKeyword(t.rhs, "|", p(Pattern, t.rhs), indent = indent)))
      case t: Pat.Tuple => m(SimplePattern, s("(", rsep(t.args), ")"))
      case t: Pat.ArgClause => m(SimplePattern, s("(", rsep(t.values), ")"))
      case t: Pat.Extract => m(SimplePattern, s(t.fun, t.argClause))
      case t: Pat.ExtractInfix =>
        val pop = new InfixPat(t)
        val rhs = t.argClause match {
          case Pat.ArgClause(pat :: Nil) if !pat.is[Lit.Unit] => s(p(pop, pat))
          case pats => s(pats)
        }
        m(pop, s(p(pop, t.lhs), " ", t.op, " ", rhs))
      case t: Pat.Interpolate =>
        m(SimplePattern, printInterpolate(t.prefix, t.parts, t.args)(arg => printBracedExpr(arg)))
      case t: Pat.Xml =>
        if (!dialect.allowXmlLiterals)
          throw new UnsupportedOperationException(s"$dialect doesn't support xml literals")
        val partsIter = t.parts.iterator.map { case Lit.String(part) => part }
        val partsArgs = printPartsArgs(partsIter, t.args, "xml literal")(printBracedExpr(_))
        m(SimplePattern, partsArgs)
      case Pat.Typed(lhs, rhs: Lit) =>
        m(Pattern1, s(p(SimplePattern, lhs), kw(":"), " ", p(Literal, rhs)))
      case t: Pat.Typed =>
        m(Pattern1, s(p(SimplePattern, t.lhs), nosp(s(kw(": "), p(RefineTyp, t.rhs)))))

      // Lit
      case Lit.Boolean(value) => m(Literal, s(value.toString))
      case Lit.Byte(value) => m(
          Literal,
          s(
            "ByteLiterals.",
            if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value,
          ),
        )
      case Lit.Short(value) => m(
          Literal,
          s(
            "ShortLiterals.",
            if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value,
          ),
        )
      case Lit.Int(value) => m(Literal, s(value.toString))
      case Lit.Long(value) => m(Literal, s(value.toString + "L"))
      case t: Lit.Float =>
        val format = t.format
        w(s(format), "f", Character.toLowerCase(format.last) != 'f')
      case t: Lit.Double =>
        val format = t.format
        val suffix =
          if (Character.toLowerCase(format.last) == 'd') ""
          else if (t.origin.textOpt.exists(x => Character.toLowerCase(x.last) == 'd')) "d"
          else if (format.forall(c => Character.isDigit(c) || c == '-')) ".0"
          else ""
        s(format, suffix)
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
      case t: Decl.Val => s(printMods(t), kw("val "), printPats(t), nosp(s(kw(": "), t.decltpe)))
      case t: Decl.Var => s(printMods(t), kw("var "), printPats(t), nosp(s(kw(": "), t.decltpe)))
      case t: Decl.Type => s(printMods(t), kw("type "), t.name, t.tparamClause, t.bounds)
      case t: Decl.Def =>
        s(printMods(t), kw("def "), t.name, t.paramClauseGroups, nosp(s(kw(": "), t.decltpe)))
      case t: Decl.GivenLike =>
        val sig = givenSig(t.name, t.paramClauseGroups)
        s(printMods(t), sig, spn(), givenDeclTpe(t.decltpe))
      case t: Defn.Val => s(printMods(t), kw("val "), printPats(t), nosp(t.decltpe), " = ", t.rhs)
      case t: Defn.Var => s(printMods(t), kw("var "), printPats(t), nosp(t.decltpe), " = ", t.body)
      case t: Defn.Type =>
        s(printMods(t), kw("type "), t.name, t.tparamClause, t.bounds, " = ", t.body)
      case t: Defn.Class => s(
          printMods(t),
          r(" ")(
            kw("class"),
            s(t.name, t.tparamClause, w(" ", t.ctor, t.ctor.mods.nonEmpty)),
            t.templ,
          ),
        )
      case t: Defn.Trait =>
        if (dialect.allowTraitParameters || t.ctor.mods.isEmpty) s(
          printMods(t),
          r(" ")(
            kw("trait"),
            s(t.name, t.tparamClause, w(" ", t.ctor, t.ctor.mods.nonEmpty)),
            t.templ,
          ),
        )
        else throw new UnsupportedOperationException(s"$dialect doesn't support trait parameters")

      case t: Defn.GivenAlias =>
        val sig = givenSig(t.name, t.paramClauseGroups)
        s(printMods(t), sig, spn(), givenDeclTpe(t.decltpe), kw(" = "), t.body)
      case t: Defn.Given =>
        val sig = givenSig(t.name, t.paramClauseGroups)
        s(printMods(t), sig, spn(), t.templ)

      case t: Defn.Enum =>
        s(printMods(t), r(" ")(kw("enum"), s(t.name, t.tparamClause, t.ctor), t.templ))
      case t: Defn.RepeatedEnumCase => s(printMods(t), kw("case"), " ", rsep(t.cases))
      case t: Defn.EnumCase =>
        def init() = if (t.inits.nonEmpty) s(" extends ", rsep(t.inits)) else s("")
        s(printMods(t), kw("case"), " ", t.name, t.tparamClause, t.ctor, init())

      case t: Defn.ExtensionGroup => s(kw("extension"), " ", o(t.paramClauseGroup), " ", t.body)
      case t: Defn.Object => s(printMods(t), r(" ")(kw("object"), t.name, t.templ))
      case t: Defn.Def =>
        s(printMods(t), kw("def "), t.name, t.paramClauseGroups, nosp(t.decltpe), " = ", t.body)
      case t: Defn.Macro =>
        val pcg = t.paramClauseGroups
        val body = beforeKeyword(t.body, "macro")
        s(printMods(t), kw("def "), t.name, pcg, nosp(t.decltpe), " = ", body)
      case t: Pkg.Body => printBlock(t, t.stats)
      case t: Pkg =>
        val body = if (guessHasBraces(t)) s(" ", t.body) else r(t.body.stats.map(n(_)))
        s(kw("package"), " ", t.ref, body)
      case t: Pkg.Object => r(" ")(kw("package"), t.mods, kw("object"), t.name, t.templ)
      case t: Ctor.Primary =>
        s(w(t.mods, " ", t.mods.nonEmpty && t.paramClauses.nonEmpty), s(t.paramClauses: _*))
      case t: Ctor.Block => if (t.stats.isEmpty) s(t.init) else s("{", i(t.init), "", t.stats, n("}"))
      case t: Ctor.Secondary =>
        s(printMods(t), kw("def "), kw("this"), t.paramClauses, " = ", t.body)

      // Init
      case t: Init =>
        val tpe = t.tpe match {
          case Type.Singleton(ref @ Term.This(_: Name.Anonymous)) => ref
          case tpe => tpe
        }
        s(p(RefineTyp, tpe), t.argClauses)

      // Self
      case t: Self => w(s(t.name, t.decltpe.fold(s())(x => s(kw(": "), p(AnyInfixTyp, x)))), " =>")

      // Template
      case t: Template =>
        def bk(kw: String, res: Show.Result = Show.None)(x: Tree) =
          beforeKeyword(x, kw, if (res.isEmpty) s(x) else res, indent = true)
        val allParents = t.earlyClause match {
          case Some(x) => x :: t.inits
          case None => t.inits
        }
        val pparents = allParents match {
          case x :: xs => s(x, r(xs.map(bk("with"))))
          case _ => s()
        }
        val derived = t.derives match {
          case xs @ x :: _ => beforeKeyword(x, "derives", rsep(xs))
          case _ => s()
        }
        val ptype = templateParentType(t)
        def isOldGiven = ptype eq Defn.Given
        def isNewGiven = ptype eq Decl.Given
        val hasBodyComments = isOldGiven && ownBegComments(t.body).isDefined
        // record the body now: it prints before `with`, which prints its comments
        if (hasBodyComments) addKeywordOwner(t.body)
        val pbody = s(t.body)
        val body =
          if (pbody.isEmpty && !hasBodyComments) t.inits match {
            case x :: Nil if isOldGiven => o("with {}", x.argClauses.isEmpty)
            case _ :: xs if xs.nonEmpty => s()
            case _ => o("{}", (ptype eq Term.NewAnonymous) && t.earlyClause.isEmpty || isNewGiven)
          }
          else if (isOldGiven) bk("with", alt(pbody, s("{}")))(t.body)
          else pbody
        val noExtends = ptype != null || allParents.isEmpty
        val rest = r(" ")(pparents, derived, body)
        if (noExtends) rest else beforeKeyword(allParents.head, "extends", rest, owner = t)
      case t: Template.Body =>
        val self = o(t.selfOpt)
        t.stats match {
          case Nil => w("{ ", self, spn("}"))
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
        // a line break after `as` ends the importee, but after `=>` it does not
        val asOK = dialect.allowAsForImportRename && !hasBreaksAroundBegComments(t.rename)
        s(t.name, " ", kw(if (asOK) "as" else "=>"), spn(t.rename))
      case t: Importee.Unimport =>
        val arrow = if (dialect.allowAsForImportRename) "as" else "=>"
        s(t.name, " ", kw(arrow), " ", kw("_"))
      case _: Importee.Wildcard => if (dialect.allowStarWildcardImport) kw("*") else kw("_")
      case t: Importer =>
        val clause = t.clause match {
          case imp: Importee
              if dialect.allowAsForImportRename || !imp.isAny[Importee.Rename, Importee.Unimport] =>
            s(imp)
          case imp: Importee => printImporteeClause(imp :: Nil)
          case clause => s(clause)
        }
        if (t.ref.is[Term.Anonymous]) clause else s(t.ref, ".", clause)
      case t: Importee.Clause => printImporteeClause(t.values)
      case t: Import => s(kw("import"), " ", rsep(t.importers))
      case t: Export => s(kw("export"), " ", rsep(t.importers))

      // Case
      case t: Term.CasesBlock => s("{", t.cases, n("}"))
      case t: Case =>
        val ppat = p(Pattern, t.pat)
        val pcond = t.cond.fold(s())(x => beforeKeyword(x, "if", p(PostfixExpr, x)))
        val prest = t.body match {
          // Scala 3 sub-cases: the body is a `match` in the guard, printed as
          // `case p [if cond] if e match ...`, with no `=>`.
          case b: Term.SubMatch => s(kw("if"), " ", b)
          case b =>
            def isAttached(x: Tree) = !comments || !x.endComment.hasNewlinesBeforeLast
            def isOneLiner(body: Tree) = body match {
              case _: Lit | _: Term.Name | Term.Block(Nil) => isAttached(body)
              case _ => false
            }
            val body = caseBodyStat(b)
            def allOneLiner = t.parent match {
              case Some(p: Term.CasesBlock) => p.cases.forall(x => isOneLiner(caseBodyStat(x.body)))
              case _ => isOneLiner(body)
            }
            // a detached comment after the case would follow a body on the arrow's line
            val useSpace = (body.is[Term.Block] || isAttached(t)) && allOneLiner
            s(kw("=>"), if (useSpace) w(" ", body) else i(body))
        }
        s("case ", ppat, pcond, " ", prest)

      case t: TypeCase => s("case ", t.pat, " ", kw("=>"), " ", t.body)

      // Source
      case t: Source =>
        val shebang = t.origin.tokensOpt.flatMap { x =>
          val shebangOk = x.length >= 2 && x(0).is[Token.BOF] && x(1).is[Token.Shebang]
          if (shebangOk) Some(x(1).asInstanceOf[Token.Shebang].value) else None
        }
        // a trailing comment group may end in a blank line, which `n` does not double
        val stats = t.stats match {
          case head :: tail => s(head, r(tail.map(n(_))))
          case Nil => s()
        }
        s(o(shebang, EOL), stats)
      case t: MultiSource => r(t.sources, s"$EOL$EOL@$EOL$EOL")

      case t: Stat.Block => printBlock(t, t.stats)

      case t: Tree.Comment => printComment(t)
      // in reality, this shouldn't really be called directly as it might be printed incorrectly
      case t: Tree.Comments =>
        val isTrailing = t.parent.exists(_.endComment.contains(t))
        if (isTrailing) printEndComments(t) else printBegComments(t)
    }

    private def givenCond(pcg: Member.ParamClauseGroup): Show.Result =
      r("", " => ", " =>")(pcg.tparamClause +: pcg.paramClauses: _*)

    private def givenSigUsesNewSyntax(
        pcgs: List[Member.ParamClauseGroup],
    )(ifEmpty: => Boolean): Boolean = dialect.allowImprovedTypeClassesSyntax &&
      (pcgs match {
        case Nil => ifEmpty
        case pcg :: Nil =>
          val pcs = pcg.paramClauses
          pcs.nonEmpty && pcs.forall(_.mod.isEmpty)
        case _ => true
      })

    private def templateParentType(t: Template): AnyRef = t.parent match {
      case Some(p: Defn.Given) =>
        val isNew = givenSigUsesNewSyntax(p.paramClauseGroups)(
          t.inits.forall(_.argClauses.isEmpty) && !t.body.origin.tokensOpt.forall(
            _.rfindWideNotOrNull(_.is[Token.Trivia], -1).is[Token.KwWith], // still old syntax
          ),
        )
        if (isNew) Decl.Given else Defn.Given
      case Some(_: Term.NewAnonymous) => Term.NewAnonymous
      case _ => null
    }

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
        case Seq(f: Term.Function) if f.paramClause.mod.isDefined => s(beforeBrace, "{ ", f, spn("}"))
        case _ => s(args)
      }

    private def printImporteeClause(imps: List[Importee]): Show.Result = {
      def getLine(x: Importee): Int = x.pos.startLine
      val oneline = {
        val lines = imps.iterator.map(getLine).filter(_ >= 0)
        !lines.hasNext || {
          val line = lines.next()
          lines.forall(_ == line)
        }
      }
      if (oneline) {
        def lspace: Boolean = {
          val imp = imps.head
          getLine(imp) >= 0 && {
            val tokens = imp.tokens
            val idx = tokens.rskipWideIf(_.is[Token.Trivia], -1)
            idx < -1 && tokens.getWideOrNull(idx).is[Token.LeftBrace]
          }
        }
        // the comments that the last selector prints end with a space of their own
        def rspace: Boolean = {
          val imp = imps.last
          getLine(imp) >= 0 && ownEndComments(imp).isEmpty && {
            val tokens = imp.tokens
            val idx = tokens.skipWideIf(_.is[Token.Trivia], tokens.length)
            idx > tokens.length && tokens.getWideOrNull(idx).is[Token.RightBrace]
          }
        }
        val space = o(" ", lspace || rspace)
        s("{", space, r(imps, ", "), space, "}")
      } else {
        // the comma precedes the trailing comments of the selector
        val res =
          sequenceWithLast(imps)((x, isLast) => i(reprint(x, after = if (isLast) s() else s(","))))
        s("{", res, n("}"))
      }
    }

    implicit def syntaxParamClauseGroups: Syntax[Seq[Member.ParamClauseGroup]] = Syntax(r(_))
    implicit def syntaxArgss: Syntax[Seq[Term.ArgClause]] = Syntax(r(_))
    implicit def syntaxMods: Syntax[Seq[Mod]] = Syntax(r(_, " "))
    // each modifier, then a space that a newline drops, so that what follows keeps their column
    private def printMods(mods: Seq[Mod]): Show.Result = r(mods.map(s(_, spn())))
    private def printMods(t: Stat.WithMods): Show.Result = printMods(t.mods)
    private def isUsingOrImplicit(m: Mod): Boolean = m.is[Mod.ParamsType]
    private def printParam(t: Term.Param, keepImplicit: Boolean = false): Show.Result = {
      val mods = if (keepImplicit) t.mods else t.mods.filterNot(isUsingOrImplicit)
      val nameType = if (t.isNameAnonymous) o(t.decltpe) else s(t.name, nosp(t.decltpe))
      s(w(mods, " "), nameType, o(kw(" = "), t.default))
    }
    // a typed pattern keeps its parens: the type after them is the declared one
    private def printPats(pats: List[Pat]): Show.Result =
      rsepWith(pats)(x => parens(Pattern2, showTree(x)))
    private def printPats(t: Tree.WithPats): Show.Result = printPats(t.pats)
    private def printBlock(t: Tree, stats: List[Stat]): Show.Result =
      if (stats.isEmpty) s("{}") else s("{", stats, n("}"))
    implicit def syntaxAnnots: Syntax[Seq[Mod.Annot]] = Syntax(r(_, " "))
    // a term clause after a name does not split from it
    implicit def syntaxMemberParamss: Syntax[Seq[Member.ParamClause]] = Syntax(xs =>
      s(xs.map {
        case x: Term.ParamClause => nosp(x)
        case x => s(x)
      }: _*),
    )
    implicit def syntaxTypeOpt: Syntax[Option[Type]] = Syntax(o(kw(": "), _))

    implicit def syntaxCases: Syntax[Seq[CaseTree]] = Syntax(cases => r(cases.map(i(_))))

    @tailrec
    // the tree that the text of `t` starts with
    private def leftmost(t: Tree): Tree = (t match {
      case t: Term.SelectLike => t.qual
      case t: Term.MatchLike => t.expr
      case t: Member.Apply => t.fun
      case t: Member.Infix => t.lhs
      case t: Term.Ascribe => t.expr
      case t: Term.Assign => t.lhs
      case t: Term.Eta => t.expr
      case _ => null
    }) match {
      case null => t
      case child => leftmost(child)
    }

    /* A thenp on lines of its own: `else` starts a line, or it would continue the thenp. An
     * `else` that starts a line in the chain after it needs each `else` before it to start one. */
    @tailrec
    private def breakBeforeElse(t: Term.If): Boolean = endsInBodyOnOwnLine(t.thenp) ||
      (t.elsep match {
        case e: Term.If => guessHasElsep(e) && breakBeforeElse(e)
        case _ => false
      })

    // the tree ends in a body printed on its own line after a comment; a block closes it
    @tailrec
    private def endsInBodyOnOwnLine(t: Tree): Boolean = t.begComment.hasNewlinesBeforeOrAfter ||
      !t.is[Term.Block] &&
      (t.lastChild match {
        case null => false
        case last => endsInBodyOnOwnLine(last)
      })

    // a case body of one term prints as the term, as its block prints without braces
    private def caseBodyStat(body: Term): Term = body match {
      case Term.Block((stat: Term) :: Nil) if (stat match {
            case f: Term.Function if f.body.isInstanceOf[Term.Block] => false // keep lambda braces
            case _ => !hasComments(body)
          }) => stat
      case _ => body
    }

    private def isCaseBody(t: Tree): Boolean = t.parent match {
      case Some(p: Case) => p.body eq t
      case _ => false
    }

    private def printStats(stats: Seq[Stat], indent: Boolean = true) = r {
      var prevStat: Stat = null
      stats.map { stat =>
        val showStat = if (indent) i(stat) else n(stat)
        if (showStat.isEmpty) showStat
        else {
          // the parser would read the statement as part of the one before it
          val needNL = null != prevStat && {
            val lastPrevExpr = lastExpr(prevStat)
            lastPrevExpr.is[Term.SelectPostfix] || {
              val elem = Show.firstSyntaxElem(showStat)
              if (elem eq null) false
              else if (elem.head == '{' || elem.head == '(') guessNeedsLineSep(lastPrevExpr)
              else dialect.allowInfixOperatorAfterNL && {
                elem == "$" || // quasi
                elem.startsWith("..") || // ellipsis
                // the parser would read the name as an operator that continues the line before
                elem.isIdentSymbolicInfixOperator &&
                (leftmost(stat) match {
                  case n: Term.Name => n.parent.exists {
                      case p: Term.ApplyInfix if p.lhs eq n =>
                        p.op match {
                          case _: Quasi => true
                          case opn =>
                            val op = opn.value
                            op.isUnaryOp || !op.isIdentSymbolicInfixOperator
                        }
                      case _ => false
                    }
                  case _ => false
                })
              }
            }
          }
          prevStat = stat
          s(blank(needNL), showStat)
        }
      }
    }

    private def printFunctionLikeType(
        t: Type.FunctionLikeType,
        params: Show.Result,
        arrow: String,
    ): Show.Result = {
      val caps = t.parent match {
        case Some(p: Type.Capturing) if p.tpe eq t => s(p.caps)
        case _ => Show.None
      }
      m(Typ, s(w(params, " "), kw(arrow), caps, spn(p(Typ, t.res))))
    }

    private def printFunctionType(t: Type.ParamFunctionType, arrow: String): Show.Result =
      printFunctionLikeType(t, s(t.paramClause), arrow)

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

    private def printMacroExprBody(t: Term, prefix: String) = {
      val body = t match {
        case Term.Block(stat :: Nil) => s("{ ", stat, spn("}"))
        case body => p(Expr, body)
      }
      m(SimpleExpr, s(prefix, body))
    }

    private def printBracedExpr(t: Show.Result, useBraces: Boolean = true): Show.Result =
      w("{", t, "}", useBraces)
    private def printBracedExpr(t: Term): Show.Result = printBracedExpr(p(Expr, t), !t.is[Term.Block])
    // a block of one expression keeps its braces on one line
    private def printInterpolateArg(t: Term): Show.Result = t match {
      case b @ Term.Block((stat: Term) :: Nil) if !hasComments(b) => printBracedExpr(p(Expr, stat))
      case _ => printBracedExpr(t)
    }

    private def printInterpolate[A <: Tree](prefix: Term.Name, parts: List[Lit], args: Seq[A])(
        argPrinter: A => Show.Result,
    ): Show.Result = {

      /** @see LegacyScanner.getStringPart, when ch == '$' */
      def needBraces(id: String, nextPart: String): Boolean =
        !Character.isUnicodeIdentifierStart(id.head) ||
          nextPart.headOption.exists(Character.isUnicodeIdentifierPart)
      var needQuote = false
      val partsIter = parts.iterator.map { case Lit.String(part) =>
        needQuote ||= part.contains("\n") || part.contains("\r") || part.contains("\"")
        part.replace("$", "$$")
      }.buffered
      val partsArgs = printPartsArgs(partsIter, args, "interpolate") {
        case id: Name if !guessIsBackquoted(id) && !needBraces(id.value, partsIter.head) =>
          s("$", id.value)
        case arg => s("$", argPrinter(arg))
      }
      val quote = if (needQuote) "\"\"\"" else "\""
      s(prefix, quote, partsArgs, quote)
    }

    private def printPartsArgs[A <: Tree](partsIter: Iterator[String], args: Seq[A], what: String)(
        f: A => Show.Result,
    ): Show.Result = {
      val argsIter = args.iterator
      val res = Seq.newBuilder[Show.Result]
      while (partsIter.hasNext) {
        val part = partsIter.next()
        val notLast = partsIter.hasNext
        require(notLast == argsIter.hasNext, s"$what has more parts than args")
        res += { if (notLast) s(lit(part), f(argsIter.next())) else lit(part) }
      }
      s(res.result(): _*)
    }

    implicit def syntaxStats: Syntax[Seq[Stat]] = Syntax(printStats(_))

    def reprint(tree: Tree, after: Show.Result = Show.None): Show.Result =
      reprintWith(tree, after = after)(showTree)

    def reprintWith[T <: Tree](tree: T, after: Show.Result = Show.None, indents: Int = 1)(
        f: T => Show.Result,
    ): Show.Result = withComments(tree, after = after, indents = indents)(f(tree))

    // the trees whose leading comments `beforeKeyword` prints around its keyword
    private var keywordOwners: java.util.Set[Tree] = _
    private def addKeywordOwner(tree: Tree): Unit = {
      if (keywordOwners eq null) keywordOwners = java.util.Collections
        .newSetFromMap(new java.util.IdentityHashMap[Tree, java.lang.Boolean])
      keywordOwners.add(tree)
    }
    private def hasKeywordOwner(tree: Tree): Boolean =
      (keywordOwners ne null) && keywordOwners.contains(tree)

    private def beforeKeyword(body: Tree, keyword: String): Show.Result =
      beforeKeyword(body, keyword, reprint(body))

    // the keyword follows the detached comments of the block after it, on the next line
    private def beforeKeyword(
        body: Tree,
        keyword: String,
        bodyRes: => Show.Result,
        wrapPre: Show.Result = Show.None,
        wrapPost: Show.Result = Show.None,
        indent: Boolean = false,
        owner: Tree = null, // holds the comments, if not the body
    ): Show.Result = {
      def withKeyword(bodyPre: Show.Result) =
        s(kw(keyword), o(" ", keyword.last != '.'), s(wrapPre, bodyPre, bodyRes, wrapPost))
      // a continuation of the same clause, such as `with`, indents when it starts a line
      def afterPrev(bodyPre: Show.Result = Show.None) =
        s(if (keyword.head == '.') s() else if (indent) " " else spn(), withKeyword(bodyPre))
      val holder = if (owner eq null) body else owner
      val begComments = ownBegComments(holder)
      // record the holder before the body prints, so that the body leaves its comments to us
      if (begComments.isDefined) addKeywordOwner(holder)
      begComments match {
        case None => afterPrev()
        case Some(c) =>
          val (before, after) = commentsBeforeKeyword(c, body)
          if (before.isEmpty) afterPrev(printLeading(after, c.newlinesBefore))
          else {
            val newlines = after.headOption.fold(0)(countNewlinesBefore)
            val res = withKeyword(printLeading(after, newlines))
            i(s(newlinesBefore(c, force = true), printComments(before), res), indent)
          }
      }
    }

    def ownBegComments(tree: Tree): Option[Tree.Comments] =
      if (comments) TreeSyntax.ownBegComments(tree) else None
    def ownEndComments(tree: Tree): Option[Tree.Comments] =
      if (comments) TreeSyntax.ownEndComments(tree) else None

    def hasBreaksAroundBegComments(tree: Tree): Boolean = ownBegComments(tree)
      .hasNewlinesBeforeOrAfter
    def hasBreaksAroundEndComments(tree: Tree): Boolean = ownEndComments(tree)
      .hasNewlinesBeforeOrAfter

    def hasComments(tree: Tree): Boolean = comments && tree.hasComments
    def withComments(tree: Tree, after: Show.Result = Show.None, indents: Int = 1)(
        res: Show.Result,
    ): Show.Result =
      if (comments) {
        def afterKW = hasKeywordOwner(tree)
        TreeSyntax.withComments(tree, after = after, indents = indents, afterKeyword = afterKW)(res)
      } else s(res, after)

    private def rsep(xs: Seq[Tree], sep: String = ","): Show.Result = rsepWith(xs, sep)(showTree)

    // the separator follows the element, and precedes its trailing comments if they end the line
    private def rsepWith[T <: Tree](xs: Seq[T], sep: String = ",")(f: T => Show.Result) =
      sequenceWithLast(xs)((x, isLast) =>
        if (isLast) reprintWith(x)(f)
        else if (comments && x.endComment.hasNewlinesAfter)
          s(reprintWith(x, after = s(sep), indents = 2)(f), " ")
        else s(reprintWith(x)(f), sep, " "),
      )

    implicit def syntaxTree[T <: Tree]: Syntax[T] = Syntax[T](reprint(_))

  }

  def syntax(x: Tree, comments: Boolean = true, useOriginal: Boolean = true)(implicit
      dialect: Dialect,
  ): Show.Result = x.origin match {
    case o: Origin.Parsed if useOriginal && o.dialect.isEquivalentTo(dialect) =>
      original(x, o, comments = comments)
    case _ => reprint(x, comments = comments)
  }

  def original(x: Tree, o: Origin.Parsed, comments: Boolean = true): Show.Result = w(
    printBegComments(x, tc => sf(n(), tc.text, n())),
    x.pos.text,
    printEndComments(x, tc => sf(" ", tc.text, n())),
    comments && o.begTokenIdx > 0 && !x.is[Source],
  )
  def reprint(x: Tree, comments: Boolean = true)(implicit dialect: Dialect): Show.Result =
    new SyntaxInstances(comments = comments).reprint(x)

  /* the comments a tree prints itself: the parent prints the ones it refers to as well, and
   * its group holds the child's comments when it extends the child's group */
  private def ownComments(tree: Tree)(f: Tree => Option[Tree.Comments]): Option[Tree.Comments] =
    f(tree).filterNot(tc =>
      tree.parent.flatMap(f).exists(ptc => (tc eq ptc) || tokensOverlap(tc.origin, ptc.origin)),
    )

  // the bar before already indents this one if it starts its own line
  private def isBarAfterDetachedBar(t: Pat.Alternative): Boolean = t.parent match {
    case Some(x: Pat.Alternative) => (x.rhs eq t) && lastEndComment(x.lhs).hasNewlinesAfter
    case _ => false
  }

  @tailrec
  private def lastEndComment(tree: Tree): Option[Tree.Comments] = tree.endComment match {
    case None =>
      val child = tree.lastChild
      if (child ne null) lastEndComment(child) else None
    case x => x
  }

  private def tokensOverlap(a: Origin, b: Origin): Boolean = (a, b) match {
    case (a: Origin.Partial, b: Origin.Partial) => a.begTokenIdx < b.endTokenIdx &&
      b.begTokenIdx < a.endTokenIdx
    case _ => false
  }

  private implicit class XtensionOptionComments(private val self: Option[Tree.Comments])
      extends AnyVal {
    final def hasNewlinesBeforeLast: Boolean = self match {
      case None => false
      case Some(xs) => xs.hasNewlinesBeforeLast
    }
    final def hasNewlinesAfter: Boolean = self match {
      case None => false
      case Some(xs) => xs.hasNewlinesAfter
    }
    final def hasNewlinesBeforeOrAfter: Boolean = self match {
      case None => false
      case Some(xs) => xs.hasNewlinesBeforeOrAfter
    }
  }

  private def ownBegComments(tree: Tree): Option[Tree.Comments] = ownComments(tree)(_.begComment)

  /* how many comments of the group come before the keyword that introduces the tree; without an
   * origin, a detached group goes before the keyword and an attached one after it */
  private def commentsBeforeKeyword(
      c: Tree.Comments,
      tree: Tree,
  ): (List[Tree.Comment], List[Tree.Comment]) = tree.origin match {
    case o: Origin.Parsed =>
      val tokens = o.allInputTokens()
      var kwIdx = o.begTokenIdx - 1 // the `(` of a do-while follows the keyword
      while (kwIdx >= 0 && tokens(kwIdx).isAny[T.Trivia, T.LeftParen]) kwIdx -= 1
      var rest = c.values
      val before = List.newBuilder[Tree.Comment]
      while (rest match {
          case head :: tail if (head.origin match {
                case x: Origin.Parsed => (x.source eq o.source) && x.endTokenIdx <= kwIdx
                case _ => false
              }) =>
            before += head
            rest = tail
            true
          case _ => false
        }) {}
      (before.result(), rest)
    case _ => if (c.isDetached) (c.values, Nil) else (Nil, c.values)
  }

  private def countNewlinesBefore(x: Tree.Comment): Int = x.origin match {
    case o: Origin.Parsed => Tokens.countOf(o.allInputTokens().newlinesBefore(o.begTokenIdx))
    case _ => 0
  }
  private def ownEndComments(tree: Tree): Option[Tree.Comments] = ownComments(tree)(_.endComment)

  private def printBegComments(tree: Tree, out: Tree.Comments => Show.Result): Show.Result =
    ownBegComments(tree).fold(s())(out)
  private def printEndComments(tree: Tree, out: Tree.Comments => Show.Result): Show.Result =
    ownEndComments(tree).fold(s())(out)

  private def printComment(
      t: Tree.Comment,
      pre: Show.Result = spn(),
      post: Show.Result = spc,
  ): Show.Result = s(pre, Show.Comment(r(t.parts.map(_.value))), post)
  private def printComments(t: Tree.Comments): Show.Result = printComments(t.values)
  private def printComments(xs: List[Tree.Comment]): Show.Result =
    collect(xs)(x => printComment(x, post = printNewlines(x.getNewlinesAfter, spn())))
  private def printBegComments(t: Tree.Comments): Show.Result =
    s(newlinesBefore(t, force = true), printComments(t))
  private def printEndComments(t: Tree.Comments): Show.Result =
    s(newlinesBefore(t, force = false), printComments(t))
  /* code follows: the comments on the tree's last line stay there, and the rest go under it, so
   * that they stay the tree's when parsed again; a blank line after them keeps them anyway */
  private def printEndCommentsUnder(t: Tree.Comments, indents: Int): Show.Result = {
    val onLine =
      if (t.isDetached) 0
      else t.values.indexWhere(_.hasNewlinesAfter) match {
        case -1 => t.values.length
        case n => n + 1
      }
    val (line, under) = t.values.splitAt(onLine)
    if (under.isEmpty || under.last.hasBlankAfter) printEndComments(t)
    else {
      val blankBefore = line.lastOption.fold(t.hasBlankBefore)(_.hasBlankAfter)
      val res = s(blank(blankBefore), printComments(under))
      val indented = indents match {
        case 0 => n(res)
        case 1 => i(res)
        case _ => i(i(res))
      }
      s(s(line.map(printComment(_)): _*), indented)
    }
  }
  private def newlinesBefore(t: Tree.Comments, force: Boolean): Show.Result =
    printNewlines(t.newlinesBefore.max(if (force) 1 else 0), s())
  private def printNewlines(count: Int, none: Show.Result): Show.Result =
    if (count > 1) blank() else if (count > 0) n() else none
  // the structure printer keeps one comment per line
  private def printBegCommentsPlain(t: Tree.Comments): Show.Result =
    s(s(t.values.map(x => printComment(x, pre = n(), post = s())): _*), n())
  private def printEndCommentsPlain(t: Tree.Comments): Show.Result =
    s(s(t.values.map(printComment(_)): _*), n())

  // the leading comments `xs` of the tree that follows, with `newlines` before them
  private def printLeading(xs: List[Tree.Comment], newlines: Int): Show.Result =
    if (xs.isEmpty) s()
    else if (newlines > 0) s(printNewlines(newlines, s()), printComments(xs))
    /* attached comments stay on the line that introduces the tree, and the tree
     * indents under a line comment; at the start of a line there is nothing to
     * indent under, and the tree follows at the same indentation */
    else Show.LeadingComments(printComments(xs), breakAtLineStart = xs.hasNewlinesAfter)

  private[prettyprinters] def withComments(
      tree: Tree,
      layout: Boolean = true,
      after: Show.Result = Show.None,
      indents: Int = 1, // the levels that the comments under the tree indent
      afterKeyword: => Boolean = false,
  )(syntax: Show.Result): Show.Result = s(
    ownBegComments(tree) match {
      case None => syntax
      case Some(c) =>
        if (!layout) s(printBegCommentsPlain(c), syntax)
        else if (afterKeyword) syntax
        else s(printLeading(c.values, c.newlinesBefore), syntax)
    },
    after,
    printEndComments(
      tree,
      if (!layout) printEndCommentsPlain
      else { c =>
        // what follows a modifier keeps its column, unless its tree continues after the comments
        if (tree.is[Mod])
          i(printEndComments(c), c.newlinesBefore > 0 && !tree.parent.forall(_.is[Stat]))
        else if (tree.parent.forall {
            case p: Pat.Alternative => (p.lhs ne tree) || isBarAfterDetachedBar(p)
            case p => p.lastChild eq tree
          }) printEndComments(c)
        else printEndCommentsUnder(c, indents)
      },
    ),
  )

}

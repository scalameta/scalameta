package scala.meta
package internal
package prettyprinters

import scala.meta.classifiers._
import scala.meta.inputs.Position
import scala.meta.internal.trees.{branch => _, root => _, _}
import scala.meta.internal.tokenizers.Chars._
import scala.meta.prettyprinters._
import scala.meta.tokens.Token

import org.scalameta.adt._
import org.scalameta.invariants._
import org.scalameta.unreachable

import scala.compat.Platform.EOL

import Show.{
  function => fn,
  indent => i,
  meta => m,
  newline => n,
  opt => o,
  repeat => r,
  sequence => s,
  wrap => w
}

object TreeSyntax {
  private final object SyntaxInstances {
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
        @leaf object RefineTyp extends Type { def precedence = 3 }
        @leaf object WithTyp extends Type { def precedence = 3.5 }
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
      @leaf object Literal extends Term with Pat with Type {
        override def categories = List("Term", "Pat", "Type"); def precedence = 6
      }
      require(
        Literal.precedence == Term.SimpleExpr1.precedence && Literal.precedence == Pat.SimplePattern.precedence
      )
      @leaf object Path extends Type with Term with Pat {
        override def categories = List("Type", "Term", "Pat"); def precedence = 6
      }
      require(
        Path.precedence == Type.SimpleTyp.precedence && Path.precedence == Term.SimpleExpr1.precedence && Path.precedence == Pat.SimplePattern.precedence
      )
    }
  }
  private final class SyntaxInstances(dialect: Dialect) {
    val keywords = tokenizers.keywords(dialect)
    import SyntaxInstances.SyntacticGroup
    import SyntacticGroup.Type._, SyntacticGroup.Term._, SyntacticGroup.Pat._,
      SyntacticGroup.Literal, SyntacticGroup.Path

    def p(
        og: SyntacticGroup,
        t: Tree,
        left: Boolean = false,
        right: Boolean = false,
        force: Boolean = false
    ) = {
      def opNeedsParens(oo: String, io: String, customPrecedence: Boolean = true): Boolean = {
        def precedence(op: String) = if (customPrecedence) op.precedence else 0
        require(left != right)
        val (ol, il) = (oo.isLeftAssoc, io.isLeftAssoc)
        if (ol ^ il) true
        else {
          val (l, r) = (ol, !ol)
          val (op, ip) = (precedence(oo), precedence(io))
          if (op < ip) r
          else if (op > ip) l
          else l ^ left
        }
      }
      def groupNeedsParens(og: SyntacticGroup, ig: SyntacticGroup): Boolean = {
        require(og.categories.intersect(ig.categories).nonEmpty)
        (og, ig) match {
          case (InfixExpr(oo), InfixExpr(io)) => opNeedsParens(oo, io)
          case (InfixTyp(oo), InfixTyp(io)) =>
            opNeedsParens(oo, io, customPrecedence = dialect.useInfixTypePrecedence)
          case (Pattern3(oo), Pattern3(io)) => opNeedsParens(oo, io)
          case _ => og.precedence > ig.precedence
        }
      }
      s(t) match {
        case Show.Meta(ig: SyntacticGroup, res) if force || groupNeedsParens(og, ig) =>
          s("(", res, ")")
        case res => res
      }
    }

    def kw(keyword: String) =
      fn(sb => {
        val prelast = if (sb.length > 1) sb.charAt(sb.length - 2) else ' '
        val last = if (sb.nonEmpty) sb.charAt(sb.length - 1) else ' '
        val next = if (keyword.nonEmpty) keyword(0) else ' '
        val danger = {
          val opThenOp = isOperatorPart(last) && isOperatorPart(next)
          val underscoreThenOp = isIdentifierPart(prelast) && last == '_' && isOperatorPart(next)
          opThenOp || underscoreThenOp
        }
        if (danger) s(" " + keyword) else s(keyword)
      })

    def guessIsBackquoted(t: Name): Boolean = {
      def cantBeWrittenWithoutBackquotes(t: Name): Boolean = {
        // Fold over codepoints for a given string
        def foldCodepoints[T](value: String, start: T)(f: (Int, T, Int) => T): T = {
          val length = value.length
          @annotation.tailrec
          def work(offset: Int, acc: T): T = {
            if (offset >= length) acc
            else {
              val codepoint = value.codePointAt(offset)
              work(offset + Character.charCount(codepoint), f(offset, acc, codepoint))
            }
          }
          work(0, start)
        }
        // These rules are transcribed from
        // https://github.com/scala/scala/blob/2.13.x/spec/01-lexical-syntax.md#lexical-syntax
        def lexicalWhitespace(codepoint: Int): Boolean =
          Set[Int]('\u0020', '\u0009', '\u000D', '\u000A').contains(codepoint)
        def lexicalLetter(codepoint: Int): Boolean = (
          Set[Int]('\u0024', '\u005F').contains(codepoint)
            || Set[Int](
              Character.LOWERCASE_LETTER,
              Character.UPPERCASE_LETTER,
              Character.TITLECASE_LETTER,
              Character.OTHER_LETTER,
              Character.LETTER_NUMBER
            ).contains(Character.getType(codepoint))
        )
        def lexicalDigit(codepoint: Int): Boolean =
          Set[Int]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').contains(codepoint)
        def lexicalParentheses(codepoint: Int): Boolean =
          Set[Int]('(', ')', '[', ']', '{', '}').contains(codepoint)
        def lexicalDelimiter(codepoint: Int): Boolean =
          Set[Int]('`', '\'', '"', '.', ';', ',').contains(codepoint)
        def lexicalOperator(codepoint: Int): Boolean = (
          '\u0020' <= codepoint &&
            codepoint <= '\u007E' &&
            (!lexicalWhitespace(codepoint)
              && !lexicalLetter(codepoint)
              && !lexicalDigit(codepoint)
              && !lexicalParentheses(codepoint)
              && !lexicalDelimiter(codepoint)) ||
            Set[Int](Character.MATH_SYMBOL, Character.OTHER_SYMBOL)
              .contains(Character.getType(codepoint))
        )

        sealed trait OperatorState
        case object Accepted extends OperatorState
        case object Required extends OperatorState
        case object Forbidden extends OperatorState

        sealed trait ValidityState
        case object Valid extends ValidityState
        case object Invalid extends ValidityState

        def validPlainid(string: String): Boolean = {
          val (_, validity) =
            foldCodepoints[(OperatorState, ValidityState)](string, (Accepted, Valid))({
              // Any invalid state is invalid
              case (offset, (_, Invalid), _) => (Forbidden, Invalid)
              // Must start with either a letter or an operator
              case (offset @ 0, (Accepted, Valid), next) if lexicalLetter(next) =>
                (Forbidden, Valid)
              case (offset @ 0, (Accepted, Valid), next) if lexicalOperator(next) =>
                (Required, Valid)
              // Non-leading underscores reset operator validity
              case (offset, (Forbidden, Valid), next) if next == '_' => (Accepted, Valid)
              // Non-leading operators are accepted only after underscores
              case (offset, (Accepted, Valid), next) if lexicalOperator(next) => (Required, Valid)
              // Operators must not be followed by non-operators
              case (offset, (Required, Valid), next) if lexicalOperator(next) => (Required, Valid)
              // Lexical letters and digits can follow underscores
              case (offset, (Accepted, Valid), next) if lexicalLetter(next) => (Forbidden, Valid)
              case (offset, (Accepted, Valid), next) if lexicalDigit(next) => (Forbidden, Valid)
              // Non-operators must not be followed by operators
              case (offset, (Forbidden, Valid), next) if lexicalLetter(next) => (Forbidden, Valid)
              case (offset, (Forbidden, Valid), next) if lexicalDigit(next) => (Forbidden, Valid)
              // Bail on anything not matched here
              case (_, (_, _), next) => (Forbidden, Invalid)
            })

          validity == Valid
        }

        t.value.nonEmpty && (keywords.contains(t.value) ||
          t.value.contains("//") || t.value.contains("/*") || t.value.contains("*/") ||
          !validPlainid(t.value) || lexicalDigit(t.value.codePointAt(0)))
      }
      def isAmbiguousWithPatVarTerm(t: Term.Name, p: Tree): Boolean = {
        val looksLikePatVar = t.value.head.isLower && t.value.head.isLetter
        val thisLocationAlsoAcceptsPatVars = p match {
          case p: Term.Name => unreachable
          case p: Term.Select => false
          case p: Pat.Wildcard => unreachable
          case p: Pat.Var => false
          case p: Pat.Repeated => false
          case p: Pat.Bind => true
          case p: Pat.Alternative => true
          case p: Pat.ArgClause => p.values.contains(t)
          case p: Pat.Tuple => true
          case p: Pat.Extract => false
          case p: Pat.ExtractInfix => p.lhs eq t
          case p: Pat.Interpolate => p.args.contains(t)
          case p: Pat.Typed => unreachable
          case p: Pat => unreachable
          case p: Case => p.pat eq t
          case p: Defn.Val => p.pats.contains(t)
          case p: Defn.Var => p.pats.contains(t)
          case p: Enumerator.Generator => p.pat eq t
          case p: Enumerator.Val => p.pat eq t
          case _ => false
        }
        looksLikePatVar && thisLocationAlsoAcceptsPatVars
      }
      /* Soft keywords might need to be written with backquotes in some places.
       * Previously used match clause fails due to:
       * https://github.com/scala-native/scala-native/issues/2187
       * instead we went with if clause to work around the issue.
       */
      def isEscapableSoftKeyword(t: Name, parent: Tree): Boolean = {
        if (t.value == "extension" && dialect.allowExtensionMethods)
          parent.is[Term.Apply] || parent.is[Term.ApplyUsing]
        else if (t.value == "inline" && dialect.allowInlineMods)
          parent.is[Term.Apply] || parent.is[Term.ApplyUsing] || parent.is[Term.ApplyInfix]
        else if (t.value == "*" && dialect.allowStarWildcardImport)
          parent.is[Importee]
        else
          false
      }

      def isAmbiguousInParent(t: Tree, parent: Tree): Boolean = {
        t match {
          case t: Term.Name =>
            isAmbiguousWithPatVarTerm(t, parent) || isEscapableSoftKeyword(t, parent)
          case t: Name =>
            isEscapableSoftKeyword(t, parent)
          case _ =>
            false
        }
      }
      cantBeWrittenWithoutBackquotes(t) || t.parent.exists(isAmbiguousInParent(t, _))
    }
    def guessIsPostfix(t: Term.Select): Boolean = false
    def guessHasExpr(t: Term.Return): Boolean = t.expr match {
      case Lit.Unit() => false; case _ => true
    }
    def guessHasElsep(t: Term.If): Boolean = t.elsep match {
      case Lit.Unit() => false; case e => true
    }
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
    @annotation.tailrec
    def guessNeedsLineSep(t: Tree): Boolean = t match {
      case t: Term.ApplyUnary => guessNeedsLineSep(t.arg)
      case _: Decl.Val | _: Decl.Var | _: Decl.Def | _: Defn.Type | _: Term.Ref | _: Member.Apply |
          _: Term.Ascribe | _: Term.Tuple | _: Term.New | _: Term.Interpolate | _: Term.Xml |
          _: Lit =>
        !dialect.allowSignificantIndentation
      case t: Term.Do => false
      case t: Tree.WithBody => guessNeedsLineSep(t.body)
      case t: Stat.WithTemplate => t.templ.self.isEmpty && t.templ.stats.isEmpty
      case t: Term.ApplyInfix =>
        val args = t.argClause.values
        args.lengthCompare(1) != 0 || guessNeedsLineSep(args.head)
      case t: Term.Return => guessNeedsLineSep(t.expr)
      case t: Term.Throw => guessNeedsLineSep(t.expr)
      case t: Term.If => guessNeedsLineSep(if (guessHasElsep(t)) t.elsep else t.thenp)
      case t: Term.Try =>
        t.finallyp match {
          case Some(x) => guessNeedsLineSep(x)
          case _ => t.catchp.isEmpty && guessNeedsLineSep(t.expr)
        }
      case t: Term.TryWithHandler => guessNeedsLineSep(t.finallyp.getOrElse(t.catchp))
      case _ => false
    }

    // Branches
    implicit def syntaxTree[T <: Tree]: Syntax[T] = Syntax {
      // Bottom
      case t: Quasi =>
        if (!dialect.allowUnquotes)
          throw new UnsupportedOperationException(s"$dialect doesn't support unquoting")
        if (t.rank > 0) {
          s("." * (t.rank + 1), w("{", t.tree, "}", !t.tree.is[Quasi]))
        } else {
          val allowBraceless =
            t.tree.is[Term.Name] ||
              t.tree.is[Pat.Var] ||
              t.tree.is[Term.This] ||
              t.tree.is[Pat.Wildcard]
          implicit val syntaxDialect = dialect.unquoteVariant()
          s("$", w("{", t.tree.syntax, "}", !allowBraceless))
        }

      // Name
      case _: Name.This => s("this")
      case _: Name.Anonymous => s()
      case _: Name.Placeholder => s("_")
      case _: Term.Anonymous => s("")
      case t: Name.Indeterminate => w("`", t.value, "`", guessIsBackquoted(t))

      // Term
      case t: Term.This =>
        m(Path, w(t.qual, "."), kw("this"))
      case t: Term.Super =>
        m(Path, s(w(t.thisp, "."), kw("super"), w("[", t.superp, "]")))
      case t: Term.Name =>
        m(Path, w("`", t.value, "`", guessIsBackquoted(t)))
      case t: Term.Select =>
        t.qual match {
          case Term.New(Init(_, _, Nil)) =>
            m(Path, s("(", t.qual, ")", if (guessIsPostfix(t)) " " else ".", t.name))
          case _ => m(Path, s(p(SimpleExpr, t.qual), if (guessIsPostfix(t)) " " else ".", t.name))
        }
      case t: Term.Interpolate =>
        /** @see LegacyScanner.getStringPart, when ch == '$' */
        def needBraces(id: String, nextPart: String): Boolean =
          !Character.isUnicodeIdentifierStart(id.head) ||
            nextPart.headOption.exists(Character.isUnicodeIdentifierPart)

        val parts = t.parts.map { case Lit(part: String) => part.replace("$", "$$") }
        val zipped = parts.zip(t.args).zip(parts.tail).map {
          case ((part, id: Name), next) if !guessIsBackquoted(id) && !needBraces(id.value, next) =>
            s(part, "$", id.value)
          case ((part, arg), _) => s(part, "${", p(Expr, arg), "}")
        }
        val quote = if (parts.exists(s => s.contains("\n") || s.contains("\""))) "\"\"\"" else "\""
        m(SimpleExpr1, s(t.prefix, quote, r(zipped), parts.last, quote))
      case t: Term.Xml =>
        if (!dialect.allowXmlLiterals)
          throw new UnsupportedOperationException(s"$dialect doesn't support xml literals")
        val parts = t.parts.map { case Lit(part: String) => part }
        val zipped = parts.zip(t.args).map { case (part, arg) => s(part, "{", p(Expr, arg), "}") }
        m(SimpleExpr1, s(r(zipped), parts.last))

      case t: Term.ArgClause => s("(", o(t.mod, " "), r(t.values, ", "), ")")
      case t: Term.Apply =>
        m(SimpleExpr1, s(p(SimpleExpr1, t.fun), printApplyArgs(t.argClause, " ")))
      case t: Term.ApplyUsing =>
        val args = s("(", kw("using"), " ", r(t.args, ", "), ")")
        m(SimpleExpr1, s(p(SimpleExpr1, t.fun), args))
      case t: Term.ApplyType => m(SimpleExpr1, s(p(SimpleExpr, t.fun), t.targClause))
      case t: Term.ApplyInfix =>
        val args = t.argClause.values match {
          case (arg: Term) :: Nil if (arg match {
                case _: Term.AnonymousFunction | _: Lit.Unit => false
                case _: Lit | _: Term.Ref | _: Term.Function | _: Term.If | _: Term.Match |
                    _: Term.ApplyInfix | _: Term.QuotedMacroExpr | _: Term.SplicedMacroExpr |
                    _: Term.SplicedMacroPat | _: Term.Apply | _: Term.Placeholder =>
                  true
                case _ =>
                  false
              }) =>
            p(InfixExpr(t.op.value), arg, right = true)
          case _ => printApplyArgs(t.argClause, "")
        }

        m(
          InfixExpr(t.op.value),
          s(p(InfixExpr(t.op.value), t.lhs, left = true), " ", t.op, t.targClause, " ", args)
        )
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
          val res =
            if (pre == Show.None && x.isEmpty) s("{}")
            else s("{", w(" ", pre), x, n("}"))
          m(SimpleExpr, res)
        }
        t match {
          case Block(
                Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, Block(stats)) :: Nil
              ) if mods.exists(_.is[Mod.Implicit]) =>
            block(s("implicit ", name, o(": ", tptopt), " =>"), stats)
          case Block(
                Function(Term.Param(_, name: Name, None, _) :: Nil, Block(stats)) :: Nil
              ) =>
            block(s(name, " =>"), stats)
          case Block(Function(params, Block(stats)) :: Nil) =>
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
      case t: Term.Match =>
        m(
          Expr1,
          s(
            w(t.mods, " "),
            p(PostfixExpr, t.expr),
            " ",
            kw("match"),
            " {",
            t.cases,
            n("}")
          )
        )
      case t: Term.Try =>
        m(
          Expr1,
          s(
            kw("try"),
            " ",
            p(Expr, t.expr),
            if (t.catchp.nonEmpty) s(" ", kw("catch"), " {", t.catchp, n("}"))
            else s(""),
            t.finallyp
              .map { finallyp => s(" ", kw("finally"), " ", finallyp) }
              .getOrElse(s())
          )
        )
      case t: Term.TryWithHandler =>
        m(
          Expr1,
          s(
            kw("try"),
            " ",
            p(Expr, t.expr),
            " ",
            kw("catch"),
            " ",
            t.catchp,
            t.finallyp
              .map { finallyp => s(" ", kw("finally"), " ", finallyp) }
              .getOrElse(s())
          )
        )
      case t: Term.FunctionTerm =>
        val arrow = t match {
          case _: Term.Function => "=>"
          case _: Term.ContextFunction => "?=>"
        }
        val paramsSyntax = printParams(t.paramClause, needParens = false)
        m(Expr, s(paramsSyntax, " ", kw(arrow), " ", p(Expr, t.body)))
      case t: Term.PolyFunction =>
        m(Expr, t.tparamClause, " ", kw("=>"), " ", p(Expr, t.body))
      case Term.QuotedMacroExpr(Term.Block(stats)) =>
        stats match {
          case head :: Nil => s("'{ ", head, " }")
          case other => s("'{", other, n("}"))
        }
      case t: Term.QuotedMacroExpr =>
        m(SimpleExpr, s("'", p(Expr, t.body)))
      case t: Term.QuotedMacroType =>
        s("'[ ", t.tpe, " ]")
      case Term.SplicedMacroExpr(Term.Block(stats)) =>
        stats match {
          case head :: Nil => s("${ ", head, " }")
          case other => s("${", other, n("}"))
        }
      case Term.SplicedMacroPat(pat) =>
        s("${ ", pat, " }")
      case t: Term.SplicedMacroExpr =>
        m(SimpleExpr, s("$", p(Expr, t.body)))
      case t: Pat.Macro => m(SimplePattern, s(t.body))
      case t: Type.Macro => m(SimpleTyp, s(t.body))
      case t: Term.PartialFunction => m(SimpleExpr, s("{", t.cases, n("}")))
      case t: Term.While =>
        m(Expr1, s(kw("while"), " (", t.expr, ") ", p(Expr, t.body)))
      case t: Term.Do =>
        m(Expr1, s(kw("do"), " ", p(Expr, t.body), " ", kw("while"), " (", t.expr, ")"))
      case t: Term.For =>
        m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", t.body))
      case t: Term.ForYield =>
        m(Expr1, s(kw("for"), " (", r(t.enums, "; "), ") ", kw("yield"), " ", t.body))
      case t: Term.New => m(SimpleExpr, s(kw("new"), " ", t.init))
      case t: Term.EndMarker => s(kw("end"), " ", t.name.value)
      case t: Term.NewAnonymous =>
        val needsExplicitBraces = t.templ.early.isEmpty && t.templ.inits.lengthCompare(2) < 0 &&
          t.templ.self.isEmpty && t.templ.stats.isEmpty
        m(SimpleExpr, s(kw("new"), " ", t.templ), w(" {", "", "}", needsExplicitBraces))
      case _: Term.Placeholder => m(SimpleExpr1, kw("_"))
      case t: Term.Eta => m(PostfixExpr, s(p(SimpleExpr1, t.expr), " ", kw("_")))
      case t: Term.Repeated =>
        if (dialect.allowPostfixStarVarargSplices)
          s(p(PostfixExpr, t.expr), kw("*"))
        else
          s(p(PostfixExpr, t.expr), kw(":"), " ", kw("_*"))
      case t: Term.Param =>
        // NOTE: `implicit/using` in parameters is skipped as it applies to whole list
        printParam(t)
      case t: Term.ParamClause =>
        printParams(t, needParens = !t.parent.exists(_.is[Term]))

      // Type
      case t: Type.AnonymousName => m(Path, s(""))
      case t: Type.Name => m(Path, if (guessIsBackquoted(t)) s("`", t.value, "`") else s(t.value))
      case t: Type.Select => m(SimpleTyp, s(t.qual, kw("."), t.name))
      case t: Type.Project => m(SimpleTyp, s(p(SimpleTyp, t.qual), kw("#"), t.name))
      case t: Type.Singleton => m(SimpleTyp, s(p(SimpleExpr1, t.ref), ".", kw("type")))
      case t: Type.ArgClause => r(t.values.map(arg => p(Typ, arg)), "[", ", ", "]")
      case t: Type.Apply =>
        m(SimpleTyp, s(p(SimpleTyp, t.tpe), t.argClause))
      case t: Type.ApplyInfix =>
        m(
          InfixTyp(t.op.value),
          s(
            p(InfixTyp(t.op.value), t.lhs, left = true),
            " ",
            t.op,
            " ",
            p(InfixTyp(t.op.value), t.rhs, right = true)
          )
        )
      case t: Type.FuncParamClause =>
        t.values match {
          case arg :: Nil if (arg match {
                case _: Type.Tuple | _: Type.ByName | _: Type.FunctionParamOrArg |
                    _: Type.Function =>
                  false
                case _ => true
              }) =>
            s(arg)
          case args => s("(", r(args, ", "), ")")
        }
      case t: Type.Function => m(Typ, s(t.paramClause, " ", kw("=>"), " ", p(Typ, t.res)))
      case t: Type.ContextFunction => m(Typ, s(t.paramClause, " ", kw("?=>"), " ", p(Typ, t.res)))
      case t: Type.Tuple => m(SimpleTyp, s("(", r(t.args, ", "), ")"))
      case t: Type.With => m(WithTyp, s(p(WithTyp, t.lhs), " with ", p(WithTyp, t.rhs)))
      case t: Type.And =>
        if (!dialect.allowAndTypes)
          throw new UnsupportedOperationException(s"$dialect doesn't support and types")
        m(
          InfixTyp("&"),
          s(
            p(InfixTyp("&"), t.lhs, left = true),
            " ",
            "&",
            " ",
            p(InfixTyp("&"), t.rhs, right = true)
          )
        )
      case t: Type.Or =>
        if (!dialect.allowOrTypes)
          throw new UnsupportedOperationException(s"$dialect doesn't support or types")
        m(
          InfixTyp("|"),
          s(
            p(InfixTyp("|"), t.lhs, left = true),
            " ",
            "|",
            " ",
            p(InfixTyp("|"), t.rhs, right = true)
          )
        )
      case t: Type.Refine =>
        m(
          RefineTyp,
          t.tpe.map(tpe => s(p(WithTyp, tpe), " ")).getOrElse(s("")),
          "{",
          w(" ", r(t.stats, "; "), " ", t.stats.nonEmpty),
          "}"
        )
      case t: Type.Existential =>
        m(Typ, s(p(AnyInfixTyp, t.tpe), " ", kw("forSome"), " { ", r(t.stats, "; "), " }"))
      case t: Type.Annotate => m(AnnotTyp, s(p(SimpleTyp, t.tpe), " ", t.annots))
      case t: Type.Lambda => m(Typ, t.tparamClause, " ", kw("=>>"), " ", p(Typ, t.tpe))
      case t: Type.PolyFunction => m(Typ, t.tparamClause, " ", kw("=>"), " ", p(Typ, t.tpe))
      case t: Type.Match =>
        m(
          Type,
          s(p(AnyInfixTyp, t.tpe), " ", kw("match"), " {", t.cases, n("}"))
        )
      case t: Type.AnonymousLambda => s(t.tpe)
      case t: Type.AnonymousParam =>
        val useStar = dialect.allowStarAsTypePlaceholder && (t.origin match {
          case o: Origin.Parsed => !o.tokens.lastOption.exists(_.is[Token.Underscore])
          case _ => false
        })
        val ph = if (useStar) "*" else "_"
        m(SimpleTyp, o(t.variant), ph)
      case t: Type.Wildcard =>
        /* In order not to break existing tools `.syntax` should still return
         * `_` instead `?` unless specifically used.
         */
        def questionMarkUsed = t.origin match {
          case o: Origin.Parsed => !o.tokens.headOption.exists(_.is[Token.Underscore])
          case _ => false
        }
        val useQM = dialect.allowQuestionMarkAsTypeWildcard &&
          (dialect.allowUnderscoreAsTypePlaceholder || questionMarkUsed)
        m(SimpleTyp, s(kw(if (useQM) "?" else "_")), t.bounds)
      case t: Type.PatWildcard =>
        m(SimpleTyp, kw("_"))
      case t: Type.Placeholder =>
        /* In order not to break existing tools `.syntax` should still return
         * `_` instead `?` unless specifically used.
         */
        def questionMarkUsed = t.origin match {
          case o: Origin.Parsed => !o.tokens.exists(_.is[Token.Underscore])
          case _ => false
        }
        val useQM = dialect.allowQuestionMarkAsTypeWildcard &&
          (dialect.allowUnderscoreAsTypePlaceholder || questionMarkUsed)
        m(SimpleTyp, s(kw(if (useQM) "?" else "_"), t.bounds))
      case t: Type.Bounds =>
        s(
          t.lo.map(lo => s(" ", kw(">:"), " ", p(Typ, lo))).getOrElse(s()),
          t.hi.map(hi => s(" ", kw("<:"), " ", p(Typ, hi))).getOrElse(s())
        )
      case t: Type.Repeated =>
        t.tpe match {
          case tByName: Type.ByName =>
            m(ParamTyp, s(kw("=>"), " ", p(Typ, tByName.tpe), kw("*")))
          case _ =>
            m(ParamTyp, s(p(Typ, t.tpe), kw("*")))
        }
      case t: Type.ByName => m(ParamTyp, s(kw("=>"), " ", p(Typ, t.tpe)))
      case t: Type.Var => m(SimpleTyp, s(t.name.value))
      case t: Type.FunctionArg => m(ParamTyp, w(t.mods, " "), p(Typ, t.tpe))
      case t: Type.TypedParam => m(SimpleTyp, w(t.mods, " "), s(t.name.value), ": ", p(Typ, t.typ))
      case t: Type.ParamClause => r(t.values, "[", ", ", "]")
      case t: Type.Param =>
        def isVariant(m: Mod) = m.is[Mod.Variant]
        val mods = t.mods.filterNot(isVariant)
        require(t.mods.length - mods.length <= 1)
        val variance = o(t.mods.find(isVariant))
        val tbounds = s(t.tbounds)
        val vbounds = {
          if (t.vbounds.nonEmpty && !dialect.allowViewBounds)
            throw new UnsupportedOperationException(s"$dialect doesn't support view bounds")
          r(t.vbounds.map { s(" ", kw("<%"), " ", _) })
        }
        val cbounds = r(t.cbounds.map { s(kw(":"), " ", _) })
        s(w(mods, " "), variance, t.name, t.tparamClause, tbounds, vbounds, cbounds)

      // Pat
      case t: Pat.Var =>
        m(SimplePattern, s(if (guessIsBackquoted(t.name)) s"`${t.name.value}`" else t.name.value))
      case _: Pat.Wildcard => m(SimplePattern, kw("_"))
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
          case _ =>
            s(" ", kw("@"))
        }
        m(Pattern2, s(p(SimplePattern, t.lhs), separator, " ", p(AnyPattern3, t.rhs)))
      case t: Pat.Alternative =>
        m(Pattern, s(p(Pattern, t.lhs), " ", kw("|"), " ", p(Pattern, t.rhs)))
      case t: Pat.Tuple => m(SimplePattern, s("(", r(t.args, ", "), ")"))
      case t: Pat.ArgClause => m(SimplePattern, s("(", r(t.values, ", "), ")"))
      case t: Pat.Extract => m(SimplePattern, s(t.fun, t.argClause))
      case t: Pat.ExtractInfix =>
        m(
          Pattern3(t.op.value),
          s(
            p(Pattern3(t.op.value), t.lhs, left = true),
            " ",
            t.op,
            " ",
            t.argClause match {
              case Pat.ArgClause(pat :: Nil) => s(p(Pattern3(t.op.value), pat, right = true))
              case pats => s(pats)
            }
          )
        )
      case t: Pat.Interpolate =>
        /** @see LegacyScanner.getStringPart, when ch == '$' */
        def needBraces(id: String): Boolean = !Character.isUnicodeIdentifierStart(id.head)
        val parts = t.parts.map { case Lit(part: String) => part }
        val zipped = parts.zip(t.args).map {
          case (part, id: Name) if !guessIsBackquoted(id) && !needBraces(id.value) =>
            s(part, "$", id.value)
          case (part, arg) =>
            s(part, "${", arg, "}")
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
      case t: Pat.Typed =>
        m(Pattern1, s(p(SimplePattern, t.lhs), kw(":"), " ", p(RefineTyp, t.rhs)))

      // Lit
      case Lit.Boolean(value) => m(Literal, s(value.toString))
      case Lit.Byte(value) =>
        m(
          Literal,
          s(
            "ByteLiterals.",
            if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value
          )
        )
      case Lit.Short(value) =>
        m(
          Literal,
          s(
            "ShortLiterals.",
            if (value == 0) "Zero" else if (value > 0) "Plus" + value else "Minus" + value
          )
        )
      case Lit.Int(value) => m(Literal, s(value.toString))
      case Lit.Long(value) => m(Literal, s(value.toString + "L"))
      case Lit.Float(value) =>
        val n = value.toFloat
        if (java.lang.Float.isNaN(n)) s("Float.NaN")
        else {
          n match {
            case Float.PositiveInfinity => s("Float.PositiveInfinity")
            case Float.NegativeInfinity => s("Float.NegativeInfinity")
            case _ if Character.toLowerCase(value.last) == 'f' => s(value)
            case _ =>
              s(value, "f")
          }
        }
      case Lit.Double(value) =>
        val n = value.toDouble
        if (java.lang.Double.isNaN(n)) s("Double.NaN")
        else {
          n match {
            case Double.PositiveInfinity => s("Double.PositiveInfinity")
            case Double.NegativeInfinity => s("Double.NegativeInfinity")
            case _ if Character.toLowerCase(value.last) == 'd' => s(value)
            case _ =>
              s(value, "d")
          }
        }
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
      case t: Member.ParamClauseGroup =>
        s(t.tparamClause, t.paramClauses)
      case t: Decl.Val =>
        s(w(t.mods, " "), kw("val"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
      case t: Decl.Var =>
        s(w(t.mods, " "), kw("var"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
      case t: Decl.Type => s(w(t.mods, " "), kw("type"), " ", t.name, t.tparamClause, t.bounds)
      case t: Decl.Def =>
        s(w(t.mods, " "), kw("def "), t.name, t.paramClauseGroups, kw(": "), t.decltpe)
      case t: Decl.Given =>
        s(w(t.mods, " "), kw("given "), t.name, o(t.paramClauseGroup), kw(": "), t.decltpe)
      case t: Defn.Val =>
        s(w(t.mods, " "), kw("val"), " ", r(t.pats, ", "), t.decltpe, " ", kw("="), " ", t.rhs)
      case t: Defn.Var =>
        s(
          w(t.mods, " "),
          kw("var"),
          " ",
          r(t.pats, ", "),
          t.decltpe,
          " ",
          kw("="),
          " ",
          t.body
        )
      case t: Defn.Type =>
        s(
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
      case t: Defn.Class =>
        r(" ")(
          t.mods,
          kw("class"),
          s(t.name, t.tparamClause, w(" ", t.ctor, t.ctor.mods.nonEmpty)),
          t.templ
        )
      case t: Defn.Trait =>
        if (dialect.allowTraitParameters || t.ctor.mods.isEmpty) {
          r(" ")(
            t.mods,
            kw("trait"),
            s(t.name, t.tparamClause, w(" ", t.ctor, t.ctor.mods.nonEmpty)),
            t.templ
          )
        } else {
          throw new UnsupportedOperationException(s"$dialect doesn't support trait parameters")
        }

      case t: Defn.GivenAlias =>
        val name = givenName(t.name, t.paramClauseGroup)
        r(" ")(t.mods, kw("given"), name, p(SimpleTyp, t.decltpe), kw("="), t.body)
      case t: Defn.Given =>
        r(" ")(t.mods, kw("given"), givenName(t.name, t.paramClauseGroup), t.templ)

      case t: Defn.Enum =>
        r(" ")(t.mods, kw("enum"), s(t.name, t.tparamClause, t.ctor), t.templ)
      case t: Defn.RepeatedEnumCase =>
        s(w(t.mods, " "), kw("case"), " ", r(t.cases, ", "))
      case t: Defn.EnumCase =>
        def init() = if (t.inits.nonEmpty) s(" extends ", r(t.inits, ", ")) else s("")
        s(w(t.mods, " "), kw("case"), " ", t.name, t.tparamClause, t.ctor, init())

      case t: Defn.ExtensionGroup =>
        s(kw("extension"), " ", o(t.paramClauseGroup, " "), t.body)
      case t: Defn.Object => r(" ")(t.mods, kw("object"), t.name, t.templ)
      case t: Defn.Def =>
        s(w(t.mods, " "), kw("def "), t.name, t.paramClauseGroups, t.decltpe, " = ", t.body)
      case t: Defn.Macro =>
        s(w(t.mods, " "), kw("def "), t.name, t.paramClauseGroups, t.decltpe, " = macro ", t.body)
      case t: Pkg =>
        if (guessHasBraces(t)) s(kw("package"), " ", t.ref, " {", r(t.stats.map(i(_)), ""), n("}"))
        else s(kw("package"), " ", t.ref, r(t.stats.map(n(_))))
      case t: Pkg.Object =>
        r(" ")(kw("package"), t.mods, kw("object"), t.name, t.templ)
      case t: Ctor.Primary =>
        val paramClauses = r(t.paramClauses.map(printParams(_)))
        s(w(t.mods, " ", t.mods.nonEmpty && t.paramClauses.nonEmpty), paramClauses)
      case t: Ctor.Secondary =>
        if (t.stats.isEmpty)
          s(w(t.mods, " "), kw("def"), " ", kw("this"), t.paramClauses, " = ", t.init)
        else
          s(
            w(t.mods, " "),
            kw("def"),
            " ",
            kw("this"),
            t.paramClauses,
            " {",
            i(t.init),
            "",
            t.stats,
            n("}")
          )

      // Init
      case t: Init =>
        s(
          if (t.tpe.is[Type.Singleton]) kw("this") else p(RefineTyp, t.tpe),
          t.argClauses
        )

      // Self
      case t: Self =>
        w(s(t.name, t.decltpe), " =>")

      // Template
      case t: Template =>
        val isSelfEmpty = t.self.isEmpty
        val pearly = r(t.early, "{ ", "; ", " } with")
        val pparents = r(t.inits, " with ")
        val derived = r(t.derives, "derives ", ", ", "")
        val isGiven = t.parent.exists(_.is[Defn.Given])
        val withGiven =
          if (!isGiven) ""
          else if (isSelfEmpty && t.stats.isEmpty) {
            // this could be just `()`, but it changes the tree
            t.inits match {
              case init :: Nil if init.argClauses.isEmpty => "with {}"
              case _ => ""
            }
          } else "with"
        val noExtends = pparents.isEmpty && pearly.isEmpty ||
          isGiven || t.parent.exists(_.is[Term.NewAnonymous])
        val extendsKeyword = if (noExtends) "" else "extends"
        val pbody = t.stats match {
          case Nil => w("{ ", t.self, " }")
          case stat :: Nil =>
            val statStr = s(stat).toString
            if (statStr.contains(EOL)) s("{", w(" ", t.self), i(stat), n("}"))
            else r(" ")("{", t.self, statStr, "}")
          case stats => s("{", w(" ", t.self), stats, n("}"))
        }
        r(" ")(extendsKeyword, pearly, pparents, derived, withGiven, pbody)

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
        if (dialect.allowAsForImportRename)
          s(t.name, " ", kw("as"), " ", t.rename)
        else
          s(t.name, " ", kw("=>"), " ", t.rename)
      case t: Importee.Unimport =>
        if (dialect.allowAsForImportRename)
          s(t.name, " ", kw("as"), " ", kw("_"))
        else
          s(t.name, " ", kw("=>"), " ", kw("_"))
      case _: Importee.Wildcard =>
        if (dialect.allowStarWildcardImport)
          kw("*")
        else
          kw("_")
      case t: Importer =>
        if (t.ref.isNot[Term.Anonymous])
          s(t.ref, ".", t.importees)
        else
          s(t.importees)
      case t: Import => s(kw("import"), " ", r(t.importers, ", "))
      case t: Export =>
        s(kw("export"), " ", r(t.importers, ", "))

      // Case
      case t: Case =>
        val ppat = p(Pattern, t.pat)
        val pcond = t.cond.map(cond => s(" ", kw("if"), " ", p(PostfixExpr, cond))).getOrElse(s())
        val isOneLiner = {
          def isOneLiner(t: Case) = t.stats match {
            case Nil => true
            case head :: Nil => head.is[Lit] || head.is[Term.Name]
            case _ => false
          }
          t.parent match {
            case Some(Term.Match(_, cases)) => cases.forall(isOneLiner)
            case Some(Term.PartialFunction(cases)) => cases.forall(isOneLiner)
            case _ => isOneLiner(t)
          }
        }
        val pbody = (t.stats, isOneLiner) match {
          case (Nil, true) => s("")
          case (List(stat), true) => s(" ", stat)
          case (stats, _) => s(stats)
        }
        s("case ", ppat, pcond, " ", kw("=>"), pbody)

      case t: TypeCase =>
        s("case ", t.pat, " ", kw("=>"), " ", t.body)
      // Source
      case t: Source => r(t.stats, EOL)
      case t: MultiSource => r(t.sources, s"$EOL$EOL@$EOL$EOL")
      case t: Term.AnonymousFunction => s(t.body)
    }

    private def givenName(name: meta.Name, pcGroup: Option[Member.ParamClauseGroup]): Show.Result =
      w(s(name, o(pcGroup)), ":")

    // Multiples and optionals
    private def printApplyArgs(args: Term.ArgClause, beforeBrace: String): Show.Result =
      args.values match {
        case Seq(b: Term.Block) => s(beforeBrace, b)
        case Seq(f: Term.Function) if f.paramClause.mod.isDefined =>
          s(beforeBrace, "{ ", f, " }")
        case _ => s(args)
      }

    implicit def syntaxParamClauseGroups: Syntax[Seq[Member.ParamClauseGroup]] = Syntax { r(_) }
    implicit def syntaxArgss: Syntax[Seq[Term.ArgClause]] = Syntax { r(_) }
    implicit def syntaxMods: Syntax[Seq[Mod]] = Syntax { r(_, " ") }
    private def isUsingOrImplicit(m: Mod): Boolean = m.is[Mod.ParamsType]
    private def printParam(t: Term.Param, keepImplicit: Boolean = false): Show.Result = {
      val mods = if (keepImplicit) t.mods else t.mods.filterNot(isUsingOrImplicit)
      val nameType = if (t.isNameAnonymous) {
        o(t.decltpe)
      } else {
        s(t.name, t.decltpe)
      }
      s(w(mods, " "), nameType, o(" = ", t.default))
    }
    implicit def syntaxAnnots: Syntax[Seq[Mod.Annot]] = Syntax { r(_, " ") }
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
      w("(", s(mod, r(v.map(printParam(_, mod eq Show.None)), ", ")), ")", useParens)
    }
    implicit def syntaxMemberParamss: Syntax[Seq[Member.ParamClause]] = Syntax { r(_) }
    implicit def syntaxTypeOpt: Syntax[Option[Type]] = Syntax { o(kw(": "), _) }
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

    implicit def syntaxCases: Syntax[Seq[CaseTree]] = Syntax { cases => r(cases.map(i(_))) }

    private def printStats(stats: Seq[Stat]) = r {
      val builder = List.newBuilder[Show.Result]
      var prevStat: Stat = null
      stats.foreach { stat =>
        if (stat.is[Term.Block] && null != prevStat && guessNeedsLineSep(prevStat))
          builder += System.lineSeparator
        builder += i(stat)
        prevStat = stat
      }
      builder.result()
    }

    implicit def syntaxStats: Syntax[Seq[Stat]] = Syntax(printStats)

  }
  def apply[T <: Tree](dialect: Dialect): Syntax[T] = {
    // NOTE: This is the current state of the art of smart prettyprinting.
    // If we prettyprint a tree that's just been parsed with the same dialect,
    // then we retain formatting. Otherwise, we don't, even in the tiniest.
    // I expect to improve on this in the nearest future, because we had it much better until recently.
    Syntax { (x: T) =>
      x.origin match {
        case o: Origin.Parsed if o.dialect eq dialect => s(o.position.text)
        case _ => reprint(x)(dialect)
      }
    }
  }

  def reprint[T <: Tree](x: T)(implicit dialect: Dialect): Show.Result = {
    new SyntaxInstances(dialect).syntaxTree[T].apply(x)
  }
}

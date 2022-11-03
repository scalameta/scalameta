package scala.meta
package internal
package prettyprinters

import scala.meta.classifiers._
import scala.meta.prettyprinters._
import Show.{
  sequence => s,
  repeat => r,
  indent => i,
  newline => n,
  meta => m,
  wrap => w,
  opt => o,
  function => fn
}
import scala.meta.internal.trees.{root => _, branch => _, _}
import scala.meta.internal.tokenizers.Chars._
import org.scalameta.adt._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.compat.Platform.EOL
import scala.meta.inputs.Position
import scala.meta.tokens.Token

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

    def p(og: SyntacticGroup, t: Tree, left: Boolean = false, right: Boolean = false) = {
      def opNeedsParens(oo: String, io: String, customPrecedence: Boolean = true): Boolean = {
        implicit class XtensionMySyntacticInfo(name: String) {
          def isleftassoc: Boolean = name.last != ':'
          def precedence: Int = if (customPrecedence) Name(name).precedence else 0
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
        case Show.Meta(ig: SyntacticGroup, res) if groupNeedsParens(og, ig) => s("(", res, ")")
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
      case _: Name.Anonymous => s()
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
        def needBraces(id: String, charAfter: Option[Char]): Boolean = {
          val startsWithUnderscore = id.startsWith("_")
          startsWithUnderscore || {
            val isOpId = isOperatorPart(id.head)
            val charAfterIsOp = charAfter.exists(isOperatorPart)
            val charAfterIsIdPart =
              charAfter.exists(chr => isNameStart(chr) || Character.isDigit(chr))
            if (isOpId) charAfterIsOp else charAfterIsIdPart
          }
        }

        val parts = t.parts.map { case Lit(part: String) => part.replace("$", "$$") }
        val zipped = parts.zip(t.args).zip(parts.tail).map {
          case ((part, id: Name), next)
              if !guessIsBackquoted(id) && !needBraces(id.value, next.headOption) =>
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
      case t: Term.Apply => m(SimpleExpr1, s(p(SimpleExpr1, t.fun), t.args))
      case t: Term.ApplyUsing =>
        val args = s("(", kw("using"), " ", r(t.args, ", "), ")")
        m(SimpleExpr1, s(p(SimpleExpr1, t.fun), args))
      case t: Term.ApplyType => m(SimpleExpr1, s(p(SimpleExpr, t.fun), t.targClause))
      case t: Term.ApplyInfix =>
        val args = t.args match {
          case (Lit.Unit()) :: Nil =>
            s("(())")
          case (arg: Term) :: Nil =>
            def needsParens: Boolean = arg match {
              case _: Term.AnonymousFunction => true
              case _: Lit | _: Term.Ref | _: Term.Function | _: Term.If | _: Term.Match |
                  _: Term.ApplyInfix | _: Term.QuotedMacroExpr | _: Term.SplicedMacroExpr |
                  _: Term.SplicedMacroPat | _: Term.Apply | _: Term.Placeholder =>
                false
              case _ =>
                true
            }

            if (needsParens) s("(", arg, ")")
            else s(p(InfixExpr(t.op.value), arg, right = true))
          case args => s(args)
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
        def pstats(s: Seq[Stat]) = r(s.map(i(_)), "")
        t match {
          case Block(
                Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, Block(stats)) :: Nil
              ) if mods.exists(_.is[Mod.Implicit]) =>
            m(
              SimpleExpr,
              s(
                "{ ",
                kw("implicit"),
                " ",
                name,
                tptopt.map(s(kw(":"), " ", _)).getOrElse(s()),
                " ",
                kw("=>"),
                " ",
                pstats(stats),
                n("}")
              )
            )
          case Block(
                Function(Term.Param(_, name: Name, None, _) :: Nil, Block(stats)) :: Nil
              ) =>
            m(SimpleExpr, s("{ ", name, " ", kw("=>"), " ", pstats(stats), n("}")))
          case Block(
                Function(Term.Param(_, _: Name.Anonymous, _, _) :: Nil, Block(stats)) :: Nil
              ) =>
            m(SimpleExpr, s("{ ", kw("_"), " ", kw("=>"), " ", pstats(stats), n("}")))
          case Block(Function(params, Block(stats)) :: Nil) =>
            m(SimpleExpr, s("{ (", r(params, ", "), ") => ", pstats(stats), n("}")))
          case _ =>
            m(SimpleExpr, if (t.stats.isEmpty) s("{}") else s("{", pstats(t.stats), n("}")))
        }
      case t: Term.If =>
        m(
          Expr1,
          s(
            w(t.mods, " "),
            kw("if"),
            " (",
            t.cond,
            ") ",
            p(Expr, t.thenp),
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
            r(t.cases.map(i(_)), ""),
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
            if (t.catchp.nonEmpty) s(" ", kw("catch"), " {", r(t.catchp.map(i(_)), ""), n("}"))
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
      case t: Term.ContextFunction =>
        t match {
          case Term.ContextFunction(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, body)
              if mods.exists(_.is[Mod.Implicit]) =>
            m(
              Expr,
              s(
                kw("implicit"),
                " ",
                name,
                tptopt.map(s(kw(":"), " ", _)).getOrElse(s()),
                " ",
                kw("?=>"),
                " ",
                p(Expr, body)
              )
            )
          case Term.ContextFunction(Term.Param(mods, name: Term.Name, None, _) :: Nil, body) =>
            m(Expr, s(name, " ", kw("?=>"), " ", p(Expr, body)))
          case Term.ContextFunction(Term.Param(_, _: Name.Anonymous, decltpeOpt, _) :: Nil, body) =>
            val param = decltpeOpt match {
              case Some(decltpe) => s(kw("("), kw("_"), kw(":"), decltpe, kw(")"))
              case None => s(kw("_"))
            }
            m(Expr, param, " ", kw("?=>"), " ", p(Expr, body))
          case Term.ContextFunction(params, body) =>
            if (params.headOption.exists(_.mods.exists(_.is[Mod.Using]))) {
              m(
                Expr,
                s("(", kw("using"), " ", r(params, ", "), ") ", kw("?=>"), " ", p(Expr, body))
              )
            } else {
              m(Expr, s("(", r(params, ", "), ") ", kw("?=>"), " ", p(Expr, body)))
            }
        }
      case t: Term.PolyFunction =>
        m(Expr, t.tparamClause, " ", kw("=>"), " ", p(Expr, t.body))
      case t: Term.Function =>
        t match {
          case Term.Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, body)
              if mods.exists(_.is[Mod.Implicit]) =>
            m(
              Expr,
              s(
                kw("implicit"),
                " ",
                name,
                tptopt.map(s(kw(":"), " ", _)).getOrElse(s()),
                " ",
                kw("=>"),
                " ",
                p(Expr, body)
              )
            )
          case Term.Function(Term.Param(mods, name: Term.Name, None, _) :: Nil, body) =>
            if (mods.exists(_.is[Mod.Using])) {
              m(Expr, s("(", kw("using"), " ", name, ") ", kw("=>"), " ", p(Expr, body)))
            } else {
              m(Expr, s(name, " ", kw("=>"), " ", p(Expr, body)))
            }
          case Term.Function(Term.Param(mods, _: Name.Anonymous, decltpeOpt, _) :: Nil, body) =>
            val isUsing = mods.exists(_.is[Mod.Using])
            val param = decltpeOpt match {
              case Some(decltpe) if isUsing =>
                s(kw("("), kw("using"), " ", kw("_"), kw(":"), " ", decltpe, kw(")"))
              case Some(decltpe) => s(kw("("), kw("_"), kw(":"), decltpe, kw(")"))
              case None => s(kw("_"))
            }
            m(Expr, param, " ", kw("=>"), " ", p(Expr, body))
          case Term.Function(params, body) =>
            m(Expr, s(printParams(params), " ", kw("=>"), " ", p(Expr, body)))
        }
      case Term.QuotedMacroExpr(Term.Block(stats)) =>
        stats match {
          case head :: Nil => s("'{ ", head, " }")
          case other => s("'{", r(other.map(i(_)), ""), n("}"))
        }
      case t: Term.QuotedMacroExpr =>
        m(SimpleExpr, s("'", p(Expr, t.body)))
      case t: Term.QuotedMacroType =>
        s("'[ ", t.tpe, " ]")
      case Term.SplicedMacroExpr(Term.Block(stats)) =>
        stats match {
          case head :: Nil => s("${ ", head, " }")
          case other => s("${", r(other.map(i(_)), ""), n("}"))
        }
      case Term.SplicedMacroPat(pat) =>
        s("${ ", pat, " }")
      case t: Term.SplicedMacroExpr =>
        m(SimpleExpr, s("$", p(Expr, t.body)))
      case t: Pat.Macro => m(SimplePattern, s(t.body))
      case t: Type.Macro => m(SimpleTyp, s(t.body))
      case t: Term.PartialFunction => m(SimpleExpr, s("{", r(t.cases.map(i(_)), ""), n("}")))
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
        val needsExplicitBraces = {
          val selfIsEmpty = t.templ.self.name.is[Name.Anonymous] && t.templ.self.decltpe.isEmpty
          t.templ.early.isEmpty && t.templ.inits.length < 2 && selfIsEmpty && t.templ.stats.isEmpty
        }
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
        printParams(t.values, needParens = !t.parent.exists(_.is[Term]))

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
      case t @ (_: Type.Function | _: Type.ContextFunction) =>
        val (prefix, tParams, arrow, tRes) = t match {
          case Type.Function(params, res) => (s(), params, "=>", res)
          case Type.ContextFunction(params, res) => (s(), params, "?=>", res)
        }
        val params = tParams match {
          case param :: Nil if param.isNot[Type.Tuple] && param.isNot[Type.TypedParam] =>
            s(p(AnyInfixTyp, param))
          case params => s("(", r(params.map(param => p(ParamTyp, param)), ", "), ")")
        }
        m(Typ, s(prefix, params, " ", kw(arrow), " ", p(Typ, tRes)))
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
          s(p(AnyInfixTyp, t.tpe), " ", kw("match"), " {", r(t.cases.map(i(_)), ""), n("}"))
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
      case t: Type.TypedParam => m(SimpleTyp, s(t.name.value), ": ", p(Typ, t.typ))
      case t: Type.ParamClause => r(t.values, "[", ", ", "]")
      case t: Type.Param =>
        def isVariant(m: Mod) = m.is[Mod.Variant]
        val mods = t.mods.filterNot(isVariant)
        require(t.mods.length - mods.length <= 1)
        val variance = o(t.mods.find(isVariant))
        val name = t.name match {
          case _: Name.Anonymous => s("_")
          case n => s(n)
        }
        val tbounds = s(t.tbounds)
        val vbounds = {
          if (t.vbounds.nonEmpty && !dialect.allowViewBounds)
            throw new UnsupportedOperationException(s"$dialect doesn't support view bounds")
          r(t.vbounds.map { s(" ", kw("<%"), " ", _) })
        }
        val cbounds = r(t.cbounds.map { s(kw(":"), " ", _) })
        s(w(mods, " "), variance, name, t.tparamClause, tbounds, vbounds, cbounds)

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
      case t: Pat.Extract => m(SimplePattern, s(t.fun, t.args))
      case t: Pat.ExtractInfix =>
        m(
          Pattern3(t.op.value),
          s(
            p(Pattern3(t.op.value), t.lhs, left = true),
            " ",
            t.op,
            " ",
            t.rhs match {
              case pat :: Nil => s(p(Pattern3(t.op.value), pat, right = true))
              case pats => s(pats)
            }
          )
        )
      case t: Pat.Interpolate =>
        val parts = t.parts.map { case Lit(part: String) => part }
        val zipped = parts.zip(t.args).map {
          case (part, id: Name) if !guessIsBackquoted(id) => s(part, "$", id.value)
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
          case Position.None => enquote(value.toString, SingleQuotes)
          case pos => pos.text
        }
        m(Literal, s(syntax))
      case t @ Lit.String(value) =>
        val syntax = t.pos match {
          case Position.None =>
            enquote(value, if (value.contains('\n')) TripleQuotes else DoubleQuotes)
          case pos => pos.text
        }
        m(Literal, s(syntax))
      case Lit.Symbol(value) => m(Literal, s("'", value.name))
      case Lit.Null() => m(Literal, s(kw("null")))
      case Lit.Unit() => m(Literal, s("()"))

      // Member
      case t: Decl.Val =>
        s(w(t.mods, " "), kw("val"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
      case t: Decl.Var =>
        s(w(t.mods, " "), kw("var"), " ", r(t.pats, ", "), kw(":"), " ", t.decltpe)
      case t: Decl.Type => s(w(t.mods, " "), kw("type"), " ", t.name, t.tparamClause, t.bounds)
      case t: Decl.Def =>
        s(w(t.mods, " "), kw("def "), t.name, t.tparamClause, t.paramClauses, kw(": "), t.decltpe)
      case t: Decl.Given =>
        s(w(t.mods, " "), kw("given "), t.name, t.tparamClause, t.paramClauses, kw(": "), t.decltpe)
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
          t.rhs.map(s(_)).getOrElse(s(kw("_")))
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
        r(" ")(
          t.mods,
          kw("given"),
          givenName(t.name, t.tparamClause, t.paramClauses),
          p(SimpleTyp, t.decltpe),
          kw("="),
          t.body
        )
      case t: Defn.Given =>
        r(" ")(
          t.mods,
          kw("given"),
          givenName(t.name, t.tparamClause, t.paramClauses),
          t.templ
        )

      case t: Defn.Enum =>
        r(" ")(
          t.mods,
          kw("enum"),
          s(t.name, t.tparamClause, t.ctor),
          t.templ
        )
      case t: Defn.RepeatedEnumCase =>
        s(w(t.mods, " "), kw("case"), " ", r(t.cases, ", "))
      case t: Defn.EnumCase =>
        def init() = if (t.inits.nonEmpty) s(" extends ", r(t.inits, ", ")) else s("")
        s(w(t.mods, " "), kw("case"), " ", t.name, t.tparamClause, t.ctor, init())

      case t: Defn.ExtensionGroup =>
        val m = t.body match {
          case block: Term.Block =>
            s(block)
          case onestat =>
            s(" ", onestat)
        }
        s(
          kw("extension"),
          " ",
          t.tparamClause,
          t.paramClauses,
          m
        )
      case t: Defn.Object => r(" ")(t.mods, kw("object"), t.name, t.templ)
      case t: Defn.Def =>
        s(
          w(t.mods, " "),
          kw("def "),
          t.name,
          t.tparamClause,
          t.paramClauses,
          t.decltpe,
          " = ",
          t.body
        )
      case t: Defn.Macro =>
        s(
          w(t.mods, " "),
          kw("def"),
          " ",
          t.name,
          t.tparamClause,
          t.paramClauses,
          t.decltpe,
          " ",
          kw("="),
          " ",
          kw("macro"),
          " ",
          t.body
        )
      case t: Pkg =>
        if (guessHasBraces(t)) s(kw("package"), " ", t.ref, " {", r(t.stats.map(i(_)), ""), n("}"))
        else s(kw("package"), " ", t.ref, r(t.stats.map(n(_))))
      case t: Pkg.Object =>
        r(" ")(kw("package"), t.mods, kw("object"), t.name, t.templ)
      case t: Ctor.Primary =>
        val paramClauses = r(t.paramClauses.map(x => printParams(x.values)))
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
            r(t.stats.map(i(_)), ""),
            n("}")
          )

      // Init
      case t: Init =>
        s(
          if (t.tpe.is[Type.Singleton]) kw("this") else p(RefineTyp, t.tpe),
          r(t.argss.map(x => s("(", r(x, ", "), ")")))
        )

      // Self
      case t: Self => s(t.name, t.decltpe)

      // Template
      case t: Template =>
        val isSelfEmpty = t.self.name.is[Name.Anonymous] && t.self.decltpe.isEmpty
        val isBodyEmpty = isSelfEmpty && t.stats.isEmpty
        val useExtends = (t.inits.nonEmpty || t.early.nonEmpty) &&
          t.parent.exists(p => p.isNot[Defn.Given] && p.isNot[Term.NewAnonymous])
        val extendsKeyword = if (useExtends) "extends" else ""
        val pearly = r(t.early, "{ ", "; ", " } with")
        val pparents = r(t.inits, " with ")
        val derived = r(t.derives, "derives ", ", ", "")
        val isGiven = t.parent.exists(_.is[Defn.Given])
        def needsAdditionalBraces = t.inits.size == 1 && t.inits.head.argss == Nil
        val withGiven =
          if (!isGiven) ""
          else if (isBodyEmpty) {
            // this could be just `()`, but it changes the tree
            if (needsAdditionalBraces) "with {}"
            else ""
          } else "with"
        val pbody = {
          val isOneLiner =
            t.stats.length == 0 ||
              (t.stats.length == 1 && !s(t.stats.head).toString.contains(EOL))
          (!isSelfEmpty, t.stats) match {
            case (false, Nil) => s()
            case (false, List(stat)) if isOneLiner => s("{ ", stat, " }")
            case (false, stats) => s("{", r(stats.map(i(_)), ""), n("}"))
            case (true, Nil) => s("{ ", t.self, " => }")
            case (true, List(stat)) if isOneLiner => s("{ ", t.self, " => ", stat, " }")
            case (true, stats) => s("{ ", t.self, " =>", r(stats.map(i(_)), ""), n("}"))
          }
        }
        r(" ")(extendsKeyword, pearly, pparents, derived, withGiven, pbody)

      // Mod
      case Mod.Annot(init) => s(kw("@"), p(SimpleTyp, init.tpe), init.argss)
      case Mod.Private(Name.Anonymous()) => s(kw("private"))
      case Mod.Private(within) => s(kw("private"), kw("["), within, kw("]"))
      case Mod.Protected(Name.Anonymous()) => s(kw("protected"))
      case Mod.Protected(within) => s(kw("protected"), kw("["), within, kw("]"))
      case _: Mod.Implicit => kw("implicit")
      case _: Mod.Final => kw("final")
      case _: Mod.Sealed => kw("sealed")
      case _: Mod.Open => kw("open")
      case _: Mod.Opaque => kw("opaque")
      case _: Mod.Using => kw("using")
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
          case (stats, _) => r(stats.map(i(_)), "")
        }
        s("case ", ppat, pcond, " ", kw("=>"), pbody)

      case t: TypeCase =>
        s("case ", t.pat, " ", kw("=>"), " ", t.body)
      // Source
      case t: Source => r(t.stats, EOL)
      case t: MultiSource => r(t.sources, s"$EOL$EOL@$EOL$EOL")
      case t: Term.AnonymousFunction => s(t.body)
    }

    private def givenName(
        name: meta.Name,
        tparams: Type.ParamClause,
        sparams: Seq[Term.ParamClause]
    ): Show.Result = {
      if (!name.is[meta.Name.Anonymous]) {
        s(name, tparams, sparams, ":")
      } else if (tparams.values.nonEmpty || sparams.nonEmpty) {
        s(tparams, sparams, ":")
      } else {
        s()
      }
    }

    // Multiples and optionals
    implicit def syntaxArgs: Syntax[Seq[Term]] = Syntax {
      case Seq(b: Term.Block) => s(" ", b)
      case Seq(f @ Term.Function(params, _))
          if params.exists(_.mods.exists(m => m.is[Mod.Implicit] || m.is[Mod.Using])) =>
        s(" { ", f, " }")
      case args => s("(", r(args, ", "), ")")
    }
    implicit def syntaxArgss: Syntax[Seq[Seq[Term]]] = Syntax {
      r(_)
    }
    implicit def syntaxTargs: Syntax[Seq[Type]] = Syntax {
      r(_, "[", ", ", "]")
    }
    implicit def syntaxPats: Syntax[Seq[Pat]] = Syntax { pats => s("(", r(pats, ", "), ")") }
    implicit def syntaxMods: Syntax[Seq[Mod]] = Syntax { mods =>
      r(mods, " ")
    }
    private def isUsingOrImplicit(m: Mod): Boolean = m.is[Mod.ParamsType]
    private def printParam(t: Term.Param, keepImplicit: Boolean = false): Show.Result = {
      val mods = if (keepImplicit) t.mods else t.mods.filterNot(isUsingOrImplicit)
      val nameType = if (t.name.is[Name.Anonymous]) {
        o(t.decltpe)
      } else {
        s(t.name, t.decltpe)
      }
      s(w(mods, " "), nameType, o(" = ", t.default))
    }
    implicit def syntaxAnnots: Syntax[Seq[Mod.Annot]] = Syntax { annots =>
      r(annots, " ")
    }
    private def printParams(t: Seq[Term.Param], needParens: Boolean = true): Show.Result = {
      val (useParens, mod) = t match {
        case head +: tail =>
          val modOpt = head.mods.collectFirst {
            case x: Mod.Using => x
            case x: Mod.Implicit if tail.forall(_.mods.exists(_.is[Mod.Implicit])) => x
          }
          val useParens = needParens || tail.nonEmpty ||
            modOpt.fold(head.decltpe.isDefined)(!_.is[Mod.Implicit])
          (useParens, o(modOpt, " "))
        case _ => (true, Show.None)
      }
      w("(", s(mod, r(t.map(printParam(_, mod eq Show.None)), ", ")), ")", useParens)
    }
    implicit def syntaxParamss: Syntax[Seq[Term.ParamClause]] = Syntax { paramss =>
      r(paramss)
    }
    implicit def syntaxTypeOpt: Syntax[Option[Type]] = Syntax {
      o(kw(": "), _)
    }
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

  }
  def apply[T <: Tree](dialect: Dialect): Syntax[T] = {
    // NOTE: This is the current state of the art of smart prettyprinting.
    // If we prettyprint a tree that's just been parsed with the same dialect,
    // then we retain formatting. Otherwise, we don't, even in the tiniest.
    // I expect to improve on this in the nearest future, because we had it much better until recently.
    Syntax { (x: T) =>
      x.origin match {
        // NOTE: Options don't really matter,
        // because if we've parsed a tree, it's not gonna contain lazy seqs anyway.
        // case Origin.Parsed(_, originalDialect, _) if dialect == originalDialect && options == Options.Eager =>
        case o @ Origin.Parsed(_, `dialect`, _) => s(o.position.text)
        case _ => reprint(x)(dialect)
      }
    }
  }

  def reprint[T <: Tree](x: T)(dialect: Dialect): Show.Result = {
    new SyntaxInstances(dialect).syntaxTree[T].apply(x)
  }
}

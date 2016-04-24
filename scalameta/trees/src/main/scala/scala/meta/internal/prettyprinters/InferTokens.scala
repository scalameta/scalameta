package scala.meta
package internal
package prettyprinters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.Universe
import scala.meta.internal.ast.Quasi
import scala.meta.internal.ast.Helpers._
import scala.compat.Platform.EOL
import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.meta.inputs._
import scala.meta.tokens._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._
import scala.meta.dialects.Scala211
import scala.meta.internal.tokenquasiquotes._

// TODO: this infers tokens for the Scala211 dialect due to token quasiquotes (the dialect needs to be explicitly imported). It should be changed in the future.
// TODO: fix occasional incorrectness when semicolons are omitted
// TODO: soft wrapping
// TODO: one mega instance for tree isn't nice, maybe separate instances for leafs and inferred instances for branches
// TODO: review https://github.com/scalameta/scalameta/pull/141 and apply fixes from there to here
private[meta] object inferTokens {
  def apply(tree: Tree, proto: Option[Tree]): Tokens = {
    val tokens = infer(tree, proto)(scala.meta.dialects.Scala211) // as explained above, forcing dialect.
    if (tokens.isInstanceOf[Tokens.Synthetic]) tokens
    else Tokens.Synthetic(tokens: _*)
  }

  /* Generate a single token for a literal */
  private def mineLitTk(value: Any)(implicit dialect: Dialect): Tokens = {
    if (value.isInstanceOf[Unit]) return toks"()"
    implicit def stringToInput(str: String) = Input.String(str)
    val str = if (value != null) value.toString else "null"
    val newTok = value match {
      case y: Int =>     Token.Constant.Int(str, dialect, 0, str.length, y)
      case y: Long =>    Token.Constant.Long(str + "L", dialect, 0, str.length + 1, y)
      case y: Float =>   Token.Constant.Float(str + "F", dialect, 0, str.length + 1, y)
      case y: Double =>  Token.Constant.Double(str, dialect, 0, str.length, y)
      case y: Char =>
        val newChar = enquote(str, SingleQuotes)
        Token.Constant.Char(newChar, dialect, 0, newChar.length, y)
      case y: Symbol =>  Token.Constant.Symbol(str, dialect, 0, str.length, y)
      case y: String =>
        val newStr = {
          if (y.contains(EOL)) enquote(str, TripleQuotes)
          else enquote(str, DoubleQuotes)
        }
        Token.Constant.String(newStr, dialect, 0, newStr.length, y)
      case true =>       Token.True(str, dialect, 0)
      case false =>      Token.False(str, dialect, 0)
      case null =>       Token.Null(str, dialect, 0)
    }
    Tokens(newTok)
  }

  /* Generate a single token for ident */
  private def mineIdentTk(value: String)(implicit dialect: Dialect): Tokens = Tokens(Token.Ident(Input.String(value), dialect, 0, value.length, value))

  /* Checking if a token is a potential indentation */
  val isIndent = (t: Token) => t.show[Syntax] == " " || t.show[Syntax] == "\t" || t.show[Syntax] == "\r"

  /* Global infering function */
  private def infer(tree: Tree, proto: Option[Tree])(implicit dialect: Dialect): Tokens = {
    import scala.meta.tokenizers._

    /* partial token vectors used in various constructions */
    val indentation =        toks"  " // In the future, this could be inferred
    val singleDoubleQuotes = Tokens(Token.Constant.String(Input.String("\""), dialect, 0, 1, "\""))
    val tripleDoubleQuotes = Tokens(Token.Constant.String(Input.String("\"\"\""), dialect, 0, 3, "\"\"\""))
    val newline =            Tokens(Token.LF(Input.String("\n"), dialect, 0))

    /* Enrichments for token manipulation */
    implicit class RichTree(tree: Tree) {
      def tks =      tokensWithParens(tree)
      def indTks =   indent(tokensWithParens(tree))(indentation)
      def `[->o->` = toks"$newline${tree.indTks}$newline"
    }
    implicit class RichTreeSeq(trees: Seq[Tree]) {
      /* Flatten tokens corresponding to a sequence of trees together. Can transform tokens corresponding to each tree using a first-order function. */
      def flattks(start: Tokens = toks"")(sep: Tokens = toks"", op: (Tokens => Tokens) = (x: Tokens) => x, trimAffixes: Boolean = true)(end: Tokens = toks"") = {
        val sq = {
          if (trees.isEmpty && trimAffixes) toks""
          else if (trees.isEmpty) start ++ end
          else start ++ trees.init.flatMap(v => op(v.tks) ++ sep) ++ op(trees.last.tks) ++ end
        }
        Tokens(sq: _*)
      }
      /* various combiners for tokens:
       * - o   representing subsequence
       * - [   represent an indentation
       * - ->  represent a new line
       * - _   represent a space
       * -     other tokens represent themselves.
       */
      val indentFun:          (Tokens => Tokens) = (s: Tokens) => indent(s)(indentation)
      /* In some construction, the line return is already present in the tokens. This prevents to put a second one and break the layout. */
      val avoidDoubleLineFun: (Tokens => Tokens) = (s: Tokens) => if (s.last.show[Syntax] == "\n") Tokens(s.repr.init: _*) else s
      def `oo` =       flattks()()()
      def `o->o` =     flattks()(newline, avoidDoubleLineFun)()
      def `->o->` =    flattks(newline)(newline, avoidDoubleLineFun)(newline)
      def `->o` =      flattks(newline)(newline)()
      def `o_o` =      flattks()(toks" ")()
      def `o,o` =      flattks()(toks", ")()
      def `o;o` =      flattks()(toks"; ")()
      def `o_o_` =     flattks()(toks" ")(toks" ")
      def `[o,o]` =    flattks(toks"[")(toks", ", trimAffixes = true)(toks"]")
      def `[[o,o]]` =  flattks(toks"[")(toks", ", trimAffixes = false)(toks"]")
      def `{o,o}` =    flattks(toks"{ ")(toks", ", trimAffixes = true)(toks" }")
      def `{{o,o}}` =  flattks(toks"{ ")(toks", ", trimAffixes = false)(toks" }")
      def `(o,o)` =    flattks(toks"(")(toks", ", trimAffixes = true)(toks")")
      def `((o,o))` =  flattks(toks"(")(toks", ", trimAffixes = false)(toks")")
      def `[->o->` =   flattks(newline)(newline, avoidDoubleLineFun andThen indentFun)(newline)
      def `[->o` =     flattks(newline)(newline, avoidDoubleLineFun andThen indentFun)()
    }
    implicit class RichTreeSeqSeq(trees: Seq[Seq[Tree]]) {
      def `((o,o))` = {
        val sq = trees match {
          case Nil => toks""
          case _ => trees.flatMap(_.`((o,o))`)
        }
        Tokens(sq: _*)
      }
    }
    implicit class RichTokensSeq(tkss: Seq[Tokens]) {
      def `oo` = Tokens(tkss.flatMap(_.repr): _*)
    }

    /* Append a template to a defn (class, trait, object) */
    def apndTempl(t: Template): Tokens = {
      val ext = if (!t.parents.isEmpty) toks" extends" else toks""
      val tpl = t.tks match {
        case tks if tks.repr.isEmpty => toks""
        case tks =>                     toks" $tks"
      }
      toks"$ext$tpl"
    }

    /* Append a declared type to a val / def / var */
    def apndDeclTpe(t: Option[Type]): Tokens = t match {
      case Some(tpe) if tpe.tks.length > 0 => toks": ${tpe.tks}"
      case _ =>                               toks""
    }

    /* Append bounds to a specific type declaration */
    def apndTpeBounds(t: Type.Bounds): Tokens = {
      if (t.lo.isDefined || t.hi.isDefined) toks" ${t.tks}"
      else toks""
    }

    /* Append a list of parameters wit implicit modifer properly added */
    def apndTermParamss(trees: Seq[Seq[Term.Param]]): Tokens = {
      def apndTermParams(ts: Seq[Term.Param]): Tokens = ts match {
        case Nil =>
          toks"()"
        case x :: Nil if x.mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          toks"(implicit ${x.tks})"
        case x :: xs if x.mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          toks"(implicit ${x.tks}, ${xs.`o,o`})"
        case _ =>
          ts.`(o,o)`
      }
      val sq = trees match {
        case Nil => toks""
        case _ => trees.flatMap(apndTermParams(_))
      }
      Tokens(sq: _*)
    }

    /* Append a name, but takes care of adding a space if one is needed with what follows (e.g. bind with a name ending with "_"). */
    def apndBindedName(name: Name): Tokens = name.tks match {
      case nm if nm.length == 0 => nm
      case nm if nm.last.show[Syntax] endsWith "_" => toks"$nm " // NOTE: adding a space if the name ends with _
      case nm => nm
    }

    /* Generate tokens for Pats, adding a space if one is needed */
    def apndDefnDeclPats(pats: Seq[Pat]) = pats.`o,o` match {
      case nm if nm.length == 0 => nm
      case ps if ps.last.show[Syntax] endsWith "_" => toks"$ps " // NOTE: adding a space if the name ends with _
      case ps => ps
    }

    /* The helpers below are heavily based on the ones used for the original show[Syntax] implementation. */
    def guessIsBackquoted(t: Name): Boolean = {
      // TODO: deduplicate wrt package.scala in tokenizers
      val keywords = Set(
        "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
        "object", "override", "return", "sealed", "trait", "try", "var", "while",
        "catch", "class", "extends", "false", "forSome", "if", "match", "new",
        "package", "private", "super", "this", "true", "type", "with", "yield",
        "def", "final", "implicit", "null", "protected", "throw", "val", "_",
        ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
      )
      def cantBeWrittenWithoutBackquotes(t: Name): Boolean = {
        t.value != "this" && (keywords.contains(t.value) || t.value.contains(" "))
      }
      def isAmbiguousWithPatVarTerm(t: Term.Name, p: Tree): Boolean = {
        val looksLikePatVar = t.value.head.isLower && t.value.head.isLetter
        val thisLocationAlsoAcceptsPatVars = p match {
          case p: Term.Name =>            unreachable
          case p: Term.Select =>          false
          case p: Pat.Wildcard =>         unreachable
          case p: Pat.Var.Term =>         false
          case p: Pat.Bind =>             unreachable
          case p: Pat.Alternative =>      true
          case p: Pat.Tuple =>            true
          case p: Pat.Extract =>          p.args.exists(_ eq t)
          case p: Pat.ExtractInfix =>     (p.lhs eq t) || p.rhs.exists(_ eq t)
          case p: Pat.Interpolate =>      p.args.exists(_ eq t)
          case p: Pat.Typed =>            unreachable
          case p: Pat =>                  unreachable
          case p: Case =>                 p.pat eq t
          case p: Defn.Val =>             p.pats.exists(_ eq t)
          case p: Defn.Var =>             p.pats.exists(_ eq t)
          case p: Enumerator.Generator => p.pat eq t
          case p: Enumerator.Val =>       p.pat eq t
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
    def guessPatHasRefinement(t: Pat.Type.Compound): Boolean = t.refinement.nonEmpty
    def guessHasExpr(t: Term.Return): Boolean = t.expr match { case Lit(_: Unit) => false; case _ => true }
    def guessHasElsep(t: Term.If): Boolean = t.elsep match { case Lit(_: Unit) => false; case _ => true }
    def guessHasBraces(t: Pkg): Boolean = {
      def isOnlyChildOfOnlyChild(t: Pkg): Boolean = t.parent match {
        case Some(pkg: Pkg) =>       isOnlyChildOfOnlyChild(pkg) && pkg.stats.length == 1
        case Some(source: Source) => source.stats.length == 1
        case None =>                 true
        case _ =>                    unreachable
      }
      !isOnlyChildOfOnlyChild(t)
    }

    /* Checking operator precedence and associativity to see if parentheses are needed in case of infix calls. */
    def opNeedsParens(childOp: String, parentOp: String, isLeft: Boolean, customAssoc: Boolean, customPrecedence: Boolean): Boolean = {
      implicit class XtensionMySyntacticInfo(opName: String) {
        def isLeftAssoc: Boolean = if (customAssoc) opName.last != ':' else true
        def precedence: Int = if (customPrecedence) Term.Name(opName).precedence else 0
      }
      val (childLAssoc, parentLAssoc) = (childOp.isLeftAssoc, parentOp.isLeftAssoc)
      if (childLAssoc ^ parentLAssoc) true
      else {
        val (childPrec, parentPrec) = (childOp.precedence, parentOp.precedence)
        if (isLeft) !childLAssoc || (childPrec < parentPrec)
        else childLAssoc && childPrec <= parentPrec
      }
    }

    /* Infer parentheses for a token based on its parent and generate the corresponding token stream using the `tkz` dispatcher. */
    def tokensWithParens(tree: Tree): Tokens = {
      def impNeedsParens(t: Tree) = t match {
        case _: Term.If =>        true
        case _: Term.Do =>        true
        case _: Term.While =>     true
        case _: Term.Function =>  true
        case _: Term.For =>       true
        case _: Term.ForYield =>  true
        case _: Term.Ascribe =>   true
        case _: Term.Annotate =>  true
        case _: Term.Match =>     true
        case _ =>                 false
      }
      val needsParens = (tree, tree.parent) match {
        /* Covering cases for calls on Term.Match  */
        case (_: Term.Match, Some(_: Term.Select)) =>     true
        case (t1, Some(t2: Term.Match)) if t2.scrut eq t1 => impNeedsParens(t1)
        /* Covering cases for Term.ApplyInfix */
        case (t1: Term.ApplyInfix, Some(t2: Term.ApplyInfix)) => opNeedsParens(t1.op.value, t2.op.value, isLeft = t2.lhs eq t1, customAssoc = true, customPrecedence = true)
        case (t1, Some(t2: Term.ApplyInfix)) if t2.args.length == 1 => impNeedsParens(t1)
        case (_: Term.ApplyInfix, Some(_: Term.Select)) => true
        /* Covering cases for Term.Ascibe */
        case (_: Term.Ascribe, Some(_: Term.Select)) =>   true
        /* Covering cases for Term.Annotate */
        case (_: Term.Annotate, Some(_: Term.Select)) =>  true
        case (_: Term.Ascribe, Some(_: Term.Annotate)) => true
        /* Covering cases for Type.Function */
         /* TODO: figure out why a function in a template as an extends does not have a parent! */
        case (_: Type.Function, None) =>                  true
        /* Covering cases for Type.ApplyInfix */
        case (t1: Type.ApplyInfix, Some(t2: Type.ApplyInfix)) =>         opNeedsParens(t1.op.value, t2.op.value, isLeft = t2.lhs eq t1, customAssoc = true, customPrecedence = false)
        case (t1: Pat.Type.ApplyInfix, Some(t2: Pat.Type.ApplyInfix)) => opNeedsParens(t1.op.value, t2.op.value, isLeft = t2.lhs eq t1, customAssoc = true, customPrecedence = false)
        /* Covering cases for Pat.ExtractInfix */
        case (t: Pat.Bind, Some(_: Pat.ExtractInfix)) => true
        case (t1: Pat.ExtractInfix, Some(t2: Pat.ExtractInfix)) => opNeedsParens(t1.ref.value, t2.ref.value, isLeft = t2.lhs eq t1, customAssoc = true, customPrecedence = true)
        /* Covering cases for Pat.Typed */
        case (t: Pat.Typed, Some(_: Pat.ExtractInfix)) => true
        /* Covering cases for Pat.Alternative */
        case (_: Pat.Alternative, Some(_: Pat.Bind)) =>   true
        /* Covering cases for Term.Unary */
        case (_, Some(_: Term.ApplyUnary)) =>
          tree match {
            case _: Term.ApplyInfix => true
            case _: Term.If =>         true
            case _ =>                  false
          }
        case (_: Term.ApplyUnary, Some(_: Term.Select)) => true
        case _ =>                                         false
      }
      def hasParens = tree.tokens.head.isInstanceOf[Token.LeftParen] && tree.tokens.last.isInstanceOf[Token.RightParen]
      if (needsParens && !hasParens) toks"(${deindent(tree.tokens)})"
      else deindent(tree.tokens)
    }

    def reconstructTokens(toks0: Tokens, stats0: Seq[Stat], stats1: Seq[Stat]): Tokens = {
      /* If the proto is defined, we can then extract the top-level comments the root contains and re-insert them
       * between the proper statements. We have no guarantee however that the indentation in a stat will correspond
       * to the indentation in the source token stream. As an outcome, we filter all indentation out prior to the
       * comparison. We then do the assumption that the correspondence between the stats from the original tree and
       * the new one are equivalent, i.e. that if a class A was before a class B, the class A is still before the
       * class B in the modified tree, even if this one or the other might contain more stats (in which case, they
       * are either added or removed at the end). By doing so, we are guaranteed to preserve top-level comments. */
      val oStatsTokens = stats0.map(_.tokens).repr
      val zipped = (oStatsTokens zip stats1.map(_.tokens.repr))
      val oStatsTail = oStatsTokens.drop(zipped.length).map(ts => (ts, Seq[Token]()))
      /* Loop and replace the original token streams from Source.stats the new ones */
      def loop(stream: Seq[Token], toReplace: Seq[(Seq[Token], Seq[Token])]): Seq[Token] = toReplace match {
        case Seq() => stream
        case tss =>
          val sliceIndex = stream.indexOfSlice(tss.head._1)
          loop(stream.patch(sliceIndex, tss.head._2, tss.head._1.length), tss.tail)
      }
      /* Keeping BOF */
      val patchedTokens =
        if (!toks0.repr.isEmpty && toks0.head.isInstanceOf[Token.BOF])
          toks0.repr.head +: loop(toks0.repr.tail, zipped ++ oStatsTail)
        else loop(toks0.repr, zipped ++ oStatsTail)
      val newStats = stats1.drop(zipped.length).`->o->`
      Tokens(patchedTokens ++ newStats: _*)
    }

    /* Infer tokens for a given tree, making use of the helpers above. */
    def tkz(tree: Tree): Tokens = tree match {
      // Bottom
      case t: Quasi if t.rank > 1 =>
        // TODO: quasis with higher ranks aren't implemented yet
        unreachable
      case t: Quasi if t.rank == 1 =>
        val ellipsis = Tokens(Token.Ellipsis(Input.String("." * (t.rank + 1)), dialect, 0, t.rank + 1, t.rank))
        val repr = t.tree.toString.tokenize.get // NOTE: here our token-bound prettyprinting abstraction leaks really hard
        val prefix = if (!t.tree.isInstanceOf[Quasi]) toks"{" else toks""
        val suffix = if (!t.tree.isInstanceOf[Quasi]) toks"}" else toks""
        ellipsis ++ prefix ++ repr ++ suffix
      case t: Quasi if t.rank == 0 =>
        val innerTreeTks = t.tree.toString.tokenize.get // NOTE: here our token-bound prettyprinting abstraction leaks really hard
        val name = mineIdentTk(t.pt.getName.stripPrefix("scala.meta.").stripPrefix("internal.ast."))
        toks"$${$innerTreeTks @ $name}"

      // Name
      case t: Name.Anonymous => toks"_"
      case t: Name.Indeterminate =>
        if (guessIsBackquoted(t)) mineIdentTk("`" + t.value + "`")
        else mineIdentTk(t.value)

      // Term
      case t: Term if t.isCtorCall =>
        if (t.isInstanceOf[Ctor.Ref.Function]) toks"=>${t.ctorArgss.`((o,o))`}"
        else toks"${t.ctorTpe.tks}${t.ctorArgss.`((o,o))`}"
      case t: Term.This =>
        val qual = if (t.qual.isInstanceOf[Name.Anonymous]) toks"" else toks"${t.qual.tks}."
        toks"${qual}this"
      case t: Term.Super =>
        val thisqual = if (t.thisp.isInstanceOf[Name.Anonymous]) toks"" else toks"${t.thisp.tks}."
        val superqual = if (t.superp.isInstanceOf[Name.Anonymous]) toks"" else toks"[${t.superp.tks}]"
        toks"${thisqual}super${superqual}"
      case t: Term.Name =>
        if (guessIsBackquoted(t)) mineIdentTk("`" + t.value + "`")
        else mineIdentTk(t.value)
      case t: Term.Select => toks"${t.qual.tks}.${t.name.tks}"
      case t: Term.Interpolate =>
        val zipped = (t.parts zip t.args) map {
          case (part, id: Name) if !guessIsBackquoted(id) => toks"${mineIdentTk(part.value.require[String])}$$${mineIdentTk(id.value)}"
          case (part, args) => toks"${mineIdentTk(part.value.require[String])}$${${args.tks}}"
        }
        val multiline = t.parts.map(_.value.require[String]).exists(s => s.contains(EOL) || s.contains("\""))
        val quote: Tokens = if (multiline) tripleDoubleQuotes else singleDoubleQuotes
        toks"${mineIdentTk(t.prefix.value)}$quote${zipped.`oo`}${mineIdentTk(t.parts.last.value.require[String])}$quote"
      case t: Term.Apply =>
        if (t.args.size == 1 && t.args.head.isInstanceOf[Term.PartialFunction]) toks"${t.fun.tks} ${t.args.head.tks}"
        else if (t.args.size == 1 && t.args.head.isInstanceOf[Term.Block]) toks"${t.fun.tks} ${t.args.head.tks}"
        else toks"${t.fun.tks}${t.args.`((o,o))`}"
      case t: Term.ApplyType => toks"${t.fun.tks}${t.targs.`[[o,o]]`}"
      case t: Term.ApplyInfix =>
        val rhs = t.args match {
          case (arg: Term) :: Nil => arg.tks
          case args => args.`((o,o))`
        }
        toks"${t.lhs.tks} ${t.op.tks} $rhs"
      case t: Term.ApplyUnary => toks"${t.op.tks}${t.arg.tks}"
      case t: Term.Assign =>     toks"${t.lhs.tks} = ${t.rhs.tks}"
      case t: Term.Update =>     toks"${t.fun.tks}${t.argss.`((o,o))`} = ${t.rhs.tks}"
      case t: Term.Return =>
        if (guessHasExpr(t)) toks"return ${t.expr.tks}"
        else toks"return"
      case t: Term.Throw =>      toks"throw ${t.expr.tks}"
      case t: Term.Ascribe =>    toks"${t.expr.tks}: ${t.decltpe.tks}"
      case t: Term.Annotate =>   toks"${t.expr.tks}: ${t.annots.`o_o`}"
      case t: Term.Tuple =>      t.elements.`((o,o))`
      case t: Term.Block =>
        import Term.{ Block, Function }
        t match {
          case Block(Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, Block(stats)) :: Nil) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
            val tpt = tptopt.map(v => toks": ${v.tks}").getOrElse(toks"")
            toks"{ implicit ${name.tks}$tpt =>${stats.`[->o->`}}"
          case Block(Function(Term.Param(mods, name: Term.Name, None, _) :: Nil, Block(stats)) :: Nil) =>
            toks"{ ${name.tks} =>${stats.`[->o->`}}"
          case Block(Function(Term.Param(_, _: Name.Anonymous, _, _) :: Nil, Block(stats)) :: Nil) =>
            toks"{ _ =>${stats.`[->o->`}}"
          case Block(Function(Nil, Block(stats)) :: Nil) =>
            toks"{ () =>${stats.`[->o->`}}"
          case Block(Function(params, Block(stats)) :: Nil) if params.length == 1 =>
            toks"{ ${params.`o,o`} =>${stats.`[->o->`}}"
          case Block(Function(params, Block(stats)) :: Nil) =>
            toks"{ ${params.`((o,o))`} =>${stats.`[->o->`}}"
          case _ =>
            if (t.stats.isEmpty) toks"{}"
            else toks"{${t.stats.`[->o->`}}"
        }
      case t: Term.If =>
        if (guessHasElsep(t)) toks"if (${t.cond.tks}) ${t.thenp.tks} else ${t.elsep.tks}"
        else toks"if (${t.cond.tks}) ${t.thenp.tks}"
      case t: Term.Match => toks"${t.scrut.tks} match {${t.cases.`[->o->`}}"
      case t: Term.TryWithCases =>
        val tryBlock = toks"try ${t.expr.tks}"
        val catchBlock = if (t.catchp.nonEmpty) toks" catch {${t.catchp.`[->o->`}}" else toks""
        val finallyBlock = if (t.finallyp.isDefined) toks" finally ${t.finallyp.get.tks}" else toks""
        tryBlock ++ catchBlock ++ finallyBlock
      case t: Term.TryWithTerm =>
        val tryBlock = toks"try ${t.expr.tks} catch {${t.catchp.`[->o->`}}"
        val finallyBlock = if (t.finallyp.isDefined) toks" finally ${t.finallyp.get.tks}" else toks""
        tryBlock ++ finallyBlock
      case t: Term.Function =>
        val cbody = t.body match {
          case b: Term.Block if b.stats.size == 1 && b.stats.head.isInstanceOf[Term.Block] => b.stats.head.tks
          case _ => t.body.tks
        }
        t match {
          case Term.Function(Term.Param(mods, name: Term.Name, tptopt, _) :: Nil, body) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
            val tpt = tptopt.map(v => toks": ${v.tks}").getOrElse(toks"")
            toks"implicit ${name.tks}$tpt =>$cbody"
          case Term.Function(Term.Param(mods, name: Term.Name, None, _) :: Nil, body) =>
            toks"${name.tks} => $cbody"
          case Term.Function(Term.Param(_, _: Name.Anonymous, _, _) :: Nil, body) =>
            toks"_ => $cbody"
          case Term.Function(Nil, body) =>
            toks"() => $cbody"
          case Term.Function(params, body) =>
            toks"${params.`((o,o))`} => $cbody"
        }
      case t: Term.PartialFunction => toks"{${t.cases.`[->o->`}}"
      case t: Term.While =>           toks"while (${t.expr.tks}) ${t.body.tks}"
      case t: Term.Do =>              toks"do ${t.body.tks} while (${t.expr.tks})"
      case t: Term.For =>             toks"for (${t.enums.`o;o`}) ${t.body.tks}"
      case t: Term.ForYield =>        toks"for (${t.enums.`o;o`}) yield ${t.body.tks}"
      case t: Term.New =>             toks"new ${t.templ.tks}"
      case _: Term.Placeholder =>     toks"_"
      case t: Term.Eta =>             toks"${t.term.tks} _"
      case t: Term.Arg.Named =>       toks"${t.name.tks} = ${t.rhs.tks}"
      case t: Term.Arg.Repeated =>    toks"${t.arg.tks}: _*"
      case t: Term.Param =>
        val mods = t.mods.filter(!_.isInstanceOf[Mod.Implicit]) // NOTE: `implicit` in parameters is skipped in favor of `implicit` in the enclosing parameter list
        val tname = apndBindedName(t.name)
        val tpe = t.decltpe.map(v => toks": ${v.tks}").getOrElse(toks"")
        val default = t.default.map(v => toks" = ${v.tks}").getOrElse(toks"")
        mods.`o_o_` ++ tname ++ tpe ++ default

      // Type
      case t: Type.Name =>
        if (guessIsBackquoted(t)) mineIdentTk("`" + t.value + "`")
        else mineIdentTk(t.value)
      case t: Type.Select =>     toks"${t.qual.tks}.${t.name.tks}"
      case t: Type.Project =>    toks"${t.qual.tks}#${t.name.tks}"
      case t: Type.Singleton =>  toks"${t.ref.tks}.type"
      case t: Type.Apply =>      toks"${t.tpe.tks}[${t.args.`o,o`}]"
      case t: Type.ApplyInfix => toks"${t.lhs.tks} ${t.op.tks} ${t.rhs.tks}"
      case t: Type.Function =>
        val params = {
          if (t.params.size == 1) t.params.head.tks
          else t.params.`((o,o))`
        }
        toks"$params => ${t.res.tks}"
      case t: Type.Tuple =>      t.elements.`((o,o))`
      case t: Type.Compound =>
        val tpes = t.tpes.flattks()(toks" with ")()
        if (guessHasRefinement(t)) toks"$tpes { ${t.refinement.`o;o`} }"
        else tpes
      case t: Type.Existential =>  toks"${t.tpe.tks} forSome { ${t.quants.`o;o`} }"
      case t: Type.Annotate =>     toks"${t.tpe.tks} ${t.annots.`o_o`}"
      case t: Type.Placeholder =>  toks"_ ${t.bounds.tks}"
      case t: Type.Bounds =>
        val loOpt = t.lo.map(lo => toks">: ${lo.tks}")
        val hiOpt = t.hi.map(hi => toks"<: ${hi.tks}")
        (loOpt, hiOpt) match {
          case (None, None) =>         toks""
          case (Some(hi), None) =>     hi
          case (None, Some(lo)) =>     lo
          case (Some(hi), Some(lo)) => toks"$hi $lo"
        }
      case t: Type.Arg.Repeated => toks"${t.tpe.tks}*"
      case t: Type.Arg.ByName =>   toks"=> ${t.tpe.tks}"
      case t: Type.Param =>
        val mods = t.mods.filter(m => !m.isInstanceOf[Mod.Covariant] && !m.isInstanceOf[Mod.Contravariant])
        require(t.mods.length - mods.length <= 1)
        val variance = t.mods.foldLeft(toks"")((curr, m) => if (m.isInstanceOf[Mod.Covariant]) toks"+" else if (m.isInstanceOf[Mod.Contravariant]) toks"-" else curr)
        val tname = apndBindedName(t.name)
        val tbounds = if (t.tbounds.lo.nonEmpty || t.tbounds.hi.nonEmpty) toks" ${t.tbounds.tks}" else toks""
        val vbounds = t.vbounds.flattks(toks" <% ")(toks" <% ")()
        val cbounds = t.cbounds.flattks(toks": ")(toks": ")()
        toks"${mods.o_o_}$variance$tname${t.tparams.`[o,o]`}$tbounds$vbounds$cbounds"

      // Pat
      case t: Pat.Var.Term =>    mineIdentTk(t.name.value)
      case _: Pat.Wildcard =>    toks"_"
      case t: Pat.Bind =>
        val separator: Tokens = if (t.rhs.isInstanceOf[Pat.Arg.SeqWildcard] && dialect.bindToSeqWildcardDesignator == ":") toks"" else toks" "
        val designator: Tokens = if (t.rhs.isInstanceOf[Pat.Arg.SeqWildcard] && dialect.bindToSeqWildcardDesignator == ":") toks":" else toks"@"
        toks"${t.lhs.tks}$separator$designator ${t.rhs.tks}"
      case t: Pat.Alternative => toks"${t.lhs.tks} | ${t.rhs.tks}"
      case t: Pat.Tuple =>       t.elements.`((o,o))`
      case t: Pat.Extract =>     toks"${t.ref.tks}${t.targs.`[o,o]`}${t.args.`((o,o))`}"
      case t: Pat.ExtractInfix =>
        t.rhs match {
          case x :: Nil => toks"${t.lhs.tks} ${t.ref.tks} ${x.tks}"
          case _ => toks"${t.lhs.tks} ${t.ref.tks} ${t.rhs.`((o,o))`}"
        }
      case t: Pat.Interpolate =>
        val zipped = (t.parts zip t.args) map {
          case (part, Pat.Var.Term(id: Name)) if !guessIsBackquoted(id) =>
            toks"${mineIdentTk(part.value.require[String])}$$${mineIdentTk(id.value)}"
          case (part, args) =>
            toks"${mineIdentTk(part.value.require[String])}$${${args.tks}}"
        }
        toks"${mineIdentTk(t.prefix.value)}$singleDoubleQuotes${zipped.`oo`}${mineIdentTk(t.parts.last.value.require[String])}$singleDoubleQuotes"
      case t: Pat.Typed =>           toks"${t.lhs.tks}: ${t.rhs.tks}"
      case _: Pat.Arg.SeqWildcard => toks"_*"

      // Pat.Type
      case t: Pat.Type.Wildcard =>    toks"_"
      case t: Pat.Var.Type =>         mineIdentTk(t.name.value)
      case t: Pat.Type.Project =>     toks"${t.qual.tks}#${t.name.tks}"
      case t: Pat.Type.Apply =>       toks"${t.tpe.tks}${t.args.`[[o,o]]`}"
      case t: Pat.Type.ApplyInfix =>  toks"${t.lhs.tks} ${t.op.tks} ${t.rhs.tks}"
      case t: Pat.Type.Function =>
        val params = if (t.params.size == 1) t.params.head.tks else t.params.`((o,o))`
        toks"$params => ${t.res.tks}"
      case t: Pat.Type.Tuple =>       t.elements.`((o,o))`
      case t: Pat.Type.Compound =>
        val tpes = t.tpes.flattks()(toks" with ")()
        if (guessPatHasRefinement(t)) toks"$tpes { ${t.refinement.`o;o`} }"
        else tpes
      case t: Pat.Type.Existential => toks"${t.tpe.tks} forSome { ${t.quants.`o;o`} }"
      case t: Pat.Type.Annotate =>    toks"${t.tpe.tks} ${t.annots.`o_o`}"
      case t: Pat.Type.Placeholder => toks"_ ${t.bounds.tks}"

      // Lit
      case t: Lit => mineLitTk(t.value)

      // Member
      case t: Decl.Val =>      toks"${t.mods.`o_o_`}val ${apndDefnDeclPats(t.pats)}: ${t.decltpe.tks}"
      case t: Decl.Var =>      toks"${t.mods.`o_o_`}var ${apndDefnDeclPats(t.pats)}: ${t.decltpe.tks}"
      case t: Decl.Type =>     toks"${t.mods.`o_o_`}type ${t.name.tks}${t.tparams.`[o,o]`}${apndTpeBounds(t.bounds)}"
      case t: Decl.Def =>      toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${apndTermParamss(t.paramss)}: ${t.decltpe.tks}"
      case t: Defn.Val =>      toks"${t.mods.`o_o_`}val ${apndDefnDeclPats(t.pats)}${apndDeclTpe(t.decltpe)} = ${t.rhs.tks}"
      case t: Defn.Var =>
        val rhs = t.rhs.map(trm => toks" = ${trm.tks}").getOrElse(toks"")
        toks"${t.mods.`o_o_`}var ${apndDefnDeclPats(t.pats)}${apndDeclTpe(t.decltpe)}$rhs"
      case t: Defn.Type =>     toks"${t.mods.`o_o_`}type ${t.name.tks}${t.tparams.`[o,o]`} = ${t.body.tks}"
      case t: Defn.Class =>    toks"${t.mods.`o_o_`}class ${t.name.tks}${t.tparams.`[o,o]`}${t.ctor.tks}${apndTempl(t.templ)}"
      case t: Defn.Trait =>    toks"${t.mods.`o_o_`}trait ${t.name.tks}${t.tparams.`[o,o]`}${t.ctor.tks}${apndTempl(t.templ)}"
      case t: Defn.Object =>   toks"${t.mods.`o_o_`}object ${t.name.tks}${apndTempl(t.templ)}"
      case t: Defn.Def =>      toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${apndTermParamss(t.paramss)}${apndDeclTpe(t.decltpe)} = ${t.body.tks}"
      case t: Defn.Macro =>    toks"${t.mods.`o_o_`}def ${t.name.tks}${t.tparams.`[o,o]`}${apndTermParamss(t.paramss)}${apndDeclTpe(t.decltpe)} = macro ${t.body.tks}"
      case t: Pkg =>
        proto match {
          case Some(original: Pkg) =>
            reconstructTokens(original.tokens, original.stats, t.stats)
          case _ =>
            if (guessHasBraces(t)) toks"package ${t.ref.tks} {${t.stats.`[->o->`}}"
            else toks"package ${t.ref.tks}${t.stats.`->o`}"
        }
      case t: Pkg.Object =>    toks"package ${t.mods.`o_o_`}object ${t.name.tks}${apndTempl(t.templ)}"
      case t: Ctor.Primary =>
        val cmods = t.mods.`o_o`
        val cparamss = apndTermParamss(t.paramss)
        if (!t.mods.isEmpty && !t.paramss.isEmpty) toks" $cmods $cparamss"
        else if (!t.paramss.isEmpty)               toks"$cparamss"
        else if (!t.mods.isEmpty)                  toks" $cmods"
        else                                       toks""
      case t: Ctor.Secondary =>
        if (t.body.isInstanceOf[Term.Block]) toks"def this${apndTermParamss(t.paramss)} ${t.body.tks}"
        else toks"def this${apndTermParamss(t.paramss)} = ${t.body.tks}"

      // Template
      case t: Template =>
        proto match {
          case Some(original: Template) =>
            reconstructTokens(original.tokens, original.stats.getOrElse(Nil), t.stats.getOrElse(Nil))
          case _ =>
            val isSelfEmpty = t.self.name.isInstanceOf[Name.Anonymous] && t.self.decltpe.isEmpty
            val isSelfNonEmpty = !isSelfEmpty
            val isBodyEmpty = isSelfEmpty && t.stats.isEmpty
            val isTemplateEmpty = t.early.isEmpty && t.parents.isEmpty && isBodyEmpty
            if (isTemplateEmpty) toks""
            else {
              val pearly = if (!t.early.isEmpty) toks"{ ${t.early.`o;o`} } with " else toks""
              val pparents = {
                if (!t.parents.isEmpty) t.parents.flattks()(toks" with ")()
                else toks""
              }
              val pbody = {
                val isOneLiner = t.stats.map(stats => stats.length == 0 || (stats.length == 1 && !stats.head.tokens.map(_.show[Syntax]).mkString.contains(EOL))).getOrElse(true)
                (isSelfNonEmpty, t.stats.nonEmpty, t.stats.getOrElse(Nil)) match {
                  case (false, false, _) =>                      toks""
                  case (true, false, _) =>                       toks"{ ${t.self.tks} => }"
                  case (false, true, Seq()) if isOneLiner =>     toks"{}"
                  case (false, true, Seq(stat)) if isOneLiner => toks"{ ${stat.tks} }"
                  case (false, true, stats) =>                   toks"{${stats.`[->o->`}}"
                  case (true, true, Seq()) if isOneLiner =>      toks"{ ${t.self.tks} => }"
                  case (true, true, Seq(stat)) if isOneLiner =>  toks"{ ${t.self.tks} => ${stat.tks} }"
                  case (true, true, stats) =>                    toks"{ ${t.self.tks} =>${stats.`[->o->`}}"
                }
              }
              if ((!t.early.isEmpty || !t.parents.isEmpty) && !pbody.isEmpty) toks"$pearly$pparents $pbody"
              else if (!t.early.isEmpty || !t.parents.isEmpty) pearly ++ pparents
              else pbody
            }
        }

      // Mod
      case Mod.Annot(tree) =>
        if (tree.isInstanceOf[Term.Annotate]) toks"@(${tree.ctorTpe.tks})${tree.ctorArgss.`((o,o))`}"
        else                                  toks"@${tree.ctorTpe.tks}${tree.ctorArgss.`((o,o))`}"
      case Mod.Private(Name.Anonymous()) =>   toks"private"
      case Mod.Private(name) =>               toks"private[${name.tks}]"
      case Mod.Protected(Name.Anonymous()) => toks"protected"
      case Mod.Protected(name) =>             toks"protected[${name.tks}]"
      case _: Mod.Implicit =>                 toks"implicit"
      case _: Mod.Final =>                    toks"final"
      case _: Mod.Sealed =>                   toks"sealed"
      case _: Mod.Override =>                 toks"override"
      case _: Mod.Case =>                     toks"case"
      case _: Mod.Abstract =>                 toks"abstract"
      case _: Mod.Covariant =>                toks"+"
      case _: Mod.Contravariant =>            toks"-"
      case _: Mod.Lazy =>                     toks"lazy"
      case _: Mod.ValParam =>                 toks"val"
      case _: Mod.VarParam =>                 toks"var"

      // Enumerator
      case t: Enumerator.Val =>       toks"${t.pat.tks} = ${t.rhs.tks}"
      case t: Enumerator.Generator => toks"${t.pat.tks} <- ${t.rhs.tks}"
      case t: Enumerator.Guard =>     toks"if ${t.cond.tks}"

      // Import
      case t: Importee.Name =>     toks"${t.value.tks}"
      case t: Importee.Rename =>   toks"${t.from.tks} => ${t.to.tks}"
      case t: Importee.Unimport => toks"${t.name.tks} => _"
      case _: Importee.Wildcard => toks"_"
      case t: Importer =>
        val needsBraces = t.importees match {
          case Seq(_: Importee.Name) => false
          case Seq(_: Importee.Wildcard) => false
          case _ => true
        }
        if (needsBraces) toks"${t.ref.tks}.${t.importees.`{{o,o}}`}"
        else toks"${t.ref.tks}.${t.importees.`oo`}"
      case t: Import => toks"import ${t.importers.`o,o`}"

      // Case
      case t: Case =>
        val ppat = t.pat.tks
        val pcond = t.cond.map(cond => toks" if ${cond.tks}").getOrElse(toks"")
        val isOneLiner = {
          def isOneLiner(t: Case) = t.stats.length == 0 || (t.stats.length == 1 && !t.stats.head.tokens.map(_.show[Syntax]).mkString.contains(EOL))
          t.parent match {
            case Some(Term.Match(_, cases)) => cases.forall(isOneLiner)
            case Some(Term.PartialFunction(cases)) => cases.forall(isOneLiner)
            case _ => isOneLiner(t)
          }
        }
        val pbody = (t.stats, isOneLiner) match {
          case (Nil, true) =>        toks""
          case (List(stat), true) => toks" ${stat.tks}"
          case (stats, _) =>         stats.`[->o`
        }
        toks"case ${t.pat.tks}$pcond =>$pbody"

      // Source
      case t: Source =>
        proto match {
          case Some(original: Source) => reconstructTokens(original.tokens, original.stats, t.stats)
          case _ => t.stats.`o->o`
        }
    }

    tkz(tree)
  }

  implicit class RichTokens(tks: Tokens) {
    /* Split token stream line-per-line */
    def onLines = {
      @tailrec def loop(in: Seq[Token], out: Seq[Seq[Token]]): Seq[Seq[Token]] = in match {
        case Nil => out
        case _ if !in.exists(_.show[Syntax] == "\n") => out :+ in
        case _ =>
          val (bf, af) = in.span(_.show[Syntax] != "\n")
          loop(af.tail, out :+ (bf :+ af.head))
      }
      loop(tks.repr, List())
    }
  }

  /* Removing original indentation from the token stream to let the inference put them back properly.
   * Since a unique token by itself does not know if it is authentic or not, we have to remove
   * the top indentation and re-infer it, to be sure that we have something looking good in the
   * end. A drawback on this is that we don't fully keep the original indentation. */
  private def deindent(tks: Tokens): Tokens = {
    // TODO: disabled because it doesn't play well with layout reinference for Source/Pkg/Template
    // val lines = tks.onLines
    // if (tks.isSynthetic || tks.repr.isEmpty || lines.length == 1) tks
    // else {
    //   /* In many cases, the start of a stat is not on a newline, while the end is.
    //    * As an outcome, we "de-indent" based on the original indentation of the last line. This is
    //    * mainly used for block, partial function, etc. and covers the cases that require such
    //    * re-indentation. */
    //   val lastIndent = lines.last.dropWhile(t => isIndent(t))
    //   val toDrop = lines.last.length - lastIndent.length
    //   def dropWhileWithMax[T](seq: Seq[T])(f: T => Boolean, max: Int): Seq[T] = {
    //     def loop(seq: Seq[T], count: Int = 0): Seq[T] = seq match {
    //       case tss if !tss.isEmpty && f(tss.head) && count < max => loop(tss.tail, count + 1)
    //       case _ => seq
    //     }
    //     loop(seq)
    //   }
    //   val deindented = lines.init map (l => dropWhileWithMax(l)(t => isIndent(t), toDrop))
    //   Tokens((deindented :+ lastIndent).flatten: _*)
    // }
    tks
  }

  /* Adding proper indentation to the token stream */
  private def indent(tks: Tokens)(indent: Tokens): Tokens = {
    val indented = tks.onLines map (line => indent ++ line)
    Tokens(indented.flatten: _*)
  }
}

package scala.meta.syntactic
package parsers

import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import scala.{Seq => _}
import scala.collection.immutable._
import org.scalameta.unreachable
import Chars.{isOperatorPart, isScalaLetter}
import Tok._
import Tokens._
import Tokens.{Token => LegacyToken}
import scala.meta.syntactic.parsers.{Tok => NewToken}
import scala.reflect.ClassTag
import scala.meta.internal.ast._
import scala.meta.Origin
import org.scalameta.tokens._

object SyntacticInfo {
  private[meta] val unaryOps = Set("-", "+", "~", "!")
  private[meta] def isUnaryOp(s: String): Boolean = unaryOps contains s
  implicit class SyntacticNameOps(val name: Name) extends AnyVal {
    import name._
    def isLeftAssoc: Boolean = value.last != ':'
    def isUnaryOp: Boolean = SyntacticInfo.isUnaryOp(value)
    def isAssignmentOp = value match {
      case "!=" | "<=" | ">=" | "" => false
      case _                       => (value.last == '=' && value.head != '='
                                       && isOperatorPart(value.head))
    }
    // opPrecedence?
    def precedence: Int =
      if (isAssignmentOp) 0
      else if (isScalaLetter(value.head)) 1
      else (value.head: @scala.annotation.switch) match {
        case '|'             => 2
        case '^'             => 3
        case '&'             => 4
        case '=' | '!'       => 5
        case '<' | '>'       => 6
        case ':'             => 7
        case '+' | '-'       => 8
        case '*' | '/' | '%' => 9
        case _               => 10
      }
    def isVarPattern: Boolean = name match {
      case _: Term.Name => !isBackquoted && value.head.isLower && value.head.isLetter
      case _            => false
    }
  }
  implicit class SyntacticTermRefOps(val tree: Term.Ref) extends AnyVal {
    def isPath: Boolean = tree.isStableId || tree.isInstanceOf[Term.This]
    def isQualId: Boolean = tree match {
      case _: Term.Name                   => true
      case Term.Select(qual: Term.Ref, _) => qual.isQualId
      case _                              => false
    }
    def isStableId: Boolean = tree match {
      case _: Term.Name | Term.Select(_: Term.Super, _) => true
      case Term.Select(qual: Term.Ref, _)               => qual.isPath
      case _                                            => false
    }
  }
  implicit class RichMod(val mod: Mod) extends AnyVal {
    def isAccess: Boolean = mod match {
      case _: Mod.Private         => true
      case _: Mod.PrivateThis     => true
      case _: Mod.PrivateWithin   => true
      case _: Mod.Protected       => true
      case _: Mod.ProtectedThis   => true
      case _: Mod.ProtectedWithin => true
      case _                      => false
    }
  }
  implicit class RichMods(val mods: List[Mod]) extends AnyVal {
    def has[T <: Mod](implicit tag: ClassTag[T]): Boolean =
      mods.exists { _.getClass == tag.runtimeClass }
    def getAll[T <: Mod](implicit tag: ClassTag[T]): List[T] =
      mods.collect { case m if m.getClass == tag.runtimeClass => m.asInstanceOf[T] }
    def access: Option[Mod] = mods.collectFirst{ case m if m.isAccess => m }
  }
  implicit class RichStat(val stat: Stat) extends AnyVal {
    def isTopLevelStat: Boolean = stat match {
      case _: Import => true
      case _: Pkg => true
      case _: Defn.Class => true
      case _: Defn.Trait => true
      case _: Defn.Object => true
      case _: Pkg.Object => true
      case _ => false
    }
    def isTemplateStat: Boolean = stat match {
      case _: Import => true
      case _: Term => true
      case _: Decl => true
      case _: Defn => true
      case _: Ctor.Secondary => true
      case _ => false
    }
    def isBlockStat: Boolean = stat match {
      case _: Import => true
      case _: Term => true
      case stat: Defn.Var => stat.rhs.isDefined
      case _: Defn => true
      case _ => false
    }
    def isRefineStat: Boolean = stat match {
      case _: Decl => true
      case _: Defn.Type => true
      case _ => false
    }
    def isExistentialStat: Boolean = stat match {
      case _: Decl.Val => true
      case _: Decl.Type => true
      case _ => false
    }
    def isEarlyStat: Boolean = stat match {
      case _: Defn.Val => true
      case _: Defn.Var => true
      case _ => false
    }
  }
}
import SyntacticInfo._

class Parser(val origin: Origin) extends AbstractParser {
  def this(code: String) = this(Origin.String(code))
  /** The parse starting point depends on whether the origin file is self-contained:
   *  if not, the AST will be supplemented.
   */
  def parseStartRule = () => compilationUnit()

  // NOTE: Scala's parser isn't ready to accept whitespace and comment tokens,
  // so we have to filter them out, because otherwise we'll get errors like `expected blah, got whitespace`
  // however, in certain tricky cases some whitespace tokens (namely, newlines) do have to be emitted
  // this leads to extremely dirty and seriously crazy code, which I'd like to replace in the future
  private class CrazyTokIterator(var pos: Int = -1, var tok: NewToken = null) extends TokIterator {
    var sepRegions: List[LegacyToken] = Nil
    require(tokens.nonEmpty)
    if (pos == -1) next() // TODO: only do next() if we've been just created. forks can't go for next()
    def hasNext: Boolean = tokens.drop(pos + 1).exists(_.isNot[Whitespace])
    def next(): Tok = {
      if (!hasNext) throw new NoSuchElementException()
      pos += 1
      val prev = tok
      val curr = tokens(pos)
      val next = tokens.drop(pos + 1).filter(_.isNot[Whitespace]).headOption.getOrElse(null)
      def updateSepRegions() = {
        if (prev == null) ()
        else if (prev.is[`(`]) sepRegions = RPAREN :: sepRegions
        else if (prev.is[`[`]) sepRegions = RBRACKET :: sepRegions
        else if (prev.is[`{`]) sepRegions = RBRACE :: sepRegions
        else if (prev.is[`case`]) sepRegions = ARROW :: sepRegions
        else if (prev.is[`}`]) {
          while (!sepRegions.isEmpty && sepRegions.head != RBRACE) sepRegions = sepRegions.tail
          if (!sepRegions.isEmpty) sepRegions = sepRegions.tail
        } else if (prev.is[`]`]) if (!sepRegions.isEmpty && sepRegions.head == RBRACKET) sepRegions = sepRegions.tail
        else if (prev.is[`)`]) if (!sepRegions.isEmpty && sepRegions.head == RPAREN) sepRegions = sepRegions.tail
        else if (prev.is[`=>`]) if (!sepRegions.isEmpty && sepRegions.head == ARROW) sepRegions = sepRegions.tail
        else () // TODO: handle string interpolation when we decide on its representation
      }
      def shouldEmitNewline() = {
        if (prev == null || next == null) false
        else prev.is[CanEndStat] && next.isNot[CantStartStat] && (sepRegions.isEmpty || sepRegions.head == RBRACE)
      }
      def emit() = {
        tok = curr
        tok
      }
      updateSepRegions()
      if (curr.isNot[Whitespace] || (curr.is[`\n`] && shouldEmitNewline)) emit()
      else this.next()
    }
    def fork: TokIterator = new CrazyTokIterator(pos, tok)
  }

  val tokens = origin.tokens
  var in: TokIterator = new CrazyTokIterator()

  /** the markup parser */
  // private[this] lazy val xmlp = new MarkupParser(this, preserveWS = true)
  // object symbXMLBuilder extends SymbolicXMLBuilder(this, preserveWS = true)
  // TODO: implement XML support
  def xmlLiteral(): Term = ??? //xmlp.xLiteral
  def xmlLiteralPattern(): Pat = ??? // xmlp.xLiteralPattern
}

class Location private(val value: Int) extends AnyVal
object Location {
  val Local      = new Location(0)
  val InBlock    = new Location(1)
  val InTemplate = new Location(2)
}
import Location.{ Local, InBlock, InTemplate }

abstract class AbstractParser { parser =>
  trait TokIterator extends Iterator[Tok] { def tok: Tok; def fork: TokIterator }
  var in: TokIterator
  def tok = in.tok
  def next() = in.next()
  val origin: Origin

  val report = Report(() => in.tok.offset)
  import report._

  /** Scoping operator used to temporarily look into the future.
   *  Backs up token iterator before evaluating a block and restores it after.
   */
  @inline final def ahead[T](body: => T): T = {
    val forked = in.fork
    next()
    try body finally in = forked
  }

  /** Perform an operation while peeking ahead.
   *  Recover to original state in case of exception.
   */
  @inline def peekingAhead[T](tree: => T): T = {
    val forked = in.fork
    next()
    // try it, in case it is recoverable
    try tree catch { case e: Exception => in = forked ; throw e }
  }

  def parseStartRule: () => Source

  def parseRule[T](rule: this.type => T): T = {
    val t = rule(this)
    accept[EOF]
    t
  }

  /** This is the general parse entry point.
   */
  def parseSource(): Source = parseRule(_.parseStartRule())

  /** This is the alternative entry point for repl, script runner, toolbox and parsing in macros.
   */
  def parseStat(): Stat = parseRule(parser => parser.statSeq(parser.templateStat.orElse(parser.topStat))) match {
    case stat :: Nil => stat
    case stats if stats.forall(_.isBlockStat) => Term.Block(stats)
    // TODO: haha, Source itself is not a stat
    // case stats if stats.forall(_.isTopLevelStat) => Source(stats)
    case _ => syntaxError("these statements can't be mixed together")
  }
  def parseStats(): List[Stat] = parseRule(_.templateStats())

  /** These are alternative entry points for quasiquotes.
   */
  def parseTerm(): Term = parseRule(_.expr())
  def parseType(): Type.Arg = parseRule(_.paramType())
  def parsePat(): Pat.Arg = parseRule(_.pattern())

  /** These are alternative entry points for quasiquotes.
   */
  def parseParam(): Templ.Param = ???
  def parseTparam(): Type.Param = ???
  def parseTermArg(): Term.Arg = ???
  def parseEnum(): Enum = ???
  def parseMod(): Mod = ???
  def parseTempl(): Templ = ???
  def parseCtorRef(): Ctor.Ref = ???
  def parseSelector(): Selector = ???
  def parseCase(): Case = ???

/* ------------- PARSER COMMON -------------------------------------------- */

  /** Methods inParensOrError and similar take a second argument which, should
   *  the next token not be the expected opener (e.g. Tok.`(`) will be returned
   *  instead of the contents of the groupers.  However in all cases accept[`(`]
   *  will be called, so a parse error will still result.  If the grouping is
   *  optional, tok should be tested before calling these methods.
   */
  @inline final def inParens[T](body: => T): T = {
    accept[`(`]
    val ret = body
    accept[`)`]
    ret
  }
  @inline final def inParensOrError[T](body: => T, alt: T): T =
    if (tok.is[`(`]) inParens(body)
    else { accept[`(`]; alt }

  @inline final def inParensOrUnit[T, Ret >: Lit](body: => Ret): Ret = inParensOrError(body, Lit.Unit())
  @inline final def inParensOrNil[T](body: => List[T]): List[T] = inParensOrError(body, Nil)

  @inline final def inBraces[T](body: => T): T = {
    accept[`{`]
    val ret = body
    accept[`}`]
    ret
  }
  @inline final def inBracesOrError[T](body: => T, alt: T): T =
    if (tok.is[`{`]) inBraces(body)
    else { accept[`{`]; alt }

  @inline final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOrError(body, Nil)
  @inline final def inBracesOrUnit[T](body: => Term): Term = inBracesOrError(body, Lit.Unit())
  @inline final def dropAnyBraces[T](body: => T): T =
    if (tok.is[`{`]) inBraces(body)
    else body

  @inline final def inBrackets[T](body: => T): T = {
    accept[`[`]
    val ret = body
    accept[`]`]
    ret
  }

/* ------------- ERROR HANDLING ------------------------------------------- */

  private var inFunReturnType = false
  @inline private def fromWithinReturnType[T](body: => T): T = {
    val saved = inFunReturnType
    inFunReturnType = true
    try body
    finally inFunReturnType = saved
  }

  // TODO: fixme once trees have positions
  def offset(t: Tree): Offset = in.tok.offset

  def syntaxErrorExpected[T: TokMetadata]: Nothing = syntaxError(s"${implicitly[TokMetadata[T]].name} expected but ${tok.name} found.")

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept[T: TokMetadata]: Unit =
    if (tok.is[T]) {
      if (tok.isNot[EOF]) next()
    } else syntaxErrorExpected[T]

  /** If current token is T consume it. */
  def acceptOpt[T: TokMetadata]: Unit =
    if (tok.is[T]) next()

  /** {{{
   *  semi = nl {nl} | `;`
   *  nl  = `\n' // where allowed
   *  }}}
   */
  def acceptStatSep(): Unit = tok match {
    case _: `\n` => next()
    case _       => accept[`;`]
  }
  def acceptStatSepOpt() =
    if (!tok.is[StatSeqEnd])
      acceptStatSep()

/* -------------- TOKEN CLASSES ------------------------------------------- */

  def isIdentAnd(pred: String => Boolean) = tok match {
    case id: Ident if pred(id.value) => true
    case _                           => false
  }
  def isUnaryOp: Boolean            = isIdentAnd(SyntacticInfo.isUnaryOp)
  def isIdentExcept(except: String) = isIdentAnd(_ != except)
  def isIdentOf(name: String)       = isIdentAnd(_ == name)
  def isRawStar: Boolean            = isIdentOf("*")
  def isRawBar: Boolean             = isIdentOf("|")

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

  /** Convert tree to formal parameter list. */
  def convertToParams(tree: Term): List[Term.Param] = tree match {
    case Term.Tuple(ts) => ts.toList flatMap convertToParam
    case _              => List(convertToParam(tree)).flatten
  }

  /** Convert tree to formal parameter. */
  def convertToParam(tree: Term): Option[Term.Param] = tree match {
    case name: Name =>
      Some(Term.Param(Nil, Some(name.toTermName), None, None))
    case Term.Placeholder() =>
      Some(Term.Param(Nil, None, None, None))
    case Term.Ascribe(name: Name, tpt) =>
      Some(Term.Param(Nil, Some(name.toTermName), Some(tpt), None))
    case Term.Ascribe(Term.Placeholder(), tpt) =>
      Some(Term.Param(Nil, None, Some(tpt), None))
    case Lit.Unit() =>
      None
    case other =>
      syntaxError(s"$other, not a legal formal parameter", at = offset(other))
  }

  def convertToTypeId(ref: Term.Ref): Option[Type] = ref match {
    case Term.Select(qual: Term.Ref, name) =>
      Some(Type.Select(qual, name.toTypeName))
    case name: Term.Name =>
      Some(name.toTypeName)
    case _ =>
      None
  }

  /** {{{ part { `sep` part } }}},or if sepFirst is true, {{{ { `sep` part } }}}. */
  final def tokenSeparated[Sep: TokMetadata, T](sepFirst: Boolean, part: => T): List[T] = {
    val ts = new ListBuffer[T]
    if (!sepFirst)
      ts += part

    while (tok.is[Sep]) {
      next()
      ts += part
    }
    ts.toList
  }

  @inline final def commaSeparated[T](part: => T): List[T] =
    tokenSeparated[`,`, T](sepFirst = false, part)

  @inline final def caseSeparated[T](part: => T): List[T] =
    tokenSeparated[`case`, T](sepFirst = true, part)

  def readAnnots(part: => Mod.Annot): List[Mod.Annot] =
    tokenSeparated[`@`, Mod.Annot](sepFirst = true, part)

  def makeTuple[T <: Tree](body: List[T], zero: () => T, tuple: List[T] => T): T = body match {
    case Nil         => zero()
    case only :: Nil => only
    case _           => tuple(body)
  }

  def makeTupleTerm(body: List[Term]): Term = {
    makeTuple[Term](body, () => Lit.Unit(), Term.Tuple(_))
  }

  def makeTupleTermParens(bodyf: => List[Term]) =
    makeTupleTerm(inParens(if (tok.is[`)`]) Nil else bodyf))

  // TODO: make zero tuple for types Lit.Unit() too?
  def makeTupleType(body: List[Type]): Type =
    makeTuple[Type](body, () => unreachable, Type.Tuple(_))

  def makeTuplePatParens(bodyf: => List[Pat.Arg]): Pat = {
    val body = inParens(if (tok.is[`)`]) Nil else bodyf.map(_.asInstanceOf[Pat]))
    makeTuple[Pat](body, () => Lit.Unit(), Pat.Tuple(_))
  }

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */

  /** Modes for infix types. */
  object InfixMode extends Enumeration {
    val FirstOp, LeftOp, RightOp = Value
  }

  // TODO: couldn't make this final, because of a patmat warning
  case class OpInfo[T: OpCtx](lhs: T, operator: Term.Name, targs: List[Type]) {
    def precedence = operator.precedence
  }

  sealed abstract class OpCtx[T] {
    def opinfo(tree: T): OpInfo[T]
    def binop(opinfo: OpInfo[T], rhs: T): T
    var stack: List[OpInfo[T]] = Nil
    def head = stack.head
    def push(top: T): Unit = stack ::= opinfo(top)
    def pop(): OpInfo[T] = try head finally stack = stack.tail
  }
  object OpCtx {
    implicit object `List Term.Arg Context` extends OpCtx[List[Term.Arg]] {
      def opinfo(tree: List[Term.Arg]): OpInfo[List[Term.Arg]] = {
        val name = termName()
        val targs = if (tok.is[`[`]) exprTypeArgs() else Nil
        OpInfo(tree, name, targs)
      }
      def binop(opinfo: OpInfo[List[Term.Arg]], rhs: List[Term.Arg]): List[Term.Arg] = {
        val lhs = makeTupleTerm(opinfo.lhs map {
          case t: Term => t
          case other   => unreachable
        })
        Term.ApplyInfix(lhs, opinfo.operator, opinfo.targs, rhs) :: Nil
      }
    }
    implicit object `Pat Context` extends OpCtx[Pat] {
      def opinfo(tree: Pat): OpInfo[Pat] = {
        val name = termName()
        if (tok.is[`[`]) syntaxError("infix patterns cannot have type arguments")
        OpInfo(tree, name, Nil)
      }
      def binop(opinfo: OpInfo[Pat], rhs: Pat): Pat = {
        val args = rhs match {
          case Pat.Tuple(args) => args.toList
          case _               => List(rhs)
        }
        Pat.ExtractInfix(opinfo.lhs, opinfo.operator, args)
      }
    }
  }

  def opctx[T](implicit ctx: OpCtx[T]) = ctx

  def checkHeadAssoc[T: OpCtx](leftAssoc: Boolean) = checkAssoc(opctx.head.operator, leftAssoc)

  def checkAssoc(op: Name, leftAssoc: Boolean): Unit = (
    if (op.isLeftAssoc != leftAssoc)
      syntaxError("left- and right-associative operators with same precedence may not be mixed")
  )

  def finishPostfixOp(base: List[OpInfo[List[Term.Arg]]], opinfo: OpInfo[List[Term.Arg]]): List[Term.Arg] =
    Term.Select(reduceStack(base, opinfo.lhs) match {
      case (t: Term) :: Nil => t
      case _                => unreachable
    }, opinfo.operator, isPostfix = true) :: Nil

  def finishBinaryOp[T: OpCtx](opinfo: OpInfo[T], rhs: T): T = opctx.binop(opinfo, rhs)

  def reduceStack[T: OpCtx](base: List[OpInfo[T]], top: T): T = {
    val opPrecedence = if (tok.is[Ident]) termName(advance = false).precedence else 0
    val leftAssoc    = !tok.is[Ident] || termName(advance = false).isLeftAssoc

    reduceStack(base, top, opPrecedence, leftAssoc)
  }

  def reduceStack[T: OpCtx](base: List[OpInfo[T]], top: T, opPrecedence: Int, leftAssoc: Boolean): T = {
    def isDone          = opctx.stack == base
    def lowerPrecedence = !isDone && (opPrecedence < opctx.head.precedence)
    def samePrecedence  = !isDone && (opPrecedence == opctx.head.precedence)
    def canReduce       = lowerPrecedence || leftAssoc && samePrecedence

    if (samePrecedence)
      checkHeadAssoc(leftAssoc)

    def loop(top: T): T =
      if (!canReduce) top
      else {
        val info = opctx.pop()
        loop(finishBinaryOp(info, top))
      }

    loop(top)
  }

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

  /** Methods which implicitly propagate the context in which they were
   *  called: either in a pattern context or not.  Formerly, this was
   *  threaded through numerous methods as boolean isPattern.
   */
  trait PatternContextSensitive {
    /** {{{
     *  ArgType       ::=  Type
     *  }}}
     */
    def argType(): Type
    def functionArgType(): Type.Arg

    private def tupleInfixType(): Type = {
      next()
      if (tok.is[`)`]) {
        next()
        accept[`=>`]
        Type.Function(Nil, typ())
      }
      else {
        val ts = functionTypes()
        accept[`)`]
        if (tok.is[`=>`]) {
          next()
          Type.Function(ts, typ())
        } else {
          val tuple = makeTupleType(ts map {
            case t: Type              => t
            case p: Type.Arg.ByName   => syntaxError("by name type not allowed here", at = offset(p))
            case p: Type.Arg.Repeated => syntaxError("repeated type not allowed here", at = offset(p))
          })
          infixTypeRest(
            compoundTypeRest(Some(
              annotTypeRest(
                simpleTypeRest(
                  tuple)))),
            InfixMode.FirstOp
          )
        }
      }
    }
    private def makeExistentialTypeTree(t: Type): Type.Existential = {
      // EmptyTrees in the result of refinement() stand for parse errors
      // so it's okay for us to filter them out here
      Type.Existential(t, existentialStats())
    }

    /** {{{
     *  Type ::= InfixType `=>' Type
     *         | `(' [`=>' Type] `)' `=>' Type
     *         | InfixType [ExistentialClause]
     *  ExistentialClause ::= forSome `{' ExistentialDcl {semi ExistentialDcl}} `}'
     *  ExistentialDcl    ::= type TypeDcl | val ValDcl
     *  }}}
     */
    def typ(): Type = {
      val t: Type =
        if (tok.is[`(`]) tupleInfixType()
        else infixType(InfixMode.FirstOp)

      tok match {
        case _: `=>`      => next(); Type.Function(List(t), typ())
        case _: `forSome` => next(); makeExistentialTypeTree(t)
        case _            => t
      }
    }

    /** {{{
     *  TypeArgs    ::= `[' ArgType {`,' ArgType} `]'
     *  }}}
     */
    def typeArgs(): List[Type] = inBrackets(types())

    /** {{{
     *  ModType        ::=  SimpleType {Annotation}
     *  }}}
     */
    def annotType(): Type = annotTypeRest(simpleType())

    /** {{{
     *  SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' Id
     *                     |  StableId
     *                     |  Path `.' type
     *                     |  `(' Types `)'
     *                     |  WildcardType
     *  }}}
     */
    def simpleType(): Type = {
      simpleTypeRest(tok match {
        case _: `(`   => makeTupleType(inParens(types()))
        case _: `_ ` => next(); wildcardType()
        case _        =>
          val ref: Term.Ref = path()
          if (tok.isNot[`.`])
            convertToTypeId(ref) getOrElse { syntaxError("identifier expected") }
          else {
            next()
            accept[`type`]
            Type.Singleton(ref)
          }
      })
    }

    private def typeProjection(qual: Type): Type.Project = {
      next()
      Type.Project(qual, typeName())
    }
    def simpleTypeRest(t: Type): Type = tok match {
      case _: `#` => simpleTypeRest(typeProjection(t))
      case _: `[` => simpleTypeRest(Type.Apply(t, typeArgs()))
      case _      => t
    }

    /** {{{
     *  CompoundType ::= ModType {with ModType} [Refinement]
     *                |  Refinement
     *  }}}
     */
    def compoundType(): Type = compoundTypeRest(
      if (tok.is[`{`]) None
      else Some(annotType())
    )

    // TODO: warn about def f: Unit { } case?
    def compoundTypeRest(t: Option[Type]): Type = {
      val ts = new ListBuffer[Type] ++ t
      while (tok.is[`with`]) {
        next()
        ts += annotType()
      }
      newLineOptWhenFollowedBy[`{`]
      val types = ts.toList
      if (tok.is[`{`]) {
        val refinements = refinement()
        (types, refinements) match {
          case (typ :: Nil, Nil) => typ
          case _  => Type.Compound(types, refinements)
        }
      } else {
        if (types.length == 1) types.head
        else Type.Compound(types)
      }
    }

    def infixTypeRest(t: Type, mode: InfixMode.Value): Type = {
      if (isIdentExcept("*")) {
        val name = termName(advance = false)
        val leftAssoc = name.isLeftAssoc
        if (mode != InfixMode.FirstOp) checkAssoc(name, leftAssoc = mode == InfixMode.LeftOp)
        val op = typeName()
        newLineOptWhenFollowing(_.is[TypeIntro])
        def mkOp(t1: Type) = Type.ApplyInfix(t, op, t1)
        if (leftAssoc)
          infixTypeRest(mkOp(compoundType()), InfixMode.LeftOp)
        else
          mkOp(infixType(InfixMode.RightOp))
      } else t
    }

    /** {{{
     *  InfixType ::= CompoundType {id [nl] CompoundType}
     *  }}}
     */
    def infixType(mode: InfixMode.Value): Type =
      infixTypeRest(compoundType(), mode)

    /** {{{
     *  Types ::= Type {`,' Type}
     *  }}}
     */
    def types(): List[Type] = commaSeparated(argType())
    def functionTypes(): List[Type.Arg] = commaSeparated(functionArgType())
  }

  // TODO: can we get away without this?
  implicit class NameToName(name: Name) {
    def toTypeName: Type.Name = name match {
      case name: Type.Name => name
      case _              => Type.Name(name.value, name.isBackquoted)
    }
    def toTermName: Term.Name = name match {
      case name: Term.Name => name
      case _              => Term.Name(name.value, name.isBackquoted)
    }
  }

  def name[T](ctor: (String, Boolean) => T, advance: Boolean): T = tok match {
    case Ident(_, name, isBackquoted, _) =>
      val res = ctor(name, isBackquoted)
      if (advance) next()
      res
    case _ =>
      syntaxErrorExpected[Ident]
  }
  def termName(advance: Boolean = true): Term.Name = name(Term.Name(_, _), advance)
  def typeName(advance: Boolean = true): Type.Name = name(Type.Name(_, _), advance)

  /** {{{
   *  Path       ::= StableId
   *              |  [Ident `.'] this
   *  ModType ::= Path [`.' type]
   *  }}}
   */
  // TODO: this has to be rewritten
  def path(thisOK: Boolean = true): Term.Ref = {
    def stop = tok.isNot[`.`] || ahead { tok.isNot[`this`] && tok.isNot[`super`] && !tok.is[Ident] }
    if (tok.is[`this`]) {
      next()
      val thisnone = Term.This(None)
      if (stop && thisOK) thisnone
      else {
        accept[`.`]
        selectors(thisnone)
      }
    } else if (tok.is[`super`]) {
      next()
      val superp = Term.Super(None, mixinQualifierOpt())
      accept[`.`]
      val supersel = Term.Select(superp, termName(), isPostfix = false)
      if (stop) supersel
      else {
        next()
        selectors(supersel)
      }
    } else {
      val name = termName()
      if (stop) name
      else {
        next()
        if (tok.is[`this`]) {
          next()
          val thisid = Term.This(Some(name.value))
          if (stop && thisOK) thisid
          else {
            accept[`.`]
            selectors(thisid)
          }
        } else if (tok.is[`super`]) {
          next()
          val superp = Term.Super(Some(name.value), mixinQualifierOpt())
          accept[`.`]
          val supersel = Term.Select(superp, termName(), isPostfix = false)
          if (stop) supersel
          else {
            next()
            selectors(supersel)
          }
        } else {
          selectors(name)
        }
      }
    }
  }

  def selector(t: Term): Term.Select = Term.Select(t, termName(), isPostfix = false)
  def selectors(t: Term.Ref): Term.Ref ={
    val t1 = selector(t)
    if (tok.is[`.`] && ahead { tok.is[Ident] }) {
      next()
      selectors(t1)
    }
    else t1
  }

  /** {{{
  *   MixinQualifier ::= `[' Id `]'
  *   }}}
  */
  def mixinQualifierOpt(): Option[String] =
    if (tok.is[`[`]) Some(inBrackets(typeName().value))
    else None

  /** {{{
   *  StableId ::= Id
   *            |  Path `.' Id
   *            |  [id `.'] super [`[' id `]']`.' id
   *  }}}
   */
  def stableId(): Term.Ref =
    path(thisOK = false)

  /** {{{
  *   QualId ::= Id {`.' Id}
  *   }}}
  */
  def qualId(): Term.Ref = {
    val name = termName()
    if (tok.isNot[`.`]) name
    else {
      next()
      selectors(name)
    }
  }

  /** {{{
   *  SimpleExpr    ::= literal
   *                  | symbol
   *                  | null
   *  }}}
   */
  def literal(isNegated: Boolean = false): Lit = {
    val res = tok match {
      case Literal.Char(_, v, _)       => Lit.Char(v)
      case Literal.Int(_, v, _)        => Lit.Int(if (isNegated) -v else v)
      case Literal.Long(_, v, _)       => Lit.Long(if (isNegated) -v else v)
      case Literal.Float(_, v, _)      => Lit.Float(if (isNegated) -v else v)
      case Literal.Double(_, v, _)     => Lit.Double(if (isNegated) -v else v)
      case Literal.String(_, v, _)     => Lit.String(v)
      case Literal.Symbol(_, v, _)     => Lit.Symbol(v)
      case Interpolation.Part(_, v, _) => Lit.String(v)
      case _: `true`                   => Lit.Bool(true)
      case _: `false`                  => Lit.Bool(false)
      case _: `null`                   => Lit.Null()
      case _                           => syntaxError("illegal literal")
    }
    next()
    res
  }

  def interpolate[Ctx, Ret](arg: () => Ctx, result: (Term.Name, List[Lit.String], List[Ctx]) => Ret): Ret = {
    def part() = {
      val value = tok match {
        case Literal.String(_, v, _)     => v
        case Interpolation.Part(_, v, _) => v
        case _                           => syntaxErrorExpected[Interpolation.Part]
      }
      val lit = Lit.String(value)
      next()
      lit
    }
    val Interpolation.Id(_, nameStr, _) = tok
    val interpolator = Term.Name(nameStr, isBackquoted = false) // termName() for INTERPOLATIONID
    next()
    val partsBuf = new ListBuffer[Lit.String]
    val argsBuf = new ListBuffer[Ctx]
    while (tok.is[Interpolation.Part]) {
      partsBuf += part()
      argsBuf += arg()
    }
    if (tok.is[Literal.String]) partsBuf += part()
    result(interpolator, partsBuf.toList, argsBuf.toList)
  }

  def interpolateTerm(): Term.Interpolate = {
    def dropTrivialBlock(term: Term): Term = term match {
      case Term.Block((stat: Term) :: Nil) => stat
      case _ => term
    }
    interpolate[Term, Term.Interpolate](arg = { () =>
      tok match {
        case _: Ident   => termName()
        // case _: `_ ` => freshPlaceholder()       // ifonly etapolation
        case _: `{`     => dropTrivialBlock(expr()) // dropAnyBraces(expr0(Local))
        case _: `this`  => next(); Term.This(None)
        case _          => syntaxError("error in interpolated string: identifier or block expected")
      }
    }, result = Term.Interpolate(_, _, _))
  }

  def interpolatePat(): Pat.Interpolate =
    interpolate[Pat, Pat.Interpolate](arg = () => dropAnyBraces(pattern().asInstanceOf[Pat]), result = Pat.Interpolate(_, _, _))

/* ------------- NEW LINES ------------------------------------------------- */

  def newLineOpt(): Unit = {
    if (tok.is[`\n`]) next()
  }

  def newLinesOpt(): Unit = {
    if (tok.is[`\n`])
      next()
  }

  def newLineOptWhenFollowedBy[T: TokMetadata]: Unit = {
    // note: next is defined here because current is Tok.`\n`
    if (tok.is[`\n`] && ahead { tok.is[T] }) newLineOpt()
  }

  def newLineOptWhenFollowing(p: Tok => Boolean): Unit = {
    // note: next is defined here because current is Tok.`\n`
    if (tok.is[`\n`] && ahead { p(tok) }) newLineOpt()
  }

/* ------------- TYPES ---------------------------------------------------- */

  /** {{{
   *  TypedOpt ::= [`:' Type]
   *  }}}
   */
  def typedOpt(): Option[Type] =
    if (tok.is[`:`]) { next(); Some(typ()) }
    else None

  def typeOrInfixType(location: Location): Type =
    if (location == Local) typ()
    else startInfixType()

  def annotTypeRest(t: Type): Type =
    (t /: annots(skipNewLines = false)) { case (t, annot) => Type.Annotate(t, annot :: Nil) }

  /** {{{
   *  WildcardType ::= `_' TypeBounds
   *  }}}
   */
  def wildcardType(): Type = Type.Placeholder(typeBounds())

/* ----------- EXPRESSIONS ------------------------------------------------ */

  def condExpr(): Term = {
    if (tok.is[`(`]) {
      next()
      val r = expr()
      accept[`)`]
      r
    } else {
      accept[`(`]
      Lit.Bool(true)
    }
  }

  /** {{{
   *  Expr       ::= (Bindings | [`implicit'] Id | `_')  `=>' Expr
   *               | Expr1
   *  ResultExpr ::= (Bindings | Id `:' CompoundType) `=>' Block
   *               | Expr1
   *  Expr1      ::= if `(' Expr `)' {nl} Expr [[semi] else Expr]
   *               | try (`{' Block `}' | Expr) [catch `{' CaseClauses `}'] [finally Expr]
   *               | while `(' Expr `)' {nl} Expr
   *               | do Expr [semi] while `(' Expr `)'
   *               | for (`(' Enumerators `)' | `{' Enumerators `}') {nl} [yield] Expr
   *               | throw Expr
   *               | return [Expr]
   *               | [SimpleExpr `.'] Id `=' Expr
   *               | SimpleExpr1 ArgumentExprs `=' Expr
   *               | PostfixExpr Ascription
   *               | PostfixExpr match `{' CaseClauses `}'
   *  Bindings   ::= `(' [Binding {`,' Binding}] `)'
   *  Binding    ::= (Id | `_') [`:' Type]
   *  Ascription ::= `:' CompoundType
   *               | `:' Annotation {Annotation}
   *               | `:' `_' `*'
   *  }}}
   */
  def expr(): Term = expr(Local)

  def expr(location: Location): Term = tok match {
    case _: `if` =>
      next()
      val cond = condExpr()
      newLinesOpt()
      val thenp = expr()
      if (tok.is[`else`]) { next(); Term.If(cond, thenp, expr()) }
      else if (tok.is[`;`] && ahead { tok.is[`else`] }) { next(); next(); Term.If(cond, thenp, expr()) }
      else { Term.If(cond, thenp) }
    case _: `try` =>
      next()
      val body: Term = tok match {
        case _: `{` => inBracesOrUnit(block())
        case _: `(` => inParensOrUnit(expr())
        case _      => expr()
      }
      val catchopt =
        if (tok.isNot[`catch`]) None
        else {
          next()
          if (tok.isNot[`{`]) Some(expr())
          else inBraces {
            if (tok.is[`case`]) Some(caseClauses())
            else Some(expr())
          }
        }
      val finallyopt = tok match {
        case _: `finally` => next(); Some(expr())
        case _            => None
      }
      catchopt match {
        case None => Term.TryWithCases(body, Nil, finallyopt)
        case Some(cases: List[_]) => Term.TryWithCases(body, cases.asInstanceOf[List[Case]], finallyopt)
        case Some(term: Term) => Term.TryWithTerm(body, term, finallyopt)
        case _ => unreachable
      }
    case _: `while` =>
      next()
      val cond = condExpr()
      newLinesOpt()
      val body = expr()
      Term.While(cond, body)
    case _: `do` =>
      next()
      val body = expr()
      if (tok.is[StatSep]) next()
      accept[`while`]
      val cond = condExpr()
      Term.Do(body, cond)
    case _: `for` =>
      next()
      val enums =
        if (tok.is[`{`]) inBracesOrNil(enumerators())
        else inParensOrNil(enumerators())
      newLinesOpt()
      if (tok.is[`yield`]) {
        next()
        Term.ForYield(enums, expr())
      } else {
        Term.For(enums, expr())
      }
    case _: `return` =>
      next()
      if (tok.is[ExprIntro]) Term.Return(expr())
      else Term.Return()
    case _: `throw` =>
      next()
      Term.Throw(expr())
    case _: `implicit` =>
      implicitClosure(location)
    case _ =>
      var t: Term = postfixExpr()
      if (tok.is[`=`]) {
        t match {
          case ref: Term.Ref =>
            next()
            t = Term.Assign(ref, expr())
          case app: Term.Apply =>
            def decompose(term: Term): (Term, List[List[Term.Arg]]) = term match {
              case Term.Apply(fun, args) =>
                val (core, otherArgss) = decompose(fun)
                (core, args.toList +: otherArgss)
              case term =>
                (term, Nil)
            }
            val (core, argss) = decompose(app)
            next()
            t = Term.Update(core, argss, expr())
          case _ =>
        }
      } else if (tok.is[`:`]) {
        val colonPos = next()
        if (tok.is[`@`]) {
          t = Term.Annotate(t, annots(skipNewLines = false))
        } else {
          t = {
            val tpt = typeOrInfixType(location)
            // this does not correspond to syntax, but is necessary to
            // accept closures. We might restrict closures to be between {...} only.
            Term.Ascribe(t, tpt)
          }
        }
      } else if (tok.is[`match`]) {
        next()
        t = Term.Match(t, inBracesOrNil(caseClauses()))
      }
      // in order to allow anonymous functions as statements (as opposed to expressions) inside
      // templates, we have to disambiguate them from self type declarations - bug #1565
      // The case still missed is unparenthesized single argument, like "x: Int => x + 1", which
      // may be impossible to distinguish from a self-type and so remains an error.  (See #1564)
      def lhsIsTypedParamList() = t match {
        case Term.Tuple(xs) if xs.forall(_.isInstanceOf[Term.Ascribe]) => true
        case _ => false
      }
      if (tok.is[`=>`] && (location != InTemplate || lhsIsTypedParamList)) {
        next()
        t = {
          Term.Function(convertToParams(t), if (location != InBlock) expr() else block())
        }
      }
      t
  }

  /** {{{
   *  Expr ::= implicit Id => Expr
   *  }}}
   */

  def implicitClosure(location: Location): Term.Function = {
    val param0 = convertToParam {
      val name = termName()
      if (tok.isNot[`:`]) name
      else {
        next()
        Term.Ascribe(expr, typeOrInfixType(location))
      }
    }.get
    val param = param0.copy(mods = Mod.Implicit() +: param0.mods)
    accept[`=>`]
    Term.Function(List(param), if (location != InBlock) expr() else block())
  }

  /** {{{
   *  PostfixExpr   ::= InfixExpr [Id [nl]]
   *  InfixExpr     ::= PrefixExpr
   *                  | InfixExpr Id [nl] InfixExpr
   *  }}}
   */
  def postfixExpr(): Term = {
    val ctx  = opctx[List[Term.Arg]]
    val base = ctx.stack

    def loop(top: List[Term.Arg]): List[Term.Arg] =
      if (!tok.is[Ident]) top
      else {
        ctx.push(reduceStack(base, top))
        newLineOptWhenFollowing(_.is[ExprIntro])
        if (tok.is[ExprIntro]) loop(argumentExprsOrPrefixExpr())
        else finishPostfixOp(base, ctx.pop())
      }

    reduceStack(base, loop(prefixExpr() :: Nil)) match {
      case (t: Term) :: Nil => t
      case _                => unreachable
    }
  }

  /** {{{
   *  PrefixExpr   ::= [`-' | `+' | `~' | `!' | `&'] SimpleExpr
   *  }}}
   */
  def prefixExpr(): Term =
    if (!isUnaryOp) simpleExpr()
    else {
      val op = termName()
      if (op.value == "-" && tok.is[NumericLiteral])
        simpleExprRest(literal(isNegated = true), canApply = true)
      else
        Term.ApplyUnary(op, simpleExpr())
    }

  def xmlLiteral(): Term

  /** {{{
   *  SimpleExpr    ::= new (ClassTemplate | TemplateBody)
   *                  |  BlockExpr
   *                  |  SimpleExpr1 [`_']
   *  SimpleExpr1   ::= literal
   *                  |  xLiteral
   *                  |  Path
   *                  |  `(' [Exprs] `)'
   *                  |  SimpleExpr `.' Id
   *                  |  SimpleExpr TypeArgs
   *                  |  SimpleExpr1 ArgumentExprs
   *  }}}
   */
  def simpleExpr(): Term = {
    var canApply = true
    val t: Term =
      tok match {
        case _: Literal =>
          literal()
        case _: Interpolation.Id =>
          interpolateTerm()
        case _: XMLStart =>
          xmlLiteral()
        case _: Ident | _: `this` | _: `super` =>
          path()
        case _: `_ ` =>
          next()
          Term.Placeholder()
        case _: `(` =>
          makeTupleTermParens(commaSeparated(expr()))
        case _: `{` =>
          canApply = false
          blockExpr()
        case _: `new` =>
          canApply = false
          next()
          val (edefs, parents, self, stats, hasStats) = template()
          if (hasStats) Term.New(Templ(edefs, parents, self, stats))
          else Term.New(Templ(edefs, parents, self))
        case _ =>
          syntaxError(s"illegal start of simple expression: $tok")
      }
    simpleExprRest(t, canApply = canApply)
  }

  def simpleExprRest(t: Term, canApply: Boolean): Term = {
    if (canApply) newLineOptWhenFollowedBy[`{`]
    tok match {
      case _: `.` =>
        next()
        simpleExprRest(selector(t), canApply = true)
      case _: `[` =>
        t match {
          case _: Term.Name | _: Term.Select | _: Term.Apply =>
            var app: Term = t
            while (tok.is[`[`])
              app = Term.ApplyType(app, exprTypeArgs())

            simpleExprRest(app, canApply = true)
          case _ =>
            t
        }
      case _: `(` | _: `{` if (canApply) =>
        simpleExprRest(Term.Apply(t, argumentExprs()), canApply = true)
      case _: `_ ` =>
        next()
        Term.Eta(t)
      case _ =>
        t
    }
  }

  /** Translates an Assign(_, _) node to AssignOrNamedArg(_, _) if
   *  the lhs is a simple ident. Otherwise returns unchanged.
   */
  def assignmentToMaybeNamedArg(tree: Term): Term.Arg = tree match {
    case Term.Assign(name: Term.Name, rhs) => Term.Arg.Named(name, rhs)
    case t                                 => t
  }

  def argsToTerm(args: List[Term.Arg]): Term = {
    def loop(args: List[Term.Arg]): List[Term] = args match {
      case Nil                              => Nil
      case (t: Term) :: rest                => t :: loop(rest)
      case (nmd: Term.Arg.Named) :: rest    => Term.Assign(nmd.name, nmd.rhs) :: loop(rest)
      case (rep: Term.Arg.Repeated) :: rest => syntaxError("repeated argument not allowed here", at = offset(rep))
    }
    makeTupleTerm(loop(args))
  }

  def argumentExprsOrPrefixExpr(): List[Term.Arg] =
    if (tok.isNot[`{`] && tok.isNot[`(`]) prefixExpr() :: Nil
    else {
      val args = argumentExprs()
      tok match {
        case _: `.` | _: `[` | _: `(` | _: `{` | _: `_ ` =>
          simpleExprRest(argsToTerm(args), canApply = true) :: Nil
        case _ =>
          args
      }
    }

  /** {{{
   *  ArgumentExprs ::= `(' [Exprs] `)'
   *                  | [nl] BlockExpr
   *  }}}
   */
  def argumentExprs(): List[Term.Arg] = {
    def args(): List[Term.Arg] = commaSeparated {
      expr() match {
        case Term.Ascribe(t, Type.Placeholder(Type.Bounds(None, None))) if isIdentOf("*") =>
          next()
          Term.Arg.Repeated(t)
        case Term.Assign(t: Term.Name, rhs) =>
          Term.Arg.Named(t, rhs)
        case other =>
          other
      }
    }
    tok match {
      case _: `{` => List(blockExpr())
      case _: `(` => inParens(if (tok.is[`)`]) Nil else args())
      case _      => Nil
    }
  }

  /** A succession of argument lists. */
  def multipleArgumentExprs(): List[List[Term.Arg]] = {
    if (tok.isNot[`(`]) Nil
    else argumentExprs() :: multipleArgumentExprs()
  }

  /** {{{
   *  BlockExpr ::= `{' (CaseClauses | Block) `}'
   *  }}}
   */
  def blockExpr(): Term = {
    inBraces {
      if (tok.is[`case`]) Term.PartialFunction(caseClauses())
      else block()
    }
  }

  def mkBlock(stats: List[Stat]): Term = Term.Block(stats)

  /** {{{
   *  Block ::= BlockStatSeq
   *  }}}
   *  @note  Return tree does not carry position.
   */
  def block(): Term = mkBlock(blockStatSeq())

  def caseClause(): Case =
    Case(pattern().asInstanceOf[Pat], guard(), { accept[`=>`]; blockStatSeq() })

  /** {{{
   *  CaseClauses ::= CaseClause {CaseClause}
   *  CaseClause  ::= case Pattern [Guard] `=>' Block
   *  }}}
   */
  def caseClauses(): List[Case] = {
    val cases = caseSeparated { caseClause() }
    if (cases.isEmpty)  // trigger error if there are no cases
      accept[`case`]
    cases
  }

  /** {{{
   *  Guard ::= if PostfixExpr
   *  }}}
   */
  def guard(): Option[Term] =
    if (tok.is[`if`]) { next(); Some(postfixExpr()) }
    else None

  /** {{{
   *  Enumerators ::= Generator {semi Enumerator}
   *  Enumerator  ::=  Generator
   *                |  Guard
   *                |  val Pattern1 `=' Expr
   *  }}}
   */
  def enumerators(): List[Enum] = {
    val enums = new ListBuffer[Enum]
    enums ++= enumerator(isFirst = true)
    while (tok.is[StatSep]) {
      next()
      enums ++= enumerator(isFirst = false)
    }
    enums.toList
  }

  def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true): List[Enum] =
    if (tok.is[`if`] && !isFirst) Enum.Guard(guard().get) :: Nil
    else generator(!isFirst, allowNestedIf)

  /** {{{
   *  Generator ::= Pattern1 (`<-' | `=') Expr [Guard]
   *  }}}
   */
  def generator(eqOK: Boolean, allowNestedIf: Boolean = true): List[Enum] = {
    val hasVal = tok.is[`val`]
    if (hasVal)
      next()

    val pat   = noSeq.pattern1()
    val point = tok.offset
    val hasEq = tok.is[`=`]

    if (hasVal) {
      if (hasEq) deprecationWarning("val keyword in for comprehension is deprecated")
      else syntaxError("val in for comprehension must be followed by assignment")
    }

    if (hasEq && eqOK) next()
    else accept[`<-`]
    val rhs = expr()

    def loop(): List[Enum] =
      if (tok.isNot[`if`]) Nil
      else Enum.Guard(guard().get) :: loop()

    val tail =
      if (allowNestedIf) loop()
      else Nil

    (if (hasEq) Enum.Val(pat.asInstanceOf[Pat], rhs)
     else Enum.Generator(pat.asInstanceOf[Pat], rhs)) :: tail
  }

/* -------- PATTERNS ------------------------------------------- */

  /** Methods which implicitly propagate whether the initial call took
   *  place in a context where sequences are allowed.  Formerly, this
   *  was threaded through methods as boolean seqOK.
   */
  trait SeqContextSensitive extends PatternContextSensitive {
    // is a sequence pattern _* allowed?
    def isSequenceOK: Boolean

    // are we in an XML pattern?
    def isXML: Boolean = false

    def functionArgType(): Type.Arg = paramType()
    def argType(): Type = typ()

    /** {{{
     *  Patterns ::= Pattern { `,' Pattern }
     *  SeqPatterns ::= SeqPattern { `,' SeqPattern }
     *  }}}
     */
    def patterns(): List[Pat.Arg] = commaSeparated(pattern())

    /** {{{
     *  Pattern  ::=  Pattern1 { `|' Pattern1 }
     *  SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
     *  }}}
     */
    def pattern(): Pat.Arg = {
      def loop(pat: Pat.Arg): Pat.Arg =
        if (!isRawBar) pat
        else { next(); Pat.Alternative(pat.asInstanceOf[Pat], pattern().asInstanceOf[Pat]) }
      loop(pattern1())
    }

    /** {{{
     *  Pattern1    ::= varid `:' TypePat
     *                |  `_' `:' TypePat
     *                |  Pattern2
     *  SeqPattern1 ::= varid `:' TypePat
     *                |  `_' `:' TypePat
     *                |  [SeqPattern2]
     *  }}}
     */
    def pattern1(): Pat.Arg = pattern2() match {
      case p @ (_: Term.Name | _: Pat.Wildcard) if tok.is[`:`] =>
        p match {
          case name: Term.Name if !name.isVarPattern =>
            syntaxError("Pattern variables must start with a lower-case letter. (SLS 8.1.1.)")
          case _ =>
        }
        next()
        Pat.Typed(p.asInstanceOf[Pat], compoundType())
      case p => p
    }

    /** {{{
     *  Pattern2    ::=  varid [ @ Pattern3 ]
     *                |   Pattern3
     *  SeqPattern2 ::=  varid [ @ SeqPattern3 ]
     *                |   SeqPattern3
     *  }}}
     */
    def pattern2(): Pat.Arg = {
      val p = pattern3()

      if (tok.isNot[`@`]) p
      else p match {
        case Pat.Wildcard() =>
          next()
          pattern3()
        case name: Term.Name if name.isVarPattern =>
          next()
          Pat.Bind(name, pattern3())
        case _ => p
      }
    }

    /** {{{
     *  Pattern3    ::= SimplePattern
     *                |  SimplePattern {Id [nl] SimplePattern}
     *  }}}
     */
    def pattern3(): Pat.Arg = {
      val top: Pat = simplePattern(badPattern3)
      val ctx = opctx[Pat]
      val base = ctx.stack
      // See SI-3189, SI-4832 for motivation. Cf SI-3480 for counter-motivation.
      def isCloseDelim = tok match {
        case _: `}` => isXML
        case _: `)` => !isXML
        case _      => false
      }
      def checkWildStar: Option[Pat.Arg.SeqWildcard] = top match {
        case Pat.Wildcard() if isSequenceOK && isRawStar => peekingAhead (
          // TODO: used to be Star(top) | EmptyTree, why start had param?
          if (isCloseDelim) Some(Pat.Arg.SeqWildcard())
          else None
        )
        case _ => None
      }
      def loop(top: Pat): Pat = reduceStack(base, top) match {
        case next if isIdentExcept("|") => ctx.push(next); loop(simplePattern(badPattern3))
        case next                      => next
      }
      checkWildStar getOrElse loop(top)
    }

    def badPattern3(): Nothing = {
      def isComma                = tok.is[`,`]
      def isDelimiter            = tok.is[`)`] || tok.is[`}`]
      def isCommaOrDelimiter     = isComma || isDelimiter
      val (isUnderscore, isStar) = opctx[Pat].stack match {
        case OpInfo(Pat.Wildcard(), Term.Name("*"), _) :: _ => (true,   true)
        case OpInfo(_, Term.Name("*"), _) :: _              => (false,  true)
        case _                                              => (false, false)
      }
      def isSeqPatternClose = isUnderscore && isStar && isSequenceOK && isDelimiter
      val preamble = "bad simple pattern:"
      val subtext = (isUnderscore, isStar, isSequenceOK) match {
        case (true,  true, true)  if isComma            => "bad use of _* (a sequence pattern must be the last pattern)"
        case (true,  true, true)  if isDelimiter        => "bad brace or paren after _*"
        case (true,  true, false) if isDelimiter        => "bad use of _* (sequence pattern not allowed)"
        case (false, true, true)  if isDelimiter        => "use _* to match a sequence"
        case (false, true, _)     if isCommaOrDelimiter => "trailing * is not a valid pattern"
        case _                                          => null
      }
      val msg = if (subtext != null) s"$preamble $subtext" else "illegal start of simple pattern"
      // better recovery if don't skip delims of patterns
      val skip = !isCommaOrDelimiter || isSeqPatternClose
      syntaxError(msg)
    }

    /** {{{
     *  SimplePattern    ::= varid
     *                    |  `_'
     *                    |  literal
     *                    |  XmlPattern
     *                    |  StableId  /[TypeArgs]/ [`(' [Patterns] `)']
     *                    |  StableId  [`(' [Patterns] `)']
     *                    |  StableId  [`(' [Patterns] `,' [varid `@'] `_' `*' `)']
     *                    |  `(' [Patterns] `)'
     *  }}}
     *
     * XXX: Hook for IDE
     */
    def simplePattern(): Pat =
      // simple diagnostics for this entry point
      simplePattern(() => syntaxError("illegal start of simple pattern"))
    def simplePattern(onError: () => Nothing): Pat = tok match {
      case _: Ident | _: `this` =>
        val sid = stableId()
        if (tok.is[NumericLiteral]) {
          sid match {
            case Term.Name("-") =>
              return literal(isNegated = true)
            case _ =>
          }
        }
        val targs = tok match {
          case _: `[` => typeArgs()
          case _        => Nil
        }
        (tok, sid) match {
          case (_: `(`, _)                 => Pat.Extract(sid, targs, argumentPatterns())
          case (_, _) if targs.nonEmpty    => syntaxError("pattern must be a value")
          case (_, sid: Term.Ref with Pat) => sid
          case _                           => unreachable
        }
      case _: `_ ` =>
        next()
        Pat.Wildcard()
      case _: Literal =>
        literal()
      case _: Interpolation.Id =>
        interpolatePat()
      case _: `(` =>
        makeTuplePatParens(noSeq.patterns())
      case _: XMLStart =>
        xmlLiteralPattern()
      case _ =>
        onError()
    }
  }
  /** The implementation of the context sensitive methods for parsing outside of patterns. */
  object outPattern extends PatternContextSensitive {
    def argType(): Type = typ()
    def functionArgType(): Type.Arg = paramType()
  }
  /** The implementation for parsing inside of patterns at points where sequences are allowed. */
  object seqOK extends SeqContextSensitive {
    val isSequenceOK = true
  }
  /** The implementation for parsing inside of patterns at points where sequences are disallowed. */
  object noSeq extends SeqContextSensitive {
    val isSequenceOK = false
  }
  /** For use from xml pattern, where sequence is allowed and encouraged. */
  object xmlSeqOK extends SeqContextSensitive {
    val isSequenceOK = true
    override val isXML = true
  }
  /** These are default entry points into the pattern context sensitive methods:
   *  they are all initiated from non-pattern context.
   */
  def typ(): Type      = outPattern.typ()
  def startInfixType() = outPattern.infixType(InfixMode.FirstOp)
  def startModType() = outPattern.annotType()
  def exprTypeArgs()   = outPattern.typeArgs()
  def exprSimpleType() = outPattern.simpleType()

  /** Default entry points into some pattern contexts. */
  def pattern(): Pat.Arg = noSeq.pattern()
  def seqPatterns(): List[Pat.Arg] = seqOK.patterns()
  def xmlSeqPatterns(): List[Pat.Arg] = xmlSeqOK.patterns() // Called from xml parser
  def argumentPatterns(): List[Pat.Arg] = inParens {
    if (tok.is[`)`]) Nil
    else seqPatterns()
  }
  def xmlLiteralPattern(): Pat

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  /** {{{
   *  AccessModifier ::= (private | protected) [AccessQualifier]
   *  AccessQualifier ::= `[' (Id | this) `]'
   *  }}}
   */
  def accessModifierOpt(): Option[Mod] = {
    val originalTok = in.tok
    val (isNaked, isLocal, name) = {
      if (in.tok.is[`private`] || in.tok.is[`protected`]) {
        next()
        if (in.tok.isNot[`[`]) (true, false, None)
        else {
          next()
          val result = if (in.tok.is[`this`]) { next(); (false, true, None) }
                    else (false, false, Some(termName().value))
          accept[`]`]
          result
        }
      } else {
        (false, false, None)
      }
    }
    (originalTok, isNaked, isLocal, name) match {
      case (_: `private`,   true,  false, None)       => Some(Mod.Private())
      case (_: `private`,   false, true,  None)       => Some(Mod.PrivateThis())
      case (_: `private`,   false, false, Some(name)) => Some(Mod.PrivateWithin(name))
      case (_: `protected`, true,  false, None)       => Some(Mod.Protected())
      case (_: `protected`, false, true,  None)       => Some(Mod.ProtectedThis())
      case (_: `protected`, false, false, Some(name)) => Some(Mod.ProtectedWithin(name))
      case _                                          => None
    }
  }

  /** {{{
   *  Modifiers ::= {Modifier}
   *  Modifier  ::= LocalModifier
   *              |  AccessModifier
   *              |  override
   *  }}}
   */
  def modifiers(isLocal: Boolean = false): List[Mod] = {
    def addMod(mods: List[Mod], mod: Mod, advance: Boolean = true): List[Mod] = {
      mods.foreach { m => if (m == mod) syntaxError("repeated modifier", at = offset(mod)) }
      if (advance) next()
      mods :+ mod
    }
    def acceptable = if (isLocal) tok.is[LocalModifier] else true
    def loop(mods: List[Mod]): List[Mod] =
      if (!acceptable) mods
      else (tok match {
        case _: `abstract`  => loop(addMod(mods, Mod.Abstract()))
        case _: `final`     => loop(addMod(mods, Mod.Final()))
        case _: `sealed`    => loop(addMod(mods, Mod.Sealed()))
        case _: `implicit`  => loop(addMod(mods, Mod.Implicit()))
        case _: `lazy`      => loop(addMod(mods, Mod.Lazy()))
        case _: `override`  => loop(addMod(mods, Mod.Override()))
        case _: `private`
           | _: `protected` =>
          mods.filter(_.isAccess).foreach(_ => syntaxError("duplicate private/protected qualifier"))
          val optmod = accessModifierOpt()
          optmod.map { mod => loop(addMod(mods, mod, advance = false)) }.getOrElse(mods)
        case _: `\n` if !isLocal => next(); loop(mods)
        case _                   => mods
      })
    loop(Nil)
  }

  /** {{{
   *  LocalModifiers ::= {LocalModifier}
   *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
   *  }}}
   */
  def localModifiers(): List[Mod] = modifiers(isLocal = true)

  /** {{{
   *  Annotations      ::= {`@' SimpleType {ArgumentExprs}}
   *  ConsrAnnotations ::= {`@' SimpleType ArgumentExprs}
   *  }}}
   */
  def annots(skipNewLines: Boolean): List[Mod.Annot] = readAnnots {
    val t = annot()
    if (skipNewLines) newLineOpt()
    t
  }
  def constructorAnnots(): List[Mod.Annot] = readAnnots {
    Mod.Annot(Ctor.Ref(exprSimpleType(), argumentExprs() :: Nil))
  }

  def annot(): Mod.Annot = {
    val t = exprSimpleType()
    if (tok.is[`(`]) Mod.Annot(Ctor.Ref(t, multipleArgumentExprs()))
    else Mod.Annot(Ctor.Ref(t, Nil))
  }

/* -------- PARAMETERS ------------------------------------------- */

  /** {{{
   *  ParamClauses      ::= {ParamClause} [[nl] `(' implicit Params `)']
   *  ParamClause       ::= [nl] `(' [Params] `)'
   *  Params            ::= Param {`,' Param}
   *  Param             ::= {Annotation} Id [`:' ParamType] [`=' Expr]
   *  ClassParamClauses ::= {ClassParamClause} [[nl] `(' implicit ClassParams `)']
   *  ClassParamClause  ::= [nl] `(' [ClassParams] `)'
   *  ClassParams       ::= ClassParam {`,' ClassParam}
   *  ClassParam        ::= {Annotation}  [{Modifier} (`val' | `var')] Id [`:' ParamType] [`=' Expr]
   *  }}}
   */
  def paramClauses(ownerIsType: Boolean, ownerIsCase: Boolean = false): List[List[Templ.Param]] = {
    var parsedImplicits = false
    def paramClause(): List[Templ.Param] = {
      if (tok.is[`)`])
        return Nil

      if (tok.is[`implicit`]) {
        next()
        parsedImplicits = true
      }
      commaSeparated(param(ownerIsCase, ownerIsType, isImplicit = parsedImplicits))
    }
    val paramss = new ListBuffer[List[Templ.Param]]
    newLineOptWhenFollowedBy[`(`]
    while (!parsedImplicits && tok.is[`(`]) {
      next()
      paramss += paramClause()
      accept[`)`]
      newLineOptWhenFollowedBy[`(`]
    }
    paramss.toList
  }

  /** {{{
   *  ParamType ::= Type | `=>' Type | Type `*'
   *  }}}
   */
  def paramType(): Type.Arg = tok match {
    case _: `=>` =>
      next()
      Type.Arg.ByName(typ())
    case _ =>
      val t = typ()
      if (!isRawStar) t
      else {
        next()
        Type.Arg.Repeated(t)
      }
  }

  def param(ownerIsCase: Boolean, ownerIsType: Boolean, isImplicit: Boolean): Templ.Param = {
    var mods: List[Mod] = annots(skipNewLines = false)
    if (isImplicit) mods ++= List(Mod.Implicit())
    val (isValParam, isVarParam) = (ownerIsType && tok.is[`val`], ownerIsType && tok.is[`var`])
    if (isValParam || isVarParam) next()
    if (ownerIsType) {
      mods ++= modifiers()
      mods.getAll[Mod.Lazy].foreach { m =>
        syntaxError("lazy modifier not allowed here. Use call-by-name parameters instead", at = offset(m))
      }
    }
    val name = termName()
    val tpt = {
      accept[`:`]
      val tpt = paramType()
      if (tpt.isInstanceOf[Type.Arg.ByName]) {
        def mayNotBeByName(subj: String) =
          syntaxError(s"$subj parameters may not be call-by-name")
        val isLocalToThis: Boolean =
          if (ownerIsCase) (mods.access match {
            case Some(Mod.PrivateThis()) => true
            case _ => false
          }) else (mods.access match {
            case Some(Mod.PrivateThis()) => true
            case None if !isValParam && !isVarParam => true
            case _ => false
          })
        if (ownerIsType && !isLocalToThis) {
          if (isVarParam)
            mayNotBeByName("`var'")
          else
            mayNotBeByName("`val'")
        } else if (isImplicit)
          mayNotBeByName("implicit")
      }
      tpt
    }
    val default =
      if (tok.isNot[`=`]) None
      else {
        next()
        Some(expr())
      }
    if (isValParam) Templ.Param.Val(mods, Some(name), Some(tpt), default)
    else if (isVarParam) Templ.Param.Var(mods, Some(name), Some(tpt), default)
    else Term.Param(mods, Some(name), Some(tpt), default)
  }

  /** {{{
   *  TypeParamClauseOpt    ::= [TypeParamClause]
   *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
   *  VariantTypeParam      ::= {Annotation} [`+' | `-'] TypeParam
   *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
   *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
   *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds {<% Type} {":" Type}
   *  }}}
   */
  def typeParamClauseOpt(ownerIsType: Boolean, ctxBoundsAllowed: Boolean): List[Type.Param] = {
    newLineOptWhenFollowedBy[`[`]
    if (tok.isNot[`[`]) Nil
    else inBrackets(commaSeparated {
      val mods = annots(skipNewLines = true)
      typeParam(mods, ownerIsType, ctxBoundsAllowed)
    })
  }

  def typeParam(annots: List[Mod.Annot], ownerIsType: Boolean, ctxBoundsAllowed: Boolean): Type.Param = {
    val mods =
      if (ownerIsType && tok.is[Ident]) {
        if (isIdentOf("+")) {
          next()
          annots :+ Mod.Covariant()
        } else if (isIdentOf("-")) {
          next()
          annots :+ Mod.Contravariant()
        } else annots
      } else annots
    val nameopt =
      if (tok.is[Ident]) Some(typeName())
      else if (tok.is[`_ `]) { next(); None }
      else syntaxError("identifier or `_' expected")
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    val typeBounds = this.typeBounds()
    val contextBounds = new ListBuffer[Type]
    val viewBounds = new ListBuffer[Type]
    if (ctxBoundsAllowed) {
      while (tok.is[`<%`]) {
        // TODO: syntax profile?
        // if (settings.future) {
        //   val msg = ("Use an implicit parameter instead.\n" +
        //              "Example: Instead of `def f[A <% Int](a: A)` " +
        //              "use `def f[A](a: A)(implicit ev: A => Int)`.")
        //   deprecationWarning(s"View bounds are deprecated. $msg")
        // }
        next()
        viewBounds += typ()
      }
      while (tok.is[`:`]) {
        next()
        contextBounds += typ()
      }
    }
    Type.Param(mods, nameopt, tparams, contextBounds.toList, viewBounds.toList, typeBounds)
  }

  /** {{{
   *  TypeBounds ::= [`>:' Type] [`<:' Type]
   *  }}}
   */
  def typeBounds() =
    Type.Bounds(bound[`>:`], bound[`<:`])

  def bound[T: TokMetadata]: Option[Type] =
    if (tok.is[T]) { next(); Some(typ()) } else None

/* -------- DEFS ------------------------------------------- */


  /** {{{
   *  Import  ::= import ImportExpr {`,' ImportExpr}
   *  }}}
   */
  def importStmt(): Import = {
    accept[`import`]
    Import(commaSeparated(importClause()))
  }

  /** {{{
   *  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
   *  }}}
   */
  def importClause(): Import.Clause = {
    val sid = stableId()
    def dotselectors = { accept[`.`]; Import.Clause(sid, importSelectors()) }
    sid match {
      case Term.Select(sid: Term.Ref, name: Term.Name) if sid.isStableId =>
        if (tok.is[`.`]) dotselectors
        else Import.Clause(sid, Import.Name(name.value) :: Nil)
      case _ => dotselectors
    }
  }

  /** {{{
   *  ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
   *  }}}
   */
  def importSelectors(): List[Selector] =
    if (tok.isNot[`{`]) List(importWildcardOrName())
    else inBraces(commaSeparated(importSelector()))

  def importWildcardOrName(): Selector =
    if (tok.is[`_ `]) { next(); Import.Wildcard() }
    else { val name = termName(); Import.Name(name.value) }

  /** {{{
   *  ImportSelector ::= Id [`=>' Id | `=>' `_']
   *  }}}
   */
  def importSelector(): Selector = {
    importWildcardOrName() match {
      case from: Import.Name if tok.is[`=>`] =>
        next()
        importWildcardOrName() match {
          case to: Import.Name     => Import.Rename(from.value, to.value)
          case to: Import.Wildcard => Import.Unimport(from.value)
          case _                   => unreachable
        }
      case other => other
    }
  }


  /** {{{
   *  Def    ::= val PatDef
   *           | var PatDef
   *           | def FunDef
   *           | type [nl] TypeDef
   *           | TmplDef
   *  Dcl    ::= val PatDcl
   *           | var PatDcl
   *           | def FunDcl
   *           | type [nl] TypeDcl
   *  }}}
   */
  def defOrDclOrCtor(mods: List[Mod]): Stat = {
    mods.getAll[Mod.Lazy].foreach { m =>
      if (tok.isNot[`val`]) syntaxError("lazy not allowed here. Only vals can be lazy", at = offset(m))
    }
    tok match {
      case _: `val` | _: `var` =>
        patDefOrDcl(mods)
      case _: `def` =>
        funDefOrDclOrCtor(mods)
      case _: `type` =>
        typeDefOrDcl(mods)
      case _ =>
        tmplDef(mods)
    }
  }

  def nonLocalDefOrDcl: Stat = {
    val anns = annots(skipNewLines = true)
    val mods = anns ++ modifiers()
    defOrDclOrCtor(mods) match {
      case s if s.isTemplateStat => s
      case other                 => syntaxError("is not a valid template statement", at = offset(other))
    }
  }

  /** {{{
   *  PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
   *  ValDcl ::= Id {`,' Id} `:' Type
   *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
   *  }}}
   */
  def patDefOrDcl(mods: List[Mod]): Stat = {
    val isMutable = tok.is[`var`]
    next()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2().asInstanceOf[Pat])
    val tp: Option[Type] = typedOpt()

    if (tp.isEmpty || tok.is[`=`]) {
      accept[`=`]
      val rhs =
        if (tok.is[`_ `] && tp.nonEmpty && isMutable && lhs.forall(_.isInstanceOf[Term.Name])) {
          next()
          None
        } else Some(expr())

      if (isMutable) Defn.Var(mods, lhs, tp, rhs)
      else Defn.Val(mods, lhs, tp, rhs.get)
    } else {
      mods.getAll[Mod.Lazy].foreach { m => syntaxError("lazy values may not be abstract", at = offset(m)) }
      val ids = lhs.map {
        case name: Term.Name => name
        case other           => syntaxError("pattern definition may not be abstract", at = offset(other))
      }

      if (isMutable) Decl.Var(mods, ids, tp.get)
      else Decl.Val(mods, ids, tp.get)
    }
  }

  /** {{{
   *  FunDef ::= FunSig [`:' Type] `=' [`macro'] Expr
   *          |  FunSig [nl] `{' Block `}'
   *          |  `this' ParamClause ParamClauses
   *                 (`=' ConstrExpr | [nl] ConstrBlock)
   *  FunDcl ::= FunSig [`:' Type]
   *  FunSig ::= id [FunTypeParamClause] ParamClauses
   *  }}}
   */
  def funDefOrDclOrCtor(mods: List[Mod]): Stat = {
    next()
    if (tok.isNot[`this`]) funDefRest(mods, termName())
    else {
      next()
      // TODO: ownerIsType = true is most likely a bug here
      // secondary constructors can't have val/var parameters
      val paramss = paramClauses(ownerIsType = true).asInstanceOf[Seq[Seq[Term.Param]]]
      newLineOptWhenFollowedBy[`{`]
      val (argss, stats) = tok match {
        case _: `{` => constrBlock()
        case _      => accept[`=`]; constrExpr()
      }
      Ctor.Secondary(mods, paramss, argss, stats)
    }
  }

  def funDefRest(mods: List[Mod], name: Term.Name): Stat = {
    def warnProcedureDeprecation =
      deprecationWarning(s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit`.")
    val tparams = typeParamClauseOpt(ownerIsType = false, ctxBoundsAllowed = true)
    val paramss = paramClauses(ownerIsType = false).asInstanceOf[Seq[Seq[Term.Param]]]
    newLineOptWhenFollowedBy[`{`]
    var restype = fromWithinReturnType(typedOpt())
    if (tok.is[StatSep] || tok.is[`}`]) {
      if (restype.isEmpty) {
        warnProcedureDeprecation
        Decl.Procedure(mods, name, tparams, paramss)
      } else
        Decl.Def(mods, name, tparams, paramss, restype.get)
    } else if (restype.isEmpty && tok.is[`{`]) {
      warnProcedureDeprecation
      Defn.Procedure(mods, name, tparams, paramss, {
        accept[`{`]
        var r = blockStatSeq()
        accept[`}`]
        r
      })
    } else {
      var isMacro = false
      val rhs = {
        if (tok.is[`=`]) {
          next()
          isMacro = tok.is[`macro`]
          if (isMacro) next()
        } else {
          accept[`=`]
        }
        expr()
      }
      if (isMacro) restype match {
        case Some(restype) => Defn.Macro(mods, name, tparams, paramss, restype, rhs)
        case None          => syntaxError("macros must have explicitly specified return types")
      } else Defn.Def(mods, name, tparams, paramss, restype, rhs)
    }
  }

  /** {{{
   *  ConstrExpr      ::=  SelfInvocation
   *                    |  ConstrBlock
   *  }}}
   */
  def constrExpr(): (List[List[Term.Arg]], List[Stat]) =
    if (tok.is[`{`]) constrBlock()
    else (selfInvocation(), Nil)

  /** {{{
   *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
   *  }}}
   */
  def selfInvocation(): List[List[Term.Arg]] = {
    accept[`this`]
    newLineOptWhenFollowedBy[`{`]
    var t: List[List[Term.Arg]] = List(argumentExprs())
    newLineOptWhenFollowedBy[`{`]
    while (tok.is[`(`] || tok.is[`{`]) {
      t = t :+ argumentExprs()
      newLineOptWhenFollowedBy[`{`]
    }
    t
  }

  /** {{{
   *  ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
   *  }}}
   */
  def constrBlock(): (List[List[Term.Arg]], List[Stat]) = {
    next()
    val argss = selfInvocation()
    val stats =
      if (!tok.is[StatSep]) Nil
      else { next(); blockStatSeq() }
    accept[`}`]
    (argss, stats)
  }

  /** {{{
   *  TypeDef ::= type Id [TypeParamClause] `=' Type
   *            | FunSig `=' Expr
   *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
   *  }}}
   */
  def typeDefOrDcl(mods: List[Mod]): Member.Type with Stat = {
    next()
    newLinesOpt()
    val name = typeName()
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    tok match {
      case _: `=` =>
        next()
        Defn.Type(mods, name, tparams, typ())
      case _: `>:` | _: `<:` | _: `,` | _: `}` | _: StatSep =>
        Decl.Type(mods, name, tparams, typeBounds())
      case _ =>
        syntaxError("`=', `>:', or `<:' expected")
    }
  }

  /** Hook for IDE, for top-level classes/objects. */
  def topLevelTmplDef: Member.Templ =
    tmplDef(annots(skipNewLines = true) ++ modifiers())

  /** {{{
   *  TmplDef ::= [case] class ClassDef
   *            |  [case] object ObjectDef
   *            |  [override] trait TraitDef
   *  }}}
   */
  def tmplDef(mods: List[Mod]): Member.Templ = {
    mods.getAll[Mod.Lazy].foreach { m => syntaxError("classes cannot be lazy", at = offset(m)) }
    tok match {
      case _: `trait` =>
        traitDef(mods)
      case _: `class ` =>
        classDef(mods)
      case _: `case` if ahead(tok.is[`class `]) =>
        next()
        classDef(mods :+ Mod.Case())
      case _: `object` =>
        objectDef(mods)
      case _: `case` if ahead(tok.is[`object`])=>
        next()
        objectDef(mods :+ Mod.Case())
      case _ =>
        syntaxError(s"expected start of definition")
    }
  }

  /** {{{
   *  ClassDef ::= Id [TypeParamClause] {Annotation}
   *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
   *  }}}
   */
  def classDef(mods: List[Mod]): Defn.Class = {
    next()
    // TODO:
    // if (ofCaseClass && tok.isNot[`(`])
    //  syntaxError(tok.offset, "case classes without a parameter list are not allowed;\n"+
    //                             "use either case objects or case classes with an explicit `()' as a parameter list.")
    // TODO:
    // if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods.isImplicit)))) {
    //  tok match {
    //    case _: `[` => syntaxError("no type parameters allowed here")
    //    case _: EOF => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
    //    case _      => syntaxError(start, "auxiliary constructor needs non-implicit parameter list")
    //  }
    // }

    Defn.Class(mods, typeName(),
               typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = true),
               primaryCtor(ownerIsCase = mods.has[Mod.Case]), templateOpt(OwnedByClass))
  }

  def primaryCtor(ownerIsCase: Boolean): Ctor.Primary = {
    val mods = constructorAnnots() ++ accessModifierOpt()
    val paramss = paramClauses(ownerIsType = true, ownerIsCase)
    Ctor.Primary(mods, paramss)
  }

  /** {{{
   *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
   *  }}}
   */
  def traitDef(mods: List[Mod]): Defn.Trait = {
    next()
    Defn.Trait(mods, typeName(),
               typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false),
               templateOpt(OwnedByTrait))
  }

  sealed trait TemplateOwner {
    def isTerm = this eq OwnedByObject
    def isTrait = this eq OwnedByTrait
  }
  object OwnedByTrait extends TemplateOwner
  object OwnedByClass extends TemplateOwner
  object OwnedByObject extends TemplateOwner

  /** {{{
   *  ObjectDef       ::= Id ClassTemplateOpt
   *  }}}
   */
  def objectDef(mods: List[Mod]): Defn.Object = {
    next()
    Defn.Object(mods, termName(), templateOpt(OwnedByObject))
  }

  /** {{{
   *  ClassParents       ::= ModType {`(' [Exprs] `)'} {with ModType}
   *  TraitParents       ::= ModType {with ModType}
   *  }}}
   */
  def templateParents(): List[Ctor.Ref] = {
    val parents = new ListBuffer[Ctor.Ref]
    def readAppliedParent() = {
      val parentTpe = startModType()
      parents += (tok match {
        case _: `(` => Ctor.Ref(parentTpe, multipleArgumentExprs())
        case _      => Ctor.Ref(parentTpe, Nil)
      })
    }
    readAppliedParent()
    while (tok.is[`with`]) { next(); readAppliedParent() }
    parents.toList
  }

  /** {{{
   *  ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
   *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
   *  EarlyDefs     ::= `{' [EarlyDef {semi EarlyDef}] `}'
   *  EarlyDef      ::= Annotations Modifiers PatDef
   *  }}}
   */
  def template(): (List[Stat], List[Ctor.Ref], Term.Param, List[Stat], Boolean) = {
    newLineOptWhenFollowedBy[`{`]
    if (tok.is[`{`]) {
      // @S: pre template body cannot stub like post body can!
      val (self, body) = templateBody(isPre = true)
      if (tok.is[`with`] && self.name.isEmpty && self.decltpe.isEmpty) {
        val edefs = body.map(ensureEarlyDef)
        next()
        val parents = templateParents()
        val (self1, body1, hasStats) = templateBodyOpt(parenMeansSyntaxError = false)
        (edefs, parents, self1, body1, hasStats)
      } else {
        (Nil, Nil, self, body, true)
      }
    } else {
      val parents = templateParents()
      val (self, body, hasStats) = templateBodyOpt(parenMeansSyntaxError = false)
      (Nil, parents, self, body, hasStats)
    }
  }

  def ensureEarlyDef(tree: Stat): Stat = tree match {
    case v: Defn.Val => v
    case v: Defn.Var => v
    case t: Defn.Type =>
      syntaxError("early type members are not allowed any longer. " +
                  "Move them to the regular body: the semantics are the same.",
                  at = offset(t))
    case other =>
      syntaxError("not a valid early definition", at = offset(other))
  }

  /** {{{
   *  ClassTemplateOpt ::= `extends' ClassTemplate | [[`extends'] TemplateBody]
   *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[`extends'] TemplateBody] | `<:' TemplateBody
   *  TraitExtends     ::= `extends' | `<:'
   *  }}}
   */
  def templateOpt(owner: TemplateOwner): Templ = {
    val (early, parents, self, body, hasStats) = (
      if (tok.is[`extends`] /* || tok.is[`<:`] && mods.isTrait */) {
        next()
        template()
      }
      else {
        newLineOptWhenFollowedBy[`{`]
        val (self, body, hasStats) = templateBodyOpt(parenMeansSyntaxError = owner.isTrait || owner.isTerm)
        (Nil, Nil, self, body, hasStats)
      }
    )
    if (hasStats) Templ(early, parents, self, body)
    else Templ(early, parents, self)
  }

/* -------- TEMPLATES ------------------------------------------- */

  /** {{{
   *  TemplateBody ::= [nl] `{' TemplateStatSeq `}'
   *  }}}
   * @param isPre specifies whether in early initializer (true) or not (false)
   */
  def templateBody(isPre: Boolean): (Term.Param, List[Stat]) =
    inBraces(templateStatSeq(isPre = isPre))

  def templateBodyOpt(parenMeansSyntaxError: Boolean): (Term.Param, List[Stat], Boolean) = {
    newLineOptWhenFollowedBy[`{`]
    if (tok.is[`{`]) {
      val (self, stats) = templateBody(isPre = false)
      (self, stats, true)
    } else {
      if (tok.is[`(`]) {
        if (parenMeansSyntaxError) syntaxError("traits or objects may not have parameters")
        else error("unexpected opening parenthesis")
      }
      (Term.Param(Nil, None, None, None), Nil, false)
    }
  }

  /** {{{
   *  Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
   *  }}}
   */
  def refinement(): List[Stat] = inBraces(refineStatSeq())

  def existentialStats(): List[Stat] = refinement() map {
    case stat if stat.isExistentialStat => stat
    case other                          => syntaxError("not a legal existential clause", at = offset(other))
  }

/* -------- STATSEQS ------------------------------------------- */

  def statSeq[T <: Tree](statpf: PartialFunction[Tok, T],
                         errorMsg: String = "illegal start of definition"): List[T] = {
    val stats = new ListBuffer[T]
    while (!tok.is[StatSeqEnd]) {
      if (statpf.isDefinedAt(tok)) stats += statpf(tok)
      else if (!tok.is[StatSep]) syntaxError(errorMsg)
      acceptStatSepOpt()
    }
    stats.toList
  }

  /** {{{
   *  TopStatSeq ::= TopStat {semi TopStat}
   *  TopStat ::= Annotations Modifiers TmplDef
   *            | Packaging
   *            | package object objectDef
   *            | Import
   *            |
   *  }}}
   */
  def topStatSeq(): List[Stat] = statSeq(topStat, errorMsg = "expected class or object definition")
  def topStat: PartialFunction[Tok, Stat] = {
    case _: `package `  =>
      next()
      packageOrPackageObject()
    case _: `import` =>
      importStmt()
    case _: `@` | _: TemplateIntro | _: Modifier =>
      topLevelTmplDef
  }

  /** {{{
   *  TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStats
   *  }}}
   * @param isPre specifies whether in early initializer (true) or not (false)
   */
  def templateStatSeq(isPre : Boolean): (Term.Param, List[Stat]) = {
    var self: Term.Param = Term.Param(Nil, None, None, None)
    var firstOpt: Option[Term] = None
    if (tok.is[ExprIntro]) {
      val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
      if (tok.is[`=>`]) {
        first match {
          case name: Name =>
            self = Term.Param(Nil, Some(name.toTermName), None, None)
          case Term.Placeholder() =>
            self = Term.Param(Nil, None, None, None)
          case Term.This(None) =>
            self = Term.Param(Nil, None, None, None)
          case Term.Ascribe(name: Name, tpt) =>
            self = Term.Param(Nil, Some(name.toTermName), Some(tpt), None)
          case Term.Ascribe(Term.Placeholder(), tpt) =>
            self = Term.Param(Nil, None, Some(tpt), None)
          case Term.Ascribe(tree @ Term.This(None), tpt) =>
            self = Term.Param(Nil, None, Some(tpt), None)
          case _ =>
        }
        next()
      } else {
        firstOpt = Some(first)
        acceptStatSepOpt()
      }
    }
    (self, firstOpt ++: templateStats())
  }

  /** {{{
   *  TemplateStats    ::= TemplateStat {semi TemplateStat}
   *  TemplateStat     ::= Import
   *                     | Annotations Modifiers Def
   *                     | Annotations Modifiers Dcl
   *                     | Expr1
   *                     | super ArgumentExprs {ArgumentExprs}
   *                     |
   *  }}}
   */
  def templateStats(): List[Stat] = statSeq(templateStat)
  def templateStat: PartialFunction[Tok, Stat] = {
    case _: `import` =>
      importStmt()
    case _: DefIntro | _: Modifier | _: `@` =>
      nonLocalDefOrDcl
    case _: ExprIntro =>
      expr(InTemplate)
  }

  /** {{{
   *  RefineStatSeq    ::= RefineStat {semi RefineStat}
   *  RefineStat       ::= Dcl
   *                     | type TypeDef
   *                     |
   *  }}}
   */
  def refineStatSeq(): List[Stat] = {
    val stats = new ListBuffer[Stat]
    while (!tok.is[StatSeqEnd]) {
      stats ++= refineStat()
      if (tok.isNot[`}`]) acceptStatSep()
    }
    stats.toList
  }

  def refineStat(): Option[Stat] =
    if (tok.is[DclIntro]) {
      defOrDclOrCtor(Nil) match {
        case stat if stat.isRefineStat => Some(stat)
        case other                     => syntaxError("is not a valid refinement declaration", at = offset(other))
      }
    } else if (!tok.is[StatSep]) {
      syntaxError(
        "illegal start of declaration"+
        (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
         else ""))
    } else None

  def localDef(implicitMod: Option[Mod.Implicit]): Stat = {
    val mods = (implicitMod ++: annots(skipNewLines = true)) ++ localModifiers()
    if (mods forall { case _: Mod.Implicit | _: Mod.Lazy | _: Mod.Annot => true; case _ => false })
      (defOrDclOrCtor(mods) match {
        case stat if stat.isBlockStat => stat
        case other                    => syntaxError("is not a valid block statement", at = offset(other))
      })
    else
      (tmplDef(mods) match {
        case stat if stat.isBlockStat => stat
        case other                    => syntaxError("is not a valid block statement", at = offset(other))
      })
  }

  /** {{{
   *  BlockStatSeq ::= { BlockStat semi } [ResultExpr]
   *  BlockStat    ::= Import
   *                 | Annotations [implicit] [lazy] Def
   *                 | Annotations LocalModifiers TmplDef
   *                 | Expr1
   *                 |
   *  }}}
   */
  def blockStatSeq(): List[Stat] = {
    val stats = new ListBuffer[Stat]
    while (!tok.is[StatSeqEnd] && !tok.is[CaseDefEnd]) {
      if (tok.is[`import`]) {
        stats += importStmt()
        acceptStatSepOpt()
      }
      else if (tok.is[DefIntro] || tok.is[LocalModifier] || tok.is[`@`]) {
        if (tok.is[`implicit`]) {
          next()
          if (tok.is[Ident]) stats += implicitClosure(InBlock)
          else stats += localDef(Some(Mod.Implicit()))
        } else {
          stats += localDef(None)
        }
        acceptStatSepOpt()
      }
      else if (tok.is[ExprIntro]) {
        stats += expr(InBlock)
        if (!tok.is[CaseDefEnd]) acceptStatSep()
      }
      else if (tok.is[StatSep]) {
        next()
      }
      else {
        val addendum = if (tok.is[Modifier]) " (no modifiers allowed here)" else ""
        syntaxError("illegal start of statement" + addendum)
      }
    }
    stats.toList
  }


  def packageOrPackageObject(): Stat =
    if (tok.is[`object`]) {
      next()
      packageObject()
    } else {
      Pkg(qualId(), inBracesOrNil(topStatSeq()), hasBraces = true)
    }

  def packageObject(): Pkg.Object =
    Pkg.Object(Nil, termName(), templateOpt(OwnedByObject))

  /** {{{
   *  CompilationUnit ::= {package QualId semi} TopStatSeq
   *  }}}
   */
  def compilationUnit(): Source = {
    def packageStats(): (List[Term.Ref], List[Stat])  = {
      val refs = new ListBuffer[Term.Ref]
      val ts = new ListBuffer[Stat]
      while (tok.is[`;`] || tok.is[`\n`]) next()
      if (tok.is[`package `]) {
        next()
        if (tok.is[`object`]) {
          next()
          ts += packageObject()
          if (tok.isNot[EOF]) {
            acceptStatSep()
            ts ++= topStatSeq()
          }
        } else {
          val qid = qualId()

          if (tok.is[EOF]) {
            refs += qid
          } else if (tok.is[StatSep]) {
            next()
            refs += qid
            val (nrefs, nstats) = packageStats()
            refs ++= nrefs
            ts ++= nstats
          } else {
            ts += inBraces(Pkg(qid, topStatSeq(), hasBraces = true))
            acceptStatSepOpt()
            ts ++= topStatSeq()
          }
        }
      } else {
        ts ++= topStatSeq()
      }
      (refs.toList, ts.toList)
    }

    val (refs, stats) = packageStats()
    refs match {
      case Nil          => Source(stats)
      case init :+ last => Source(init.foldRight(Pkg(last, stats, hasBraces = false)) { (ref, acc) => Pkg(ref, acc :: Nil, hasBraces = false) } :: Nil)
    }
  }
}

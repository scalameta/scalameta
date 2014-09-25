package scala.meta
package syntactic.parsers

import scala.collection.{ mutable, immutable }
import scala.reflect.ClassTag
import mutable.{ ListBuffer, StringBuilder }
import Aux._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.unreachable
import Chars.{isOperatorPart, isScalaLetter}

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
      case _: Term.Name | Term.Select(_: Qual.Super, _) => true
      case Term.Select(qual: Term.Ref, _)              => qual.isPath
      case _                                           => false
    }
  }
  implicit class RichMods(val mods: List[Mod]) extends AnyVal {
    def has[T <: Mod](implicit tag: ClassTag[T]): Boolean =
      mods.exists { _.getClass == tag.runtimeClass }
    def getAll[T <: Mod](implicit tag: ClassTag[T]): List[T] =
      mods.collect { case m if m.getClass == tag.runtimeClass => m.asInstanceOf[T] }
    def access: Option[Mod.Access] = mods.collectFirst {
      case acc: Mod.Private   => acc
      case acc: Mod.Protected => acc
    }
  }
}
import SyntacticInfo._

case class ParseAbort(msg: String) extends Exception(s"abort: $msg")
case class ParseSyntaxError(offset: Offset, msg: String) extends Exception(s"syntax error at $offset: $msg")
case class ParseIncompleteInputError(msg: String) extends Exception("incomplete input: $msg")

class Parser(val source: Source) extends AbstractParser {
  def this(code: String) = this(Source.String(code))
  /** The parse starting point depends on whether the source file is self-contained:
   *  if not, the AST will be supplemented.
   */
  def parseStartRule = () => compilationUnit()

  var in: TokIterator = new TokIterator(source.tokens)

  // warning don't stop parsing
  // TODO:
  def warning(offset: Offset, msg: String): Unit = ()
  def deprecationWarning(offset: Offset, msg: String): Unit = ()

  // errors do
  def abort(msg: String): Nothing = throw ParseAbort(msg)
  def syntaxError(offset: Offset, msg: String): Nothing = throw ParseSyntaxError(offset, msg)
  def incompleteInputError(msg: String): Nothing = throw ParseIncompleteInputError(msg)

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
  var in: TokIterator
  val source: Source
  implicit val origin: Origin = Origin.Source(source)

  /** Scoping operator used to temporarily look into the future.
   *  Backs up scanner data before evaluating a block and restores it after.
   */
  @inline final def lookingAhead[T](body: => T): T = {
    val forked = in.fork
    in.next()
    try body finally in = forked
  }

  /** Perform an operation while peeking ahead.
   *  Recover to original state in case of exception.
   */
  @inline def peekingAhead[T](tree: => T): T = {
    val forked = in.fork
    in.next()    
    // try it, in case it is recoverable
    try tree catch { case e: Exception => in = forked ; throw e }
  }

  def parseStartRule: () => CompUnit

  def parseRule[T](rule: this.type => T): T = {
    val t = rule(this)
    accept(Tok.EOF)
    t
  }

  /** This is the general parse entry point.
   */
  def parseTopLevel(): CompUnit = parseRule(_.parseStartRule())

  /** This is the alternative entry point for repl, script runner, toolbox and parsing in macros.
   */
  def parseStats(): List[Stmt.Template] = parseRule(_.templateStats())

  /** These are alternative entry points for the three main tree flavors.
   */
  def parseTerm(): Term = parseRule(_.expr())
  def parseType(): Type = parseRule(_.typ())
  def parsePat(): Pat = parseRule(_.pattern())

  /** These are alternative entry points for quasiquotes.
   */
  def parseQ(): Stmt = parseRule(parser => parser.statSeq(parser.templateStat.orElse(parser.topStat))) match {
    case stat :: Nil => stat
    case stats if stats.forall(_.isInstanceOf[Stmt.Block]) => Term.Block(stats.asInstanceOf[List[Stmt.Block]])
    // TODO: haha, CompUnit itself is not a statП
    // case stats if stats.forall(_.isInstanceOf[Stmt.TopLevel]) => Aux.CompUnit(stats.asInstanceOf[List[Stmt.TopLevel]])
    case _ => syntaxError("these statements can't be mixed together")
  }
  def parseT(): Param.Type = parseRule(_.paramType())
  def parseP(): Pat = parsePat()
  def parseParam(): Param = ???
  def parseTypeParam(): TypeParam = ???
  def parseArg(): Arg = ???
  def parseEnum(): Enum = ???
  def parseMod(): Mod = ???
  def parseCase(): Aux.Case = parseRule(_.caseClause())
  def parseParent(): Aux.Parent = ???
  def parseTemplate(): Aux.Template = ???
  def parseSelf(): Aux.Self = ???

/* ------------- PARSER COMMON -------------------------------------------- */

  /** Methods inParensOrError and similar take a second argument which, should
   *  the next token not be the expected opener (e.g. Tok.`(`) will be returned
   *  instead of the contents of the groupers.  However in all cases accept(Tok.`(`)
   *  will be called, so a parse error will still result.  If the grouping is
   *  optional, in.tok should be tested before calling these methods.
   */
  @inline final def inParens[T](body: => T): T = {
    accept(Tok.`(`)
    val ret = body
    accept(Tok.`)`)
    ret
  }
  @inline final def inParensOrError[T](body: => T, alt: T): T =
    if (in.tok == Tok.`(`) inParens(body)
    else { accept(Tok.`(`) ; alt }

  @inline final def inParensOrUnit[T, Ret >: Lit](body: => Ret): Ret = inParensOrError(body, Lit.Unit())
  @inline final def inParensOrNil[T](body: => List[T]): List[T] = inParensOrError(body, Nil)

  @inline final def inBraces[T](body: => T): T = {
    accept(Tok.`{`)
    val ret = body
    accept(Tok.`}`)
    ret
  }
  @inline final def inBracesOrError[T](body: => T, alt: T): T =
    if (in.tok == Tok.`{`) inBraces(body)
    else { accept(Tok.`{`) ; alt }

  @inline final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOrError(body, Nil)
  @inline final def inBracesOrUnit[T](body: => Term): Term = inBracesOrError(body, Lit.Unit())
  @inline final def dropAnyBraces[T](body: => T): T =
    if (in.tok == Tok.`{`) inBraces(body)
    else body

  @inline final def inBrackets[T](body: => T): T = {
    accept(Tok.`[`)
    val ret = body
    accept(Tok.`]`)
    ret
  }

/* ------------- ERROR HANDLING ------------------------------------------- */

  val assumedClosingParens = mutable.Map(Tok.`)` -> 0, Tok.`]` -> 0, Tok.`}` -> 0)

  private var inFunReturnType = false
  @inline private def fromWithinReturnType[T](body: => T): T = {
    val saved = inFunReturnType
    inFunReturnType = true
    try body
    finally inFunReturnType = saved
  }

  protected def skip(targetToken: Tok) {
    var nparens = 0
    var nbraces = 0
    while (true) {
      in.tok match {
        case Tok.EOF =>
          return
        case Tok.`;` =>
          if (nparens == 0 && nbraces == 0) return
        case Tok.`\n` =>
          if (nparens == 0 && nbraces == 0) return
        case Tok.`\n\n` =>
          if (nparens == 0 && nbraces == 0) return
        case Tok.`)` =>
          nparens -= 1
        case Tok.`}` =>
          if (nbraces == 0) return
          nbraces -= 1
        case Tok.`(` =>
          nparens += 1
        case Tok.`{` =>
          nbraces += 1
        case _ =>
      }
      if (targetToken == in.tok && nparens == 0 && nbraces == 0) return
      in.next()
    }
  }

  def warning(offset: Offset, msg: String): Unit
  def deprecationWarning(offset: Offset, msg: String) : Unit

  def abort(msg: String): Nothing
  def incompleteInputError(msg: String): Nothing
  def syntaxError(offset: Offset, msg: String): Nothing

  def syntaxError(msg: String): Nothing = syntaxError(in.offset, msg)
  def warning(msg: String): Unit = warning(in.offset, msg)
  def deprecationWarning(msg: String): Unit = deprecationWarning(in.offset, msg)

  // TODO: get offset out of tree once we have positions
  def syntaxError(tree: Tree, msg: String): Nothing = syntaxError(msg)
  def warning(tree: Tree, msg: String): Unit = deprecationWarning(msg)

  def expectedMsgTemplate(exp: String, fnd: String) = s"$exp expected but $fnd found."
  def expectedMsg(token: Tok): String = expectedMsgTemplate(token.toString, in.tok.toString)

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept(token: Tok): Unit =
    if (in.tok == token) {
      if (token != Tok.EOF) in.next()
    } else syntaxError(expectedMsg(token))

  /** {{{
   *  semi = nl {nl} | `;`
   *  nl  = `\n' // where allowed
   *  }}}
   */
  def acceptStatSep(): Unit = in.tok match {
    case Tok.`\n` | Tok.`\n\n` => in.next()
    case _                  => accept(Tok.`;`)
  }
  def acceptStatSepOpt() =
    if (!isStatSeqEnd)
      acceptStatSep()

/* -------------- TOKEN CLASSES ------------------------------------------- */

  def isModifier: Boolean      = in.tok.isMod
  def isLocalModifier: Boolean = in.tok.isLocalMod
  def isAnnotation: Boolean    = in.tok == Tok.`@`
  def isTemplateIntro: Boolean = in.tok.isTemplateIntro
  def isDclIntro: Boolean      = in.tok.isDclIntro
  def isDefIntro: Boolean      = isTemplateIntro || isDclIntro
  def isNumericLit: Boolean    = in.tok.isNumericLit
  def isLit: Boolean           = in.tok.isLit
  def isExprIntro: Boolean     = in.tok.isExprIntro
  def isTypeIntro: Boolean     = in.tok.isTypeIntro
  def isStatSeqEnd: Boolean    = in.tok.isStatSeqEnd
  def isCaseDefEnd: Boolean    = in.tok.isCaseDefEnd
  def isStatSep: Boolean       = in.tok.isStatSep
  def isName: Boolean          = in.tok.isIdent
  
  def isNameAnd(pred: String => Boolean) = in.tok match { 
    case Tok.Ident(n, _) if pred(n) => true
    case _                          => false
  }
  def isUnaryOp: Boolean           = isNameAnd(SyntacticInfo.isUnaryOp)
  def isNameExcept(except: String) = isNameAnd(_ != except)  
  def isNameOf(name: String)       = isNameAnd(_ == name)
  def isRawStar: Boolean           = isNameOf("*")
  def isRawBar: Boolean            = isNameOf("|")

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

  /** Convert tree to formal parameter list. */
  def convertToParams(tree: Term): List[Param] = tree match {
    case Term.Tuple(ts) => ts.toList flatMap convertToParam
    case _              => List(convertToParam(tree)).flatten
  }

  /** Convert tree to formal parameter. */
  def convertToParam(tree: Term): Option[Param] = tree match {
    case name: Name =>
      Some(Param.Named(Nil, name.toTermName, None, None))
    case Term.Placeholder() =>
      Some(Param.Anonymous(Nil, None))
    case Term.Ascribe(name: Name, tpt) =>
      Some(Param.Named(Nil, name.toTermName, Some(tpt), None))
    case Term.Ascribe(Term.Placeholder(), tpt) =>
      Some(Param.Anonymous(Nil, Some(tpt)))
    case Lit.Unit() =>
      None
    case other =>
      syntaxError(other, s"$other, not a legal formal parameter")
  }

  def convertToTypeId(ref: Term.Ref): Option[Type] = ref match {
    case Term.Select(qual: Qual.Type, name) =>
      Some(Type.Select(qual, name.toTypeName))
    case name: Term.Name =>
      Some(name.toTypeName)
    case _ =>
      None
  }

  /** {{{ part { `sep` part } }}},or if sepFirst is true, {{{ { `sep` part } }}}. */
  final def tokenSeparated[T](separator: Tok, sepFirst: Boolean, part: => T): List[T] = {
    val ts = new ListBuffer[T]
    if (!sepFirst)
      ts += part

    while (in.tok == separator) {
      in.next()
      ts += part
    }
    ts.toList
  }

  @inline final def commaSeparated[T](part: => T): List[T] =
    tokenSeparated(Tok.`,`, sepFirst = false, part)

  @inline final def caseSeparated[T](part: => T): List[T] =
    tokenSeparated(Tok.Case, sepFirst = true, part)

  def readAnnots(part: => Mod.Annot): List[Mod.Annot] =
    tokenSeparated(Tok.`@`, sepFirst = true, part)

  def makeTuple[T <: Tree](body: List[T], zero: () => T, tuple: List[T] => T): T = body match {
    case Nil         => zero()
    case only :: Nil => only
    case _           => tuple(body)
  }

  def makeTupleTerm(body: List[Term]): Term = {
    makeTuple[Term](body, () => Lit.Unit(), Term.Tuple(_))
  }

  def makeTupleTermParens(bodyf: => List[Term]) =
    makeTupleTerm(inParens(if (in.tok == Tok.`)`) Nil else bodyf))

  // TODO: make zero tuple for types Lit.Unit() too?
  def makeTupleType(body: List[Type]): Type =
    makeTuple[Type](body, () => unreachable, Type.Tuple(_))

  def makeTuplePatParens(bodyf: => List[Pat]): Pat = {
    val body = inParens(if (in.tok == Tok.`)`) Nil else bodyf)
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
    implicit object `List Arg Context` extends OpCtx[List[Arg]] {
      def opinfo(tree: List[Arg]): OpInfo[List[Arg]] = {
        val name = termName()
        val targs = if (in.tok == Tok.`[`) exprTypeArgs() else Nil
        OpInfo(tree, name, targs)
      }
      def binop(opinfo: OpInfo[List[Arg]], rhs: List[Arg]): List[Arg] = {
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
        if (in.tok == Tok.`[`) syntaxError("infix patterns cannot have type arguments")
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

  def finishPostfixOp(base: List[OpInfo[List[Arg]]], opinfo: OpInfo[List[Arg]]): List[Arg] =
    Term.Select(reduceStack(base, opinfo.lhs) match {
      case (t: Term) :: Nil => t
      case _                => unreachable
    }, opinfo.operator, isPostfix = true) :: Nil

  def finishBinaryOp[T: OpCtx](opinfo: OpInfo[T], rhs: T): T = opctx.binop(opinfo, rhs)

  def reduceStack[T: OpCtx](base: List[OpInfo[T]], top: T): T = {
    val opPrecedence = if (isName) termName(advance = false).precedence else 0
    val leftAssoc    = !isName || termName(advance = false).isLeftAssoc

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
    def functionArgType(): Param.Type

    private def tupleInfixType(): Type = {
      in.next()
      if (in.tok == Tok.`)`) {
        in.next()
        accept(Tok.`=>`)
        Type.Function(Nil, typ())
      }
      else {
        val ts = functionTypes()
        accept(Tok.`)`)
        if (in.tok == Tok.`=>`) {
          in.next()
          Type.Function(ts, typ())
        } else {
          val tuple = makeTupleType(ts map {
            case t: Type                => t
            case p: Param.Type.ByName   => syntaxError(p, "by name type not allowed here")
            case p: Param.Type.Repeated => syntaxError(p, "repeated type not alloed here")
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
        if (in.tok == Tok.`(`) tupleInfixType()
        else infixType(InfixMode.FirstOp)

      in.tok match {
        case Tok.`=>`    => in.next(); Type.Function(List(t), typ())
        case Tok.ForSome  => in.next(); makeExistentialTypeTree(t)
        case _        => t
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
      simpleTypeRest(in.tok match {
        case Tok.`(` => makeTupleType(inParens(types()))
        case Tok.`_` => in.next(); wildcardType()
        case _      =>
          val ref: Term.Ref = path()
          if (in.tok != Tok.`.`)
            convertToTypeId(ref) getOrElse { syntaxError("identifier expected") }
          else {
            in.next()
            accept(Tok.Type)
            Type.Singleton(ref)
          }
      })
    }

    private def typeProjection(qual: Type): Type.Project = {
      in.next()
      Type.Project(qual, typeName())
    }
    def simpleTypeRest(t: Type): Type = in.tok match {
      case Tok.`#`     => simpleTypeRest(typeProjection(t))
      case Tok.`[` => simpleTypeRest(Type.Apply(t, typeArgs()))
      case _        => t
    }

    /** {{{
     *  CompoundType ::= ModType {with ModType} [Refinement]
     *                |  Refinement
     *  }}}
     */
    def compoundType(): Type = compoundTypeRest(
      if (in.tok == Tok.`{`) None
      else Some(annotType())
    )

    // TODO: warn about def f: Unit { } case?
    def compoundTypeRest(t: Option[Type]): Type = {
      val ts = new ListBuffer[Type] ++ t
      while (in.tok == Tok.With) {
        in.next()
        ts += annotType()
      }
      newLineOptWhenFollowedBy(Tok.`{`)
      val types = ts.toList
      if (in.tok == Tok.`{`) {
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
      if (isNameExcept("*")) {
        val name = termName(advance = false)
        val leftAssoc = name.isLeftAssoc
        if (mode != InfixMode.FirstOp) checkAssoc(name, leftAssoc = mode == InfixMode.LeftOp)
        val op = typeName()
        newLineOptWhenFollowing(_.isTypeIntro)
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
    def functionTypes(): List[Param.Type] = commaSeparated(functionArgType())
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
    def toQualName: Qual.Name = name match {
      case name: Qual.Name => name
      case _               => Qual.Name(name.value, name.isBackquoted)
    }
    def toImportName: Import.Name = name match {
      case name: Import.Name => name
      case _                 => Import.Name(name.value, name.isBackquoted)
    }
  }

  def name[T](ctor: (String, Boolean) => T, advance: Boolean): T = in.tok match {
    case Tok.Ident(name, isBackquoted) => 
      val res = ctor(name, isBackquoted)
      if (advance) in.next()
      res
    case _ =>
      syntaxError(expectedMsg(Tok.Ident("", false)))
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
    def stop = in.tok != Tok.`.` || lookingAhead { in.tok != Tok.This && in.tok != Tok.Super && !isName }
    if (in.tok == Tok.This) {
      in.next()
      val thisnone = Term.This(None)
      if (stop && thisOK) thisnone
      else {
        accept(Tok.`.`)
        selectors(thisnone)
      }
    } else if (in.tok == Tok.Super) {
      in.next()
      val superp = Qual.Super(None, mixinQualifierOpt())
      accept(Tok.`.`)
      val supersel = Term.Select(superp, termName(), isPostfix = false)
      if (stop) supersel
      else {
        in.next()
        selectors(supersel)
      }
    } else {
      val name = termName()
      if (stop) name
      else {
        in.next()
        if (in.tok == Tok.This) {
          in.next()
          val thisid = Term.This(Some(name.toQualName))
          if (stop && thisOK) thisid
          else {
            accept(Tok.`.`)
            selectors(thisid)
          }
        } else if (in.tok == Tok.Super) {
          in.next()
          val superp = Qual.Super(Some(name.toQualName), mixinQualifierOpt())
          accept(Tok.`.`)
          val supersel = Term.Select(superp, termName(), isPostfix = false)
          if (stop) supersel
          else {
            in.next()
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
    if (in.tok == Tok.`.` && lookingAhead { isName }) {
      in.next()
      selectors(t1)
    }
    else t1
  }

  /** {{{
  *   MixinQualifier ::= `[' Id `]'
  *   }}}
  */
  def mixinQualifierOpt(): Option[Type.Name] =
    if (in.tok == Tok.`[`) Some(inBrackets(typeName()))
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
    if (in.tok != Tok.`.`) name
    else {
      in.next()
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
    val res = in.tok match {
      case Tok.Char(v)       => Lit.Char(v)
      case Tok.Int(v)        => Lit.Int(if (isNegated) -v else v)
      case Tok.Long(v)       => Lit.Long(if (isNegated) -v else v)
      case Tok.Float(v)      => Lit.Float(if (isNegated) -v else v)
      case Tok.Double(v)     => Lit.Double(if (isNegated) -v else v)
      case Tok.String(v)     => Lit.String(v) 
      case Tok.StringPart(v) => Lit.String(v) 
      case Tok.True          => Lit.Bool(true)
      case Tok.False         => Lit.Bool(false)
      case Tok.Null          => Lit.Null()
      case Tok.Symbol(v)     => Lit.Symbol(v)
      case _                 => syntaxError("illegal literal")
    }
    in.next()
    res
  }

  def interpolate[Ctx, Ret](arg: () => Ctx, result: (Term.Name, List[Lit.String], List[Ctx]) => Ret): Ret = {
    def part() = {
      val value = in.tok match {
        case Tok.String(v)     => v
        case Tok.StringPart(v) => v
        case _                 => syntaxError(expectedMsg(Tok.StringPart("")))
      }
      val lit = Lit.String(value)
      in.next()
      lit
    }
    val Tok.InterpolationId(nameStr) = in.tok
    val interpolator = Term.Name(nameStr, isBackquoted = false) // termName() for INTERPOLATIONID
    in.next()
    val partsBuf = new ListBuffer[Lit.String]
    val argsBuf = new ListBuffer[Ctx]
    while (in.tok.isInstanceOf[Tok.StringPart]) {
      partsBuf += part()
      argsBuf += arg()
    }
    if (in.tok.isInstanceOf[Tok.String]) partsBuf += part()
    result(interpolator, partsBuf.toList, argsBuf.toList)
  }

  def interpolateTerm(): Term.Interpolate = {
    def dropTrivialBlock(term: Term): Term = term match {
      case Term.Block((stat: Term) :: Nil) => stat
      case _ => term
    }
    interpolate[Term, Term.Interpolate](arg = { () =>
      in.tok match {
        case Tok.Ident(_, _) => termName()
        //case Tok.`_`       => freshPlaceholder()       // ifonly etapolation
        case Tok.`{`         => dropTrivialBlock(expr()) // dropAnyBraces(expr0(Local))
        case Tok.This        => in.next(); Term.This(None)
        case _               => syntaxError("error in interpolated string: identifier or block expected")
      }
    }, result = Term.Interpolate(_, _, _))
  }

  def interpolatePat(): Pat.Interpolate =
    interpolate[Pat, Pat.Interpolate](arg = () => dropAnyBraces(pattern()), result = Pat.Interpolate(_, _, _))

/* ------------- Tok.New LINES ------------------------------------------------- */

  def newLineOpt(): Unit = {
    if (in.tok == Tok.`\n`) in.next()
  }

  def newLinesOpt(): Unit = {
    if (in.tok == Tok.`\n` || in.tok == Tok.`\n\n`)
      in.next()
  }

  def newLineOptWhenFollowedBy(tok: Tok): Unit = {
    // note: next is defined here because current == Tok.`\n`
    if (in.tok == Tok.`\n` && lookingAhead { in.tok == tok }) newLineOpt()
  }

  def newLineOptWhenFollowing(p: Tok => Boolean): Unit = {
    // note: next is defined here because current == Tok.`\n`
    if (in.tok == Tok.`\n` && lookingAhead { p(in.tok) }) newLineOpt()
  }

/* ------------- TYPES ---------------------------------------------------- */

  /** {{{
   *  TypedOpt ::= [`:' Type]
   *  }}}
   */
  def typedOpt(): Option[Type] =
    if (in.tok == Tok.`:`) { in.next(); Some(typ()) }
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
    if (in.tok == Tok.`(`) {
      in.next()
      val r = expr()
      accept(Tok.`)`)
      r
    } else {
      accept(Tok.`(`)
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

  def expr(location: Location): Term = in.tok match {
    case Tok.If =>
      in.next()
      val cond = condExpr()
      newLinesOpt()
      val thenp = expr()
      if (in.tok == Tok.Else) { in.next(); Term.If(cond, thenp, expr()) }
      else { Term.If(cond, thenp) }
    case Tok.Try =>
      in.next()
      val body: Term = in.tok match {
        case Tok.`{` => inBracesOrUnit(block())
        case Tok.`(` => inParensOrUnit(expr())
        case _      => expr()
      }
      val catchopt =
        if (in.tok != Tok.Catch) None
        else {
          in.next()
          if (in.tok != Tok.`{`) Some(expr())
          else inBraces {
            if (in.tok == Tok.Case) Some(Term.Cases(caseClauses()))
            else Some(expr())
          }
        }
      val finallyopt = in.tok match {
        case Tok.Finally => in.next(); Some(expr())
        case _       => None
      }
      Term.Try(body, catchopt, finallyopt)
    case Tok.While =>
      in.next()
      val cond = condExpr()
      newLinesOpt()
      val body = expr()
      Term.While(cond, body)
    case Tok.Do =>
      in.next()
      val body = expr()
      if (isStatSep) in.next()
      accept(Tok.While)
      val cond = condExpr()
      Term.Do(body, cond)
    case Tok.For =>
      in.next()
      val enums =
        if (in.tok == Tok.`{`) inBracesOrNil(enumerators())
        else inParensOrNil(enumerators())
      newLinesOpt()
      if (in.tok == Tok.Yield) {
        in.next()
        Term.ForYield(enums, expr())
      } else {
        Term.For(enums, expr())
      }
    case Tok.Return =>
      in.next()
      if (isExprIntro) Term.Return(expr())
      else Term.Return()
    case Tok.Throw =>
      in.next()
      Term.Throw(expr())
    case Tok.Implicit =>
      implicitClosure(location)
    case _ =>
      var t: Term = postfixExpr()
      if (in.tok == Tok.`=`) {
        t match {
          case ref: Term.Ref =>
            in.next()
            t = Term.Assign(ref, expr())
          case app: Term.Apply =>
            in.next()
            t = Term.Update(app, expr())
          case _ =>
        }
      } else if (in.tok == Tok.`:`) {
        val colonPos = in.next()
        if (isAnnotation) {
          t = Term.Annotate(t, annots(skipNewLines = false))
        } else {
          t = {
            val tpt = typeOrInfixType(location)
            // this does not correspond to syntax, but is necessary to
            // accept closures. We might restrict closures to be between {...} only.
            Term.Ascribe(t, tpt)
          }
        }
      } else if (in.tok == Tok.Match) {
        in.next()
        t = Term.Match(t, Term.Cases(inBracesOrNil(caseClauses())))
      }
      // in order to allow anonymous functions as statements (as opposed to expressions) inside
      // templates, we have to disambiguate them from self type declarations - bug #1565
      // The case still missed is unparenthesized single argument, like "x: Int => x + 1", which
      // may be impossible to distinguish from a self-type and so remains an error.  (See #1564)
      def lhsIsTypedParamList() = t match {
        case Term.Tuple(xs) if xs.forall(_.isInstanceOf[Term.Ascribe]) => true
        case _ => false
      }
      if (in.tok == Tok.`=>` && (location != InTemplate || lhsIsTypedParamList)) {
        in.next()
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
      if (in.tok != Tok.`:`) name
      else {
        in.next()
        Term.Ascribe(expr, typeOrInfixType(location))
      }
    }.get
    val mods = Mod.Implicit() +: param0.mods
    val param = param0 match { case p: Param.Anonymous => p.copy(mods = mods); case p: Param.Named => p.copy(mods = mods) }
    accept(Tok.`=>`)
    Term.Function(List(param), if (location != InBlock) expr() else block())
  }

  /** {{{
   *  PostfixExpr   ::= InfixExpr [Id [nl]]
   *  InfixExpr     ::= PrefixExpr
   *                  | InfixExpr Id [nl] InfixExpr
   *  }}}
   */
  def postfixExpr(): Term = {
    val ctx  = opctx[List[Arg]]
    val base = ctx.stack

    def loop(top: List[Arg]): List[Arg] =
      if (!isName) top
      else {
        ctx.push(reduceStack(base, top))
        newLineOptWhenFollowing(_.isExprIntro)
        if (isExprIntro) loop(argumentExprsOrPrefixExpr())
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
      if (op.value == "-" && isNumericLit)
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
      in.tok match {
        case _ if isLit =>
          literal()
        case Tok.InterpolationId(_) =>
          interpolateTerm()
        case Tok.XMLStart =>
          xmlLiteral()
        case Tok.Ident(_, _) | Tok.This | Tok.Super =>
          path()
        case Tok.`_` =>
          in.next()
          Term.Placeholder()
        case Tok.`(` =>
          makeTupleTermParens(commaSeparated(expr()))
        case Tok.`{` =>
          canApply = false
          blockExpr()
        case Tok.New =>
          canApply = false
          in.next()
          val (edefs, parents, self, stats, hasStats) = template()
          if (hasStats) Term.New(Aux.Template(edefs, parents, self, stats))
          else Term.New(Aux.Template(edefs, parents, self))
        case _ =>
          syntaxError(s"illegal start of simple expression: ${in.tok}")
      }
    simpleExprRest(t, canApply = canApply)
  }

  def simpleExprRest(t: Term, canApply: Boolean): Term = {
    if (canApply) newLineOptWhenFollowedBy(Tok.`{`)
    in.tok match {
      case Tok.`.` =>
        in.next()
        simpleExprRest(selector(t), canApply = true)
      case Tok.`[` =>
        t match {
          case _: Term.Name | _: Term.Select | _: Term.Apply =>
            var app: Term = t
            while (in.tok == Tok.`[`)
              app = Term.ApplyType(app, exprTypeArgs())

            simpleExprRest(app, canApply = true)
          case _ =>
            t
        }
      case Tok.`(` | Tok.`{` if (canApply) =>
        simpleExprRest(Term.Apply(t, argumentExprs()), canApply = true)
      case Tok.`_` =>
        in.next()
        Term.Eta(t)
      case _ =>
        t
    }
  }

  /** Translates an Assign(_, _) node to AssignOrNamedArg(_, _) if
   *  the lhs is a simple ident. Otherwise returns unchanged.
   */
  def assignmentToMaybeNamedArg(tree: Term): Arg = tree match {
    case Term.Assign(name: Term.Name, rhs) => Arg.Named(name, rhs)
    case t                                 => t
  }

  def argsToTerm(args: List[Arg]): Term = {
    def loop(args: List[Arg]): List[Term] = args match {
      case Nil                         => Nil
      case (t: Term) :: rest           => t :: loop(rest)
      case (nmd: Arg.Named) :: rest    => Term.Assign(nmd.name, nmd.rhs) :: loop(rest)
      case (rep: Arg.Repeated) :: rest => syntaxError(rep, "repeated argument not allowed here")
    }
    makeTupleTerm(loop(args))
  }

  def argumentExprsOrPrefixExpr(): List[Arg] =
    if (in.tok != Tok.`{` && in.tok != Tok.`(`) prefixExpr() :: Nil
    else {
      val args = argumentExprs()
      in.tok match {
        case Tok.`.` | Tok.`[` | Tok.`(` | Tok.`{` | Tok.`_` =>
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
  def argumentExprs(): List[Arg] = {
    def args(): List[Arg] = commaSeparated {
      expr() match {
        case Term.Ascribe(t, Type.Placeholder(bounds: Aux.TypeBounds)) if !bounds.hasLo && !bounds.hasHi && isName && isNameOf("*") =>
          in.next()
          Arg.Repeated(t)
        case Term.Assign(t: Term.Name, rhs) =>
          Arg.Named(t, rhs)
        case other =>
          other
      }
    }
    in.tok match {
      case Tok.`{`   => List(blockExpr())
      case Tok.`(`   => inParens(if (in.tok == Tok.`)`) Nil else args())
      case _        => Nil
    }
  }

  /** A succession of argument lists. */
  def multipleArgumentExprs(): List[List[Arg]] = {
    if (in.tok != Tok.`(`) Nil
    else argumentExprs() :: multipleArgumentExprs()
  }

  /** {{{
   *  BlockExpr ::= `{' (CaseClauses | Block) `}'
   *  }}}
   */
  def blockExpr(): Term = {
    inBraces {
      if (in.tok == Tok.Case) Term.Cases(caseClauses())
      else block()
    }
  }

  def mkBlock(stats: List[Stmt.Block]): Term = Term.Block(stats)

  /** {{{
   *  Block ::= BlockStatSeq
   *  }}}
   *  @note  Return tree does not carry position.
   */
  def block(): Term = mkBlock(blockStatSeq())

  def caseClause(): Aux.Case =
    Aux.Case(pattern(), guard(), { accept(Tok.`=>`); blockStatSeq() })

  /** {{{
   *  CaseClauses ::= CaseClause {CaseClause}
   *  CaseClause  ::= case Pattern [Guard] `=>' Block
   *  }}}
   */
  def caseClauses(): List[Aux.Case] = {
    val cases = caseSeparated { caseClause() }
    if (cases.isEmpty)  // trigger error if there are no cases
      accept(Tok.Case)
    cases
  }

  /** {{{
   *  Guard ::= if PostfixExpr
   *  }}}
   */
  def guard(): Option[Term] =
    if (in.tok == Tok.If) { in.next(); Some(postfixExpr()) }
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
    while (isStatSep) {
      in.next()
      enums ++= enumerator(isFirst = false)
    }
    enums.toList
  }

  def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true): List[Enum] =
    if (in.tok == Tok.If && !isFirst) Enum.Guard(guard().get) :: Nil
    else generator(!isFirst, allowNestedIf)

  /** {{{
   *  Generator ::= Pattern1 (`<-' | `=') Expr [Guard]
   *  }}}
   */
  def generator(eqOK: Boolean, allowNestedIf: Boolean = true): List[Enum] = {
    val hasVal = in.tok == Tok.Val
    if (hasVal)
      in.next()

    val pat   = noSeq.pattern1()
    val point = in.offset
    val hasEq = in.tok == Tok.`=`

    if (hasVal) {
      if (hasEq) deprecationWarning("val keyword in for comprehension is deprecated")
      else syntaxError("val in for comprehension must be followed by assignment")
    }

    if (hasEq && eqOK) in.next()
    else accept(Tok.`<-`)
    val rhs = expr()

    def loop(): List[Enum] =
      if (in.tok != Tok.If) Nil
      else Enum.Guard(guard().get) :: loop()

    val tail =
      if (allowNestedIf) loop()
      else Nil

    (if (hasEq) Enum.Val(pat, rhs)
     else Enum.Generator(pat, rhs)) :: tail
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

    def functionArgType(): Param.Type = paramType()
    def argType(): Type = typ()

    /** {{{
     *  Patterns ::= Pattern { `,' Pattern }
     *  SeqPatterns ::= SeqPattern { `,' SeqPattern }
     *  }}}
     */
    def patterns(): List[Pat] = commaSeparated(pattern())

    /** {{{
     *  Pattern  ::=  Pattern1 { `|' Pattern1 }
     *  SeqPattern ::= SeqPattern1 { `|' SeqPattern1 }
     *  }}}
     */
    def pattern(): Pat = {
      def loop(pat: Pat): Pat =
        if (!isRawBar) pat
        else { in.next(); Pat.Alternative(pat, pattern()) }
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
    def pattern1(): Pat = pattern2() match {
      case p @ (_: Term.Name | _: Pat.Wildcard) if in.tok == Tok.`:` =>
        p match {
          case name: Term.Name if !name.isVarPattern =>
            syntaxError("Pattern variables must start with a lower-case letter. (SLS 8.1.1.)")
          case _ =>
        }
        in.next()
        Pat.Typed(p, compoundType())
      case p => p
    }

    /** {{{
     *  Pattern2    ::=  varid [ @ Pattern3 ]
     *                |   Pattern3
     *  SeqPattern2 ::=  varid [ @ SeqPattern3 ]
     *                |   SeqPattern3
     *  }}}
     */
    def pattern2(): Pat = {
      val p = pattern3()

      if (in.tok != Tok.`@`) p
      else p match {
        case Pat.Wildcard() =>
          in.next()
          pattern3()
        case name: Term.Name if name.isVarPattern =>
          in.next()
          Pat.Bind(name, pattern3())
        case _ => p
      }
    }

    /** {{{
     *  Pattern3    ::= SimplePattern
     *                |  SimplePattern {Id [nl] SimplePattern}
     *  }}}
     */
    def pattern3(): Pat = {
      val top: Pat = simplePattern(badPattern3)
      val ctx = opctx[Pat]
      val base = ctx.stack
      // See SI-3189, SI-4832 for motivation. Cf SI-3480 for counter-motivation.
      def isCloseDelim = in.tok match {
        case Tok.`}` => isXML
        case Tok.`)` => !isXML
        case _      => false
      }
      def checkWildStar: Option[Pat.SeqWildcard] = top match {
        case Pat.Wildcard() if isSequenceOK && isRawStar => peekingAhead (
          // TODO: used to be Star(top) | EmptyTree, why start had param?
          if (isCloseDelim) Some(Pat.SeqWildcard())
          else None
        )
        case _ => None
      }
      def loop(top: Pat): Pat = reduceStack(base, top) match {
        case next if isNameExcept("|") => ctx.push(next); loop(simplePattern(badPattern3))
        case next                      => next
      }
      checkWildStar getOrElse loop(top)
    }

    def badPattern3(): Nothing = {
      def isComma                = in.tok == Tok.`,`
      def isDelimiter            = in.tok == Tok.`)` || in.tok == Tok.`}`
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
    def simplePattern(onError: () => Nothing): Pat = in.tok match {
      case Tok.Ident(_, _) | Tok.This =>
        val sid = stableId()
        if (in.tok.isNumericLit) {
          sid match {
            case Term.Name("-") =>
              return literal(isNegated = true)
            case _ =>
          }
        }
        val targs = in.tok match {
          case Tok.`[` => typeArgs()
          case _        => Nil
        }
        (in.tok, sid) match {
          case (Tok.`(`, _)                 => Pat.Extract(sid, targs, argumentPatterns())
          case (_, _) if targs.nonEmpty    => syntaxError("pattern must be a value")
          case (_, sid: Term.Ref with Pat) => sid
          case _                           => unreachable
        }
      case Tok.`_` =>
        in.next()
        Pat.Wildcard()
      case tok if tok.isLit =>
        literal()
      case Tok.InterpolationId(_) =>
        interpolatePat()
      case Tok.`(` =>
        makeTuplePatParens(noSeq.patterns())
      case Tok.XMLStart =>
        xmlLiteralPattern()
      case _ =>
        onError()
    }
  }
  /** The implementation of the context sensitive methods for parsing outside of patterns. */
  object outPattern extends PatternContextSensitive {
    def argType(): Type = typ()
    def functionArgType(): Param.Type = paramType()
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
  def pattern(): Pat = noSeq.pattern()
  def seqPatterns(): List[Pat] = seqOK.patterns()
  def xmlSeqPatterns(): List[Pat] = xmlSeqOK.patterns() // Called from xml parser
  def argumentPatterns(): List[Pat] = inParens {
    if (in.tok == Tok.`)`) Nil
    else seqPatterns()
  }
  def xmlLiteralPattern(): Pat

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  /** {{{
   *  AccessQualifier ::= `[' (Id | this) `]'
   *  }}}
   */
  def accessQualifierOpt(): Option[Qual.Access] = {
    if (in.tok != Tok.`[`) None
    else {
      in.next()
      val res = if (in.tok != Tok.This) termName().toQualName
                else { in.next(); Term.This(None) }
      accept(Tok.`]`)
      Some(res)
    }
  }

  /** {{{
   *  AccessModifier ::= (private | protected) [AccessQualifier]
   *  }}}
   */
  def accessModifierOpt(): Option[Mod] = in.tok match {
    case Tok.Private   => in.next(); Some(Mod.Private(accessQualifierOpt()) )
    case Tok.Protected => in.next(); Some(Mod.Protected(accessQualifierOpt()) )
    case _         => None
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
      mods.foreach { m => if (m == mod) syntaxError(m, "repeated modifier") }
      if (advance) in.next()
      mods :+ mod
    }
    def acceptable = if (isLocal) isLocalModifier else true
    def loop(mods: List[Mod]): List[Mod] =
      if (!acceptable) mods
      else (in.tok match {
        case Tok.Abstract            => loop(addMod(mods, Mod.Abstract()))
        case Tok.Final               => loop(addMod(mods, Mod.Final()))
        case Tok.Sealed              => loop(addMod(mods, Mod.Sealed()))
        case Tok.Implicit            => loop(addMod(mods, Mod.Implicit()))
        case Tok.Lazy                => loop(addMod(mods, Mod.Lazy()))
        case Tok.Override            => loop(addMod(mods, Mod.Override()))
        case Tok.Private | Tok.Protected =>
          mods.foreach { m =>
            if(m.isInstanceOf[Mod.Access]) syntaxError("duplicate private/protected qualifier")
          }
          val optmod = accessModifierOpt()
          optmod.map { mod => loop(addMod(mods, mod, advance = false)) }.getOrElse(mods)
        case Tok.`\n` if !isLocal => in.next(); loop(mods)
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
    Mod.Annot(exprSimpleType(), argumentExprs() :: Nil)
  }

  def annot(): Mod.Annot = {
    val t = exprSimpleType()
    if (in.tok == Tok.`(`) Mod.Annot(t, multipleArgumentExprs())
    else Mod.Annot(t, Nil)
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
  def paramClauses(ownerIsType: Boolean, ownerIsCase: Boolean = false): (List[List[Param.Named]], List[Param.Named]) = {
    var parsedImplicits = false
    def paramClause(): List[Param.Named] = {
      if (in.tok == Tok.`)`)
        return Nil

      if (in.tok == Tok.Implicit) {
        in.next()
        parsedImplicits = true
      }
      commaSeparated(param(ownerIsCase, ownerIsType, isImplicit = parsedImplicits))
    }
    val paramss = new ListBuffer[List[Param.Named]]
    var implicits = List.empty[Param.Named]
    newLineOptWhenFollowedBy(Tok.`(`)
    while (!parsedImplicits && in.tok == Tok.`(`) {
      in.next()
      val clause = paramClause()
      if (!parsedImplicits)
        paramss += clause
      else
        implicits = clause
      accept(Tok.`)`)
      newLineOptWhenFollowedBy(Tok.`(`)
    }
    (paramss.toList, implicits)
  }

  /** {{{
   *  ParamType ::= Type | `=>' Type | Type `*'
   *  }}}
   */
  def paramType(): Param.Type = in.tok match {
    case Tok.`=>` =>
      in.next()
      Param.Type.ByName(typ())
    case _ =>
      val t = typ()
      if (!isRawStar) t
      else {
        in.next()
        Param.Type.Repeated(t)
      }
  }

  def param(ownerIsCase: Boolean, ownerIsType: Boolean, isImplicit: Boolean): Param.Named = {
    var mods: List[Mod] = annots(skipNewLines = false)
    if (ownerIsType) {
      mods ++= modifiers()
      mods.getAll[Mod.Lazy].foreach { m =>
        syntaxError(m, "lazy modifier not allowed here. Use call-by-name parameters instead")
      }
      in.tok match {
        case Tok.Val => mods = mods :+ Mod.ValParam(); in.next()
        case Tok.Var => mods = mods :+ Mod.VarParam(); in.next()
        case _   =>
      }
    }
    val name = termName()
    val tpt = {
      accept(Tok.`:`)
      val tpt = paramType()
      if (tpt.isInstanceOf[Param.Type.ByName]) {
        def mayNotBeByName(subj: String) =
          syntaxError(s"$subj parameters may not be call-by-name")
        val isLocalToThis: Boolean =
          if (ownerIsCase) (mods.access match {
            case Some(Mod.Private(Some(Term.This(_)))) => true
            case _ => false
          }) else (mods.access match {
            case Some(Mod.Private(Some(Term.This(_)))) => true
            case None if !mods.has[Mod.ValParam] && !mods.has[Mod.VarParam] => true
            case _ => false
          })
        if (ownerIsType && !isLocalToThis) {
          if(mods.has[Mod.VarParam])
            mayNotBeByName("`var'")
          else
            mayNotBeByName("`val'")
        } else if (isImplicit)
          mayNotBeByName("implicit")
      }
      tpt
    }
    val default =
      if (in.tok != Tok.`=`) None
      else {
        in.next()
        Some(expr())
      }
    Param.Named(mods, name, Some(tpt), default)
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
  def typeParamClauseOpt(ownerIsType: Boolean, ctxBoundsAllowed: Boolean): List[TypeParam] = {
    newLineOptWhenFollowedBy(Tok.`[`)
    if (in.tok != Tok.`[`) Nil
    else inBrackets(commaSeparated {
      val mods = annots(skipNewLines = true)
      typeParam(mods, ownerIsType, ctxBoundsAllowed)
    })
  }

  def typeParam(annots: List[Mod.Annot], ownerIsType: Boolean, ctxBoundsAllowed: Boolean): TypeParam = {
    val mods =
      if (ownerIsType && isName) {
        if (isNameOf("+")) {
          in.next()
          annots :+ Mod.Covariant()
        } else if (isNameOf("-")) {
          in.next()
          annots :+ Mod.Contravariant()
        } else annots
      } else annots
    val nameopt =
      if (isName) Some(typeName())
      else if (in.tok == Tok.`_`) { in.next(); None }
      else syntaxError("identifier or `_' expected")
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    val bounds = typeBounds()
    val contextBounds = new ListBuffer[Type]
    val viewBounds = new ListBuffer[Type]
    if (ctxBoundsAllowed) {
      while (in.tok == Tok.`<%`) {
        // TODO: syntax profile?
        // if (settings.future) {
        //   val msg = ("Use an implicit parameter instead.\n" +
        //              "Example: Instead of `def f[A <% Int](a: A)` " +
        //              "use `def f[A](a: A)(implicit ev: A => Int)`.")
        //   deprecationWarning(s"View bounds are deprecated. $msg")
        // }
        in.next()
        viewBounds += typ()
      }
      while (in.tok == Tok.`:`) {
        in.next()
        contextBounds += typ()
      }
    }
    nameopt match {
      case Some(name) => TypeParam.Named(mods, name, tparams, contextBounds.toList, viewBounds.toList, bounds)
      case None => TypeParam.Anonymous(mods, tparams, contextBounds.toList, viewBounds.toList, bounds)
    }
  }

  /** {{{
   *  TypeBounds ::= [`>:' Type] [`<:' Type]
   *  }}}
   */
  def typeBounds(): Aux.TypeBounds = {
    (bound(Tok.`>:`), bound(Tok.`<:`)) match {
      case (Some(lo), Some(hi)) => Aux.TypeBounds(lo, hi)
      case (Some(lo), None)     => Aux.TypeBounds(lo = lo)
      case (None, Some(hi))     => Aux.TypeBounds(hi = hi)
      case (None, None)         => Aux.TypeBounds()
    }
  }

  def bound(tok: Tok): Option[Type] =
    if (in.tok == tok) { in.next(); Some(typ()) } else None

/* -------- DEFS ------------------------------------------- */


  /** {{{
   *  Import  ::= import ImportExpr {`,' ImportExpr}
   *  }}}
   */
  def importStmt(): Import = {
    accept(Tok.Import)
    Import(commaSeparated(importClause()))
  }

  /** {{{
   *  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
   *  }}}
   */
  def importClause(): Import.Clause = {
    val sid = stableId()
    def dotselectors = { accept(Tok.`.`); Import.Clause(sid, importSelectors()) }
    sid match {
      case Term.Select(sid: Term.Ref, name: Term.Name) if sid.isStableId =>
        if (in.tok == Tok.`.`) dotselectors
        else Import.Clause(sid, name.toImportName :: Nil)
      case _ => dotselectors
    }
  }

  /** {{{
   *  ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
   *  }}}
   */
  def importSelectors(): List[Import.Selector] =
    if (in.tok != Tok.`{`) List(importWildcardOrName())
    else inBraces(commaSeparated(importSelector()))

  def importWildcardOrName(): Import.Selector =
    if (in.tok == Tok.`_`) { in.next(); Import.Wildcard() }
    else { val name = termName(); name.toImportName }

  /** {{{
   *  ImportSelector ::= Id [`=>' Id | `=>' `_']
   *  }}}
   */
  def importSelector(): Import.Selector = {
    importWildcardOrName() match {
      case from: Import.Name if in.tok == Tok.`=>` =>
        in.next()
        importWildcardOrName() match {
          case to: Import.Name     => Import.Rename(from, to)
          case to: Import.Wildcard => Import.Unimport(from)
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
  def defOrDclOrCtor(mods: List[Mod]): Stmt.Template = {
    mods.getAll[Mod.Lazy].foreach { m =>
      if (in.tok != Tok.Val) syntaxError(m, "lazy not allowed here. Only vals can be lazy")
    }
    in.tok match {
      case Tok.Val | Tok.Var =>
        patDefOrDcl(mods)
      case Tok.Def =>
        funDefOrDclOrCtor(mods)
      case Tok.Type =>
        typeDefOrDcl(mods)
      case _ =>
        tmplDef(mods)
    }
  }

  def nonLocalDefOrDcl: Stmt.Template = {
    val anns = annots(skipNewLines = true)
    val mods = anns ++ modifiers()
    defOrDclOrCtor(mods) match {
      case s: Stmt.Template => s
      case other            => syntaxError(other, "is not a valid template statement")
    }
  }

  /** {{{
   *  PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
   *  ValDcl ::= Id {`,' Id} `:' Type
   *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
   *  }}}
   */
  def patDefOrDcl(mods: List[Mod]): Stmt.Template = {
    val isMutable = in.tok == Tok.Var
    in.next()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2())
    val tp: Option[Type] = typedOpt()

    if (tp.isEmpty || in.tok == Tok.`=`) {
      accept(Tok.`=`)
      val rhs =
        if (in.tok == Tok.`_` && tp.nonEmpty && isMutable && lhs.forall(_.isInstanceOf[Term.Name])) {
          in.next()
          None
        } else Some(expr())

      if (isMutable) Defn.Var(mods, lhs, tp, rhs)
      else Defn.Val(mods, lhs, tp, rhs.get)
    } else {
      mods.getAll[Mod.Lazy].foreach { syntaxError(_, "lazy values may not be abstract") }
      val ids = lhs.map {
        case name: Term.Name => name
        case other          => syntaxError(other, "pattern definition may not be abstract")
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
  def funDefOrDclOrCtor(mods: List[Mod]): Stmt.Template = {
    in.next()
    if (in.tok != Tok.This) funDefRest(mods, termName())
    else {
      in.next()
      val (paramss, implicits) = paramClauses(ownerIsType = true)
      newLineOptWhenFollowedBy(Tok.`{`)
      val (argss, stats) = in.tok match {
        case Tok.`{` => constrBlock()
        case _      => accept(Tok.`=`); constrExpr()
      }
      Ctor.Secondary(mods, paramss, implicits, argss, stats)
    }
  }

  def funDefRest(mods: List[Mod], name: Term.Name): Stmt.Template = {
    def warnProcedureDeprecation =
      deprecationWarning(in.offset, s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit`.")
    val tparams = typeParamClauseOpt(ownerIsType = false, ctxBoundsAllowed = true)
    val (paramss, implicits) = paramClauses(ownerIsType = false)
    newLineOptWhenFollowedBy(Tok.`{`)
    var restype = fromWithinReturnType(typedOpt())
    if (isStatSep || in.tok == Tok.`}`) {
      if (restype.isEmpty) {
        warnProcedureDeprecation
        Decl.Procedure(mods, name, tparams, paramss, implicits)
      } else
        Decl.Def(mods, name, tparams, paramss, implicits, restype.get)
    } else if (restype.isEmpty && in.tok == Tok.`{`) {
      warnProcedureDeprecation
      Defn.Procedure(mods, name, tparams, paramss, implicits, {
        accept(Tok.`{`)
        var r = blockStatSeq()
        accept(Tok.`}`)
        r
      })
    } else {
      var isMacro = false
      val rhs = {
        if (in.tok == Tok.`=`) {
          in.next()
          isMacro = in.tok == Tok.Macro
          if (isMacro) in.next()
        } else {
          accept(Tok.`=`)
        }
        expr()
      }
      if (isMacro) restype match {
        case Some(restype) => Defn.Macro(mods, name, tparams, paramss, implicits, restype, rhs)
        case None => syntaxError(in.offset, "macros must have explicitly specified return types")
      } else Defn.Def(mods, name, tparams, paramss, implicits, restype, rhs)
    }
  }

  /** {{{
   *  ConstrExpr      ::=  SelfInvocation
   *                    |  ConstrBlock
   *  }}}
   */
  def constrExpr(): (List[List[Arg]], List[Stmt.Block]) =
    if (in.tok == Tok.`{`) constrBlock()
    else (selfInvocation(), Nil)

  /** {{{
   *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
   *  }}}
   */
  def selfInvocation(): List[List[Arg]] = {
    accept(Tok.This)
    newLineOptWhenFollowedBy(Tok.`{`)
    var t: List[List[Arg]] = List(argumentExprs())
    newLineOptWhenFollowedBy(Tok.`{`)
    while (in.tok == Tok.`(` || in.tok == Tok.`{`) {
      t = t :+ argumentExprs()
      newLineOptWhenFollowedBy(Tok.`{`)
    }
    t
  }

  /** {{{
   *  ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
   *  }}}
   */
  def constrBlock(): (List[List[Arg]], List[Stmt.Block]) = {
    in.next()
    val argss = selfInvocation()
    val stats =
      if (!isStatSep) Nil
      else { in.next(); blockStatSeq() }
    accept(Tok.`}`)
    (argss, stats)
  }

  /** {{{
   *  TypeDef ::= type Id [TypeParamClause] `=' Type
   *            | FunSig `=' Expr
   *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
   *  }}}
   */
  def typeDefOrDcl(mods: List[Mod]): Member.Type with Stmt.Template = {
    in.next()
    newLinesOpt()
    val name = typeName()
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    in.tok match {
      case Tok.`=` =>
        in.next()
        Defn.Type(mods, name, tparams, typ())
      case t if t == Tok.`>:` || t == Tok.`<:` || t == Tok.`,` || t == Tok.`}` || t.isStatSep =>
        Decl.Type(mods, name, tparams, typeBounds())
      case _ =>
        syntaxError("`=', `>:', or `<:' expected")
    }
  }

  /** Hook for IDE, for top-level classes/objects. */
  def topLevelTmplDef: Has.Template with Stmt.TopLevel =
    tmplDef(annots(skipNewLines = true) ++ modifiers())

  /** {{{
   *  TmplDef ::= [case] class ClassDef
   *            |  [case] object ObjectDef
   *            |  [override] trait TraitDef
   *  }}}
   */
  def tmplDef(mods: List[Mod]): Has.Template with Stmt.Template = {
    mods.getAll[Mod.Lazy].foreach { syntaxError(_, "classes cannot be lazy") }
    in.tok match {
      case Tok.Trait =>
        traitDef(mods)
      case Tok.Class =>
        classDef(mods)
      case Tok.CaseClass =>
        classDef(mods :+ Mod.Case())
      case Tok.Object =>
        objectDef(mods)
      case Tok.CaseObject =>
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
    in.next()
    // TODO:
    // if (ofCaseClass && in.tok != Tok.`(`)
    //  syntaxError(in.offset, "case classes without a parameter list are not allowed;\n"+
    //                             "use either case objects or case classes with an explicit `()' as a parameter list.")
    // TODO:
    // if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods.isImplicit)))) {
    //  in.tok match {
    //    case Tok.`[`   => syntaxError("no type parameters allowed here")
    //    case Tok.EOF        => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
    //    case _          => syntaxError(start, "auxiliary constructor needs non-implicit parameter list")
    //  }
    // }

    Defn.Class(mods, typeName(),
               typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = true),
               primaryCtor(ownerIsCase = mods.has[Mod.Case]), templateOpt(OwnedByClass))
  }

  def primaryCtor(ownerIsCase: Boolean): Ctor.Primary = {
    val mods = constructorAnnots() ++ accessModifierOpt()
    val (paramss, implicits) = paramClauses(ownerIsType = true, ownerIsCase)
    Ctor.Primary(mods, paramss, implicits)
  }

  /** {{{
   *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
   *  }}}
   */
  def traitDef(mods: List[Mod]): Defn.Trait = {
    in.next()
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
    in.next()
    Defn.Object(mods, termName(), templateOpt(OwnedByObject))
  }

  /** {{{
   *  ClassParents       ::= ModType {`(' [Exprs] `)'} {with ModType}
   *  TraitParents       ::= ModType {with ModType}
   *  }}}
   */
  def templateParents(): List[Aux.Parent] = {
    val parents = new ListBuffer[Aux.Parent]
    def readAppliedParent() = {
      val parentTpe = startModType()
      parents += (in.tok match {
        case Tok.`(` => Aux.Parent(parentTpe, multipleArgumentExprs())
        case _      => Aux.Parent(parentTpe, Nil)
      })
    }
    readAppliedParent()
    while (in.tok == Tok.With) { in.next(); readAppliedParent() }
    parents.toList
  }

  /** {{{
   *  ClassTemplate ::= [EarlyDefs with] ClassParents [TemplateBody]
   *  TraitTemplate ::= [EarlyDefs with] TraitParents [TemplateBody]
   *  EarlyDefs     ::= `{' [EarlyDef {semi EarlyDef}] `}'
   *  EarlyDef      ::= Annotations Modifiers PatDef
   *  }}}
   */
  def template(): (List[Stmt.Early], List[Aux.Parent], Aux.Self, List[Stmt.Template], Boolean) = {
    newLineOptWhenFollowedBy(Tok.`{`)
    if (in.tok == Tok.`{`) {
      // @S: pre template body cannot stub like post body can!
      val (self, body) = templateBody(isPre = true)
      if (in.tok == Tok.With && self.name.isEmpty && self.decltpe.isEmpty) {
        val edefs = body.map(ensureEarlyDef)
        in.next()
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

  def ensureEarlyDef(tree: Stmt.Template): Stmt.Early = tree match {
    case v: Defn.Val => v
    case v: Defn.Var => v
    case t: Defn.Type =>
      syntaxError(t, "early type members are not allowed any longer. " +
                     "Move them to the regular body: the semantics are the same.")
    case other =>
      syntaxError(other, "not a valid early definition")
  }

  /** {{{
   *  ClassTemplateOpt ::= `extends' ClassTemplate | [[`extends'] TemplateBody]
   *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[`extends'] TemplateBody] | `<:' TemplateBody
   *  TraitExtends     ::= `extends' | `<:'
   *  }}}
   */
  def templateOpt(owner: TemplateOwner): Aux.Template = {
    val (early, parents, self, body, hasStats) = (
      if (in.tok == Tok.Extends /* || in.tok == Tok.`<:` && mods.isTrait */) {
        in.next()
        template()
      }
      else {
        newLineOptWhenFollowedBy(Tok.`{`)
        val (self, body, hasStats) = templateBodyOpt(parenMeansSyntaxError = owner.isTrait || owner.isTerm)
        (Nil, Nil, self, body, hasStats)
      }
    )
    if (hasStats) Aux.Template(early, parents, self, body)
    else Aux.Template(early, parents, self)
  }

/* -------- TEMPLATES ------------------------------------------- */

  /** {{{
   *  TemplateBody ::= [nl] `{' TemplateStatSeq `}'
   *  }}}
   * @param isPre specifies whether in early initializer (true) or not (false)
   */
  def templateBody(isPre: Boolean): (Aux.Self, List[Stmt.Template]) =
    inBraces(templateStatSeq(isPre = isPre))

  def templateBodyOpt(parenMeansSyntaxError: Boolean): (Aux.Self, List[Stmt.Template], Boolean) = {
    newLineOptWhenFollowedBy(Tok.`{`)
    if (in.tok == Tok.`{`) {
      val (self, stats) = templateBody(isPre = false)
      (self, stats, true)
    } else {
      if (in.tok == Tok.`(`) {
        if (parenMeansSyntaxError) syntaxError("traits or objects may not have parameters")
        else abort("unexpected opening parenthesis")
      }
      (Aux.Self(None, None, hasThis = false), Nil, false)
    }
  }

  /** {{{
   *  Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
   *  }}}
   */
  def refinement(): List[Stmt.Refine] = inBraces(refineStatSeq())

  def existentialStats(): List[Stmt.Existential] = refinement() map {
    case stmt: Stmt.Existential => stmt
    case other                  => syntaxError(other, "not a legal existential clause")
  }

/* -------- STATSEQS ------------------------------------------- */

  def statSeq[T <: Tree](statpf: PartialFunction[Tok, T],
                         errorMsg: String = "illegal start of definition"): List[T] = {
    val stats = new ListBuffer[T]
    while (!isStatSeqEnd) {
      if (statpf.isDefinedAt(in.tok)) stats += statpf(in.tok)
      else if (!isStatSep) syntaxError(errorMsg)
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
  def topStatSeq(): List[Stmt.TopLevel] = statSeq(topStat, errorMsg = "expected class or object definition")
  def topStat: PartialFunction[Tok, Stmt.TopLevel] = {
    case Tok.Package  =>
      in.next()
      packageOrPackageObject()
    case Tok.Import =>
      importStmt()
    case _ if isAnnotation || isTemplateIntro || isModifier =>
      topLevelTmplDef
  }

  /** {{{
   *  TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStats
   *  }}}
   * @param isPre specifies whether in early initializer (true) or not (false)
   */
  def templateStatSeq(isPre : Boolean): (Aux.Self, List[Stmt.Template]) = {
    var self: Aux.Self = Aux.Self(None, None, hasThis = false)
    var firstOpt: Option[Term] = None
    if (isExprIntro) {
      val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
      if (in.tok == Tok.`=>`) {
        first match {
          case name: Name =>
            self = Aux.Self(Some(name.toTermName), None, hasThis = false)
          case Term.Placeholder() =>
            self = Aux.Self(None, None, hasThis = false)
          case Term.This(None) =>
            self = Aux.Self(None, None, hasThis = true)
          case Term.Ascribe(name: Name, tpt) =>
            self = Aux.Self(Some(name.toTermName), Some(tpt), hasThis = false)
          case Term.Ascribe(Term.Placeholder(), tpt) =>
            self = Aux.Self(None, Some(tpt), hasThis = false)
          case Term.Ascribe(tree @ Term.This(None), tpt) =>
            self = Aux.Self(None, Some(tpt), hasThis = true)
          case _ =>
        }
        in.next()
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
  def templateStats(): List[Stmt.Template] = statSeq(templateStat)
  def templateStat: PartialFunction[Tok, Stmt.Template] = {
    case Tok.Import =>
      importStmt()
    case _ if isDefIntro || isModifier || isAnnotation =>
      nonLocalDefOrDcl
    case _ if isExprIntro =>
      expr(InTemplate)
  }

  /** {{{
   *  RefineStatSeq    ::= RefineStat {semi RefineStat}
   *  RefineStat       ::= Dcl
   *                     | type TypeDef
   *                     |
   *  }}}
   */
  def refineStatSeq(): List[Stmt.Refine] = {
    val stats = new ListBuffer[Stmt.Refine]
    while (!isStatSeqEnd) {
      stats ++= refineStat()
      if (in.tok != Tok.`}`) acceptStatSep()
    }
    stats.toList
  }

  def refineStat(): Option[Stmt.Refine] =
    if (isDclIntro) {
      defOrDclOrCtor(Nil) match {
        case sr: Stmt.Refine => Some(sr)
        case other           => syntaxError(other, "is not a valid refinement declaration")
      }
    } else if (!isStatSep) {
      syntaxError(
        "illegal start of declaration"+
        (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
         else ""))
    } else None

  def localDef(implicitMod: Option[Mod.Implicit]): Stmt.Block = {
    val mods = (implicitMod ++: annots(skipNewLines = true)) ++ localModifiers()
    if (mods forall { case _: Mod.Implicit | _: Mod.Lazy | _: Mod.Annot => true; case _ => false })
      (defOrDclOrCtor(mods) match {
        case sb: Stmt.Block => sb
        case other          => syntaxError(other, "is not a valid block statement")
      })
    else
      (tmplDef(mods) match {
        case sb: Stmt.Block => sb
        case other          => syntaxError(other, "is not a valid block statement")
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
  def blockStatSeq(): List[Stmt.Block] = {
    val stats = new ListBuffer[Stmt.Block]
    while (!isStatSeqEnd && !isCaseDefEnd) {
      if (in.tok == Tok.Import) {
        stats += importStmt()
        acceptStatSepOpt()
      }
      else if (isDefIntro || isLocalModifier || isAnnotation) {
        if (in.tok == Tok.Implicit) {
          in.next()
          if (isName) stats += implicitClosure(InBlock)
          else stats += localDef(Some(Mod.Implicit()))
        } else {
          stats += localDef(None)
        }
        acceptStatSepOpt()
      }
      else if (isExprIntro) {
        stats += expr(InBlock)
        if (!isCaseDefEnd) acceptStatSep()
      }
      else if (isStatSep) {
        in.next()
      }
      else {
        val addendum = if (isModifier) " (no modifiers allowed here)" else ""
        syntaxError("illegal start of statement" + addendum)
      }
    }
    stats.toList
  }


  def packageOrPackageObject(): Stmt.TopLevel =
    if (in.tok == Tok.Object) {
      in.next()
      packageObject()
    } else {
      Pkg(qualId(), inBracesOrNil(topStatSeq()), hasBraces = true)
    }

  def packageObject(): Defn.Object =
    Defn.Object(Mod.Package() :: Nil, termName(), templateOpt(OwnedByObject))

  /** {{{
   *  CompilationUnit ::= {package QualId semi} TopStatSeq
   *  }}}
   */
  def compilationUnit(): CompUnit = {
    def packageStats(): (List[Term.Ref], List[Stmt.TopLevel])  = {
      val refs = new ListBuffer[Term.Ref]
      val ts = new ListBuffer[Stmt.TopLevel]
      while (in.tok == Tok.`;`) in.next()
      if (in.tok == Tok.Package) {
        in.next()
        if (in.tok == Tok.Object) {
          in.next()
          ts += packageObject()
          if (in.tok != Tok.EOF) {
            acceptStatSep()
            ts ++= topStatSeq()
          }
        } else {
          val qid = qualId()

          if (in.tok == Tok.EOF) {
            refs += qid
          } else if (isStatSep) {
            in.next()
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
      case Nil          => Aux.CompUnit(stats)
      case init :+ last => Aux.CompUnit(init.foldRight(Pkg(last, stats, hasBraces = false)) { (ref, acc) => Pkg(ref, acc :: Nil, hasBraces = false) } :: Nil)
    }
  }
}
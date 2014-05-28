package scala.reflect
package syntactic

import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import scala.reflect.syntactic.util.settings
import scala.reflect.syntactic.util.Chars.{isOperatorPart, isScalaLetter}
import scala.reflect.syntactic.Tokens._
import scala.reflect.syntactic.TokenInfo._
import scala.reflect.core._, Aux._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalareflect.unreachable

object SyntacticInfo {
  private[reflect] val unaryOps = Set("-", "+", "~", "!")
  private[reflect] def isUnaryOp(s: String): Boolean = unaryOps contains s
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
      case _: Term.Name | Term.Select(_: Aux.Super, _) => true
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

class SourceParser(val source: Source) extends Parser {
  def this(code: String) = this(Source.String(code))
  /** The parse starting point depends on whether the source file is self-contained:
   *  if not, the AST will be supplemented.
   */
  def parseStartRule = () => compilationUnit()

  lazy val in = { val s = new SourceFileScanner(source); s.init(); s }

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

abstract class Parser { parser =>
  val in: Scanner
  val source: Source
  implicit val origin: Origin = Origin.Source(source)

  /** Scoping operator used to temporarily look into the future.
   *  Backs up scanner data before evaluating a block and restores it after.
   */
  @inline final def lookingAhead[T](body: => T): T = {
    val saved = new ScannerData {} copyFrom in
    in.nextToken()
    try body finally in copyFrom saved
  }

  /** Perform an operation while peeking ahead.
   *  Pushback if the operation yields an empty tree or blows to pieces.
   */
  @inline def peekingAhead[T](tree: => T): T = {
    @inline def peekahead() = {
      in.prev copyFrom in
      in.nextToken()
    }
    @inline def pushback() = {
      in.next copyFrom in
      in copyFrom in.prev
    }
    peekahead()
    // try it, in case it is recoverable
    try tree catch { case e: Exception => pushback() ; throw e }
  }

  def parseStartRule: () => Tree

  def parseRule[T](rule: this.type => T): T = {
    val t = rule(this)
    accept(EOF)
    t
  }

  /** This is the general parse entry point.
   */
  def parse(): Tree = parseRule(_.parseStartRule())

  /** These are alternative entry points for repl, script runner, toolbox and parsing in macros.
   */
  def parseStats(): List[Tree] = parseRule(_.templateStats())

/* ------------- PARSER COMMON -------------------------------------------- */

  /** Methods inParensOrError and similar take a second argument which, should
   *  the next token not be the expected opener (e.g. LPAREN) will be returned
   *  instead of the contents of the groupers.  However in all cases accept(LPAREN)
   *  will be called, so a parse error will still result.  If the grouping is
   *  optional, in.token should be tested before calling these methods.
   */
  @inline final def inParens[T](body: => T): T = {
    accept(LPAREN)
    val ret = body
    accept(RPAREN)
    ret
  }
  @inline final def inParensOrError[T](body: => T, alt: T): T =
    if (in.token == LPAREN) inParens(body)
    else { accept(LPAREN) ; alt }

  @inline final def inParensOrUnit[T, Ret >: Lit](body: => Ret): Ret = inParensOrError(body, Lit.Unit())
  @inline final def inParensOrNil[T](body: => List[T]): List[T] = inParensOrError(body, Nil)

  @inline final def inBraces[T](body: => T): T = {
    accept(LBRACE)
    val ret = body
    accept(RBRACE)
    ret
  }
  @inline final def inBracesOrError[T](body: => T, alt: T): T =
    if (in.token == LBRACE) inBraces(body)
    else { accept(LBRACE) ; alt }

  @inline final def inBracesOrNil[T](body: => List[T]): List[T] = inBracesOrError(body, Nil)
  @inline final def inBracesOrUnit[T](body: => Term): Term = inBracesOrError(body, Lit.Unit())
  @inline final def dropAnyBraces[T](body: => T): T =
    if (in.token == LBRACE) inBraces(body)
    else body

  @inline final def inBrackets[T](body: => T): T = {
    accept(LBRACKET)
    val ret = body
    accept(RBRACKET)
    ret
  }

/* ------------- ERROR HANDLING ------------------------------------------- */

  val assumedClosingParens = mutable.Map(RPAREN -> 0, RBRACKET -> 0, RBRACE -> 0)

  private var inFunReturnType = false
  @inline private def fromWithinReturnType[T](body: => T): T = {
    val saved = inFunReturnType
    inFunReturnType = true
    try body
    finally inFunReturnType = saved
  }

  protected def skip(targetToken: Token) {
    var nparens = 0
    var nbraces = 0
    while (true) {
      in.token match {
        case EOF =>
          return
        case SEMI =>
          if (nparens == 0 && nbraces == 0) return
        case NEWLINE =>
          if (nparens == 0 && nbraces == 0) return
        case NEWLINES =>
          if (nparens == 0 && nbraces == 0) return
        case RPAREN =>
          nparens -= 1
        case RBRACE =>
          if (nbraces == 0) return
          nbraces -= 1
        case LPAREN =>
          nparens += 1
        case LBRACE =>
          nbraces += 1
        case _ =>
      }
      if (targetToken == in.token && nparens == 0 && nbraces == 0) return
      in.nextToken()
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
  def expectedMsg(token: Token): String = expectedMsgTemplate(token2string(token), token2string(in.token))

  /** Consume one token of the specified type, or signal an error if it is not there. */
  def accept(token: Token): Unit =
    if (in.token == token) in.nextToken()
    else syntaxError(expectedMsg(token))

  /** {{{
   *  semi = nl {nl} | `;`
   *  nl  = `\n' // where allowed
   *  }}}
   */
  def acceptStatSep(): Unit = in.token match {
    case NEWLINE | NEWLINES => in.nextToken()
    case _                  => accept(SEMI)
  }
  def acceptStatSepOpt() =
    if (!isStatSeqEnd)
      acceptStatSep()

/* -------------- TOKEN CLASSES ------------------------------------------- */

  def isModifier: Boolean = in.token match {
    case ABSTRACT | FINAL | SEALED | PRIVATE |
         PROTECTED | OVERRIDE | IMPLICIT | LAZY => true
    case _ => false
  }

  def isAnnotation: Boolean = in.token == AT

  def isLocalModifier: Boolean = in.token match {
    case ABSTRACT | FINAL | SEALED | IMPLICIT | LAZY => true
    case _ => false
  }

  def isTemplateIntro: Boolean = in.token match {
    case OBJECT | CASEOBJECT | CLASS | CASECLASS | TRAIT  => true
    case _                                                => false
  }
  def isDclIntro: Boolean = in.token match {
    case VAL | VAR | DEF | TYPE => true
    case _ => false
  }

  def isDefIntro = isTemplateIntro || isDclIntro

  def isNumericLit: Boolean = in.token match {
    case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT => true
    case _ => false
  }

  def isNameExcept(except: String) = isName && in.name != except
  def isNameOf(name: String)       = isName && in.name == name

  def isUnaryOp = isName && SyntacticInfo.isUnaryOp(in.name)
  def isRawStar = isName && in.name == "*"
  def isRawBar  = isName && in.name == "|"

  def isName = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT
  def isMacro = in.token == IDENTIFIER && in.name == "macro"

  def isLiteralToken(token: Token) = token match {
    case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
         STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL => true
    case _                                                        => false
  }
  def isLiteral = isLiteralToken(in.token)

  def isExprIntroToken(token: Token): Boolean = isLiteralToken(token) || (token match {
    case IDENTIFIER | BACKQUOTED_IDENT | INTERPOLATIONID |
         THIS | SUPER | IF | FOR | NEW | USCORE | TRY | WHILE |
         DO | RETURN | THROW | LPAREN | LBRACE | XMLSTART => true
    case _ => false
  })

  def isExprIntro: Boolean = isExprIntroToken(in.token)

  def isTypeIntroToken(token: Token): Boolean = token match {
    case IDENTIFIER | BACKQUOTED_IDENT | THIS |
         SUPER | USCORE | LPAREN | AT => true
    case _ => false
  }

  def isStatSeqEnd: Boolean = isStatSeqEnd(in.token)
  def isStatSeqEnd(tok: Token): Boolean = tok == RBRACE || tok == EOF

  def isCaseDefEnd = in.token == RBRACE || in.token == CASE || in.token == EOF

  def isStatSep(token: Token): Boolean =
    token == NEWLINE || token == NEWLINES || token == SEMI || in.token == EOF

  def isStatSep: Boolean = isStatSep(in.token)


/* ---------- TREE CONSTRUCTION ------------------------------------------- */

  /** Convert tree to formal parameter list. */
  def convertToParams(tree: Term): List[Aux.Param] = tree match {
    case Term.Tuple(ts) => ts.toList flatMap convertToParam
    case _              => List(convertToParam(tree)).flatten
  }

  /** Convert tree to formal parameter. */
  def convertToParam(tree: Term): Option[Aux.Param] = tree match {
    case name: Name =>
      Some(Aux.Param.Named(Nil, name.toTermName, None, None))
    case Term.Placeholder() =>
      Some(Aux.Param.Anonymous(Nil, None))
    case Term.Ascribe(name: Name, tpt) =>
      Some(Aux.Param.Named(Nil, name.toTermName, Some(tpt), None))
    case Term.Ascribe(Term.Placeholder(), tpt) =>
      Some(Aux.Param.Anonymous(Nil, Some(tpt)))
    case Lit.Unit() =>
      None
    case other =>
      syntaxError(other, s"$other, not a legal formal parameter")
  }

  def convertToTypeId(ref: Term.Ref): Option[Type] = ref match {
    case Term.Select(qual: Type.Qualifier, name) =>
      Some(Type.Select(qual, name.toTypeName))
    case name: Term.Name =>
      Some(name.toTypeName)
    case _ =>
      None
  }

  /** {{{ part { `sep` part } }}},or if sepFirst is true, {{{ { `sep` part } }}}. */
  final def tokenSeparated[T](separator: Token, sepFirst: Boolean, part: => T): List[T] = {
    val ts = new ListBuffer[T]
    if (!sepFirst)
      ts += part

    while (in.token == separator) {
      in.nextToken()
      ts += part
    }
    ts.toList
  }

  @inline final def commaSeparated[T](part: => T): List[T] =
    tokenSeparated(COMMA, sepFirst = false, part)

  @inline final def caseSeparated[T](part: => T): List[T] =
    tokenSeparated(CASE, sepFirst = true, part)

  def readAnnots(part: => Mod.Annot): List[Mod.Annot] =
    tokenSeparated(AT, sepFirst = true, part)

  def makeTuple[T <: Tree](body: List[T], zero: () => T, tuple: List[T] => T): T = body match {
    case Nil         => zero()
    case only :: Nil => only
    case _           => tuple(body)
  }

  def makeTupleTerm(body: List[Term]): Term = {
    makeTuple[Term](body, () => Lit.Unit(), Term.Tuple(_))
  }

  def makeTupleTermParens(bodyf: => List[Term]) =
    makeTupleTerm(inParens(if (in.token == RPAREN) Nil else bodyf))

  // TODO: make zero tuple for types Lit.Unit() too?
  def makeTupleType(body: List[Type]): Type =
    makeTuple[Type](body, () => unreachable, Type.Tuple(_))

  def makeTuplePatParens(bodyf: => List[Pat]): Pat = {
    val body = inParens(if (in.token == RPAREN) Nil else bodyf)
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
        val targs = if (in.token == LBRACKET) exprTypeArgs() else Nil
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
        if (in.token == LBRACKET) syntaxError("infix patterns cannot have type arguments")
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
    }, opinfo.operator) :: Nil

  def finishBinaryOp[T: OpCtx](opinfo: OpInfo[T], rhs: T): T = opctx.binop(opinfo, rhs)

  def reduceStack[T: OpCtx](base: List[OpInfo[T]], top: T): T = {
    val opPrecedence = if (isName) Term.Name(in.name)(isBackquoted = false).precedence else 0
    val leftAssoc    = !isName || Term.Name(in.name)(isBackquoted = false).isLeftAssoc

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
    def functionArgType(): ParamType

    private def tupleInfixType(): Type = {
      in.nextToken()
      if (in.token == RPAREN) {
        in.nextToken()
        accept(ARROW)
        Type.Function(Nil, typ())
      }
      else {
        val ts = functionTypes()
        accept(RPAREN)
        if (in.token == ARROW) {
          in.skipToken()
          Type.Function(ts, typ())
        } else {
          val tuple = makeTupleType(ts map {
            case t: Type               => t
            case p: ParamType.ByName   => syntaxError(p, "by name type not allowed here")
            case p: ParamType.Repeated => syntaxError(p, "repeated type not alloed here")
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
        if (in.token == LPAREN) tupleInfixType()
        else infixType(InfixMode.FirstOp)

      in.token match {
        case ARROW    => in.skipToken(); Type.Function(List(t), typ())
        case FORSOME  => in.skipToken(); makeExistentialTypeTree(t)
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
      simpleTypeRest(in.token match {
        case LPAREN => makeTupleType(inParens(types()))
        case USCORE => in.nextToken(); wildcardType()
        case _      =>
          val ref: Term.Ref = path()
          if (in.token != DOT)
            convertToTypeId(ref) getOrElse { syntaxError("identifier expected") }
          else {
            in.nextToken()
            accept(TYPE)
            Type.Singleton(ref)
          }
      })
    }

    private def typeProjection(qual: Type): Type.Project = {
      in.skipToken()
      Type.Project(qual, typeName())
    }
    def simpleTypeRest(t: Type): Type = in.token match {
      case HASH     => simpleTypeRest(typeProjection(t))
      case LBRACKET => simpleTypeRest(Type.Apply(t, typeArgs()))
      case _        => t
    }

    /** {{{
     *  CompoundType ::= ModType {with ModType} [Refinement]
     *                |  Refinement
     *  }}}
     */
    def compoundType(): Type = compoundTypeRest(
      if (in.token == LBRACE) None
      else Some(annotType())
    )

    // TODO: warn about def f: Unit { } case?
    def compoundTypeRest(t: Option[Type]): Type = {
      val ts = new ListBuffer[Type] ++ t
      while (in.token == WITH) {
        in.nextToken()
        ts += annotType()
      }
      newLineOptWhenFollowedBy(LBRACE)
      val types                 = ts.toList
      val hasExplicitRefinement = in.token == LBRACE
      val refinements           = if (in.token == LBRACE) refinement() else Nil
      (types, refinements) match {
        case (typ :: Nil, Nil) => typ
        case _                 => Type.Compound(types, refinements)(hasExplicitRefinement)
      }
    }

    def infixTypeRest(t: Type, mode: InfixMode.Value): Type = {
      if (isName && in.name != "*") {
        val name = Term.Name(in.name)(isBackquoted = false)
        val leftAssoc = name.isLeftAssoc
        if (mode != InfixMode.FirstOp) checkAssoc(name, leftAssoc = mode == InfixMode.LeftOp)
        val op = typeName()
        newLineOptWhenFollowing(isTypeIntroToken)
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
    def functionTypes(): List[ParamType] = commaSeparated(functionArgType())
  }

  // TODO: can we get away without this?
  implicit class NameToName(name: Name) {
    def toTypeName: Type.Name = name match {
      case name: Type.Name => name
      case _              => Type.Name(name.value)(name.isBackquoted)
    }
    def toTermName: Term.Name = name match {
      case name: Term.Name => name
      case _              => Term.Name(name.value)(name.isBackquoted)
    }
    def toEitherName: Name.Either = name match {
      case name: Name.Either => name
      case _                 => Name.Either(name.value)(name.isBackquoted)
    }
    def toBothName: Name.Both = name match {
      case name: Name.Both => name
      case _               => Name.Both(name.value)(name.isBackquoted)
    }
  }

  def termName(): Term.Name =
    if (!isName) syntaxError(expectedMsg(IDENTIFIER))
    else {
      val name = in.name
      val isBackquoted = in.token == BACKQUOTED_IDENT
      in.nextToken()
      Term.Name(name)(isBackquoted)
    }

  /** For when it's known already to be a type name. */
  def typeName(): Type.Name = termName().toTypeName

  /** {{{
   *  Path       ::= StableId
   *              |  [Ident `.'] this
   *  ModType ::= Path [`.' type]
   *  }}}
   */
  // TODO: this has to be rewritten
  def path(thisOK: Boolean = true): Term.Ref = {
    def stop = in.token != DOT || lookingAhead { in.token != THIS && in.token != SUPER && !isName }
    if (in.token == THIS) {
      in.nextToken()
      val thisnone = Term.This(None)
      if (stop && thisOK) thisnone
      else {
        accept(DOT)
        selectors(thisnone)
      }
    } else if (in.token == SUPER) {
      in.nextToken()
      val superp = Aux.Super(None, mixinQualifierOpt())
      accept(DOT)
      val supersel = Term.Select(superp, termName())
      if (stop) supersel
      else {
        in.skipToken()
        selectors(supersel)
      }
    } else {
      val name = termName()
      if (stop) name
      else {
        in.nextToken()
        if (in.token == THIS) {
          in.nextToken()
          val thisid = Term.This(Some(name.toEitherName))
          if (stop && thisOK) thisid
          else {
            accept(DOT)
            selectors(thisid)
          }
        } else if (in.token == SUPER) {
          in.nextToken()
          val superp = Aux.Super(Some(name.toEitherName), mixinQualifierOpt())
          accept(DOT)
          val supersel = Term.Select(superp, termName())
          if (stop) supersel
          else {
            in.skipToken()
            selectors(supersel)
          }
        } else {
          selectors(name)
        }
      }
    }
  }

  def selector(t: Term): Term.Select = Term.Select(t, termName())
  def selectors(t: Term.Ref): Term.Ref ={
    val t1 = selector(t)
    if (in.token == DOT && lookingAhead { isName }) {
      in.nextToken()
      selectors(t1)
    }
    else t1
  }

  /** {{{
  *   MixinQualifier ::= `[' Id `]'
  *   }}}
  */
  def mixinQualifierOpt(): Option[Type.Name] =
    if (in.token == LBRACKET) Some(inBrackets(typeName()))
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
    if (in.token != DOT) name
    else {
      in.skipToken()
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
    val res = in.token match {
      case CHARLIT                => Lit.Char(in.charVal)
      case INTLIT                 => Lit.Int(in.intVal(isNegated).toInt)
      case LONGLIT                => Lit.Long(in.intVal(isNegated))
      case FLOATLIT               => Lit.Float(in.floatVal(isNegated).toFloat)
      case DOUBLELIT              => Lit.Double(in.floatVal(isNegated))
      case STRINGLIT | STRINGPART => Lit.String(in.strVal.intern())
      case TRUE                   => Lit.Bool(true)
      case FALSE                  => Lit.Bool(false)
      case NULL                   => Lit.Null()
      case SYMBOLLIT              => Lit.Symbol(scala.Symbol(in.strVal))
      case _                      => syntaxError("illegal literal")
    }
    in.nextToken()
    res
  }

  def interpolate[Ctx, Ret](arg: () => Ctx, result: (Term.Name, List[Lit.String], List[Ctx]) => Ret): Ret = {
    def part() =
      if (in.token != STRINGPART && in.token != STRINGLIT) syntaxError(expectedMsg(STRINGPART))
      else {
        val lit = Lit.String(in.strVal.intern())
        in.nextToken()
        lit
      }
    val interpolator = Term.Name(in.name)(isBackquoted = false) // termName() for INTERPOLATIONID
    in.nextToken()
    val partsBuf = new ListBuffer[Lit.String]
    val argsBuf = new ListBuffer[Ctx]
    while (in.token == STRINGPART) {
      partsBuf += part()
      argsBuf += arg()
    }
    if (in.token == STRINGLIT) partsBuf += part()
    result(interpolator, partsBuf.toList, argsBuf.toList)
  }

  def interpolateTerm(): Term.Interpolate =
    interpolate[Term, Term.Interpolate](arg = { () =>
      in.token match {
        case IDENTIFIER => termName()
        //case USCORE   => freshPlaceholder()  // ifonly etapolation
        case LBRACE     => expr()              // dropAnyBraces(expr0(Local))
        case THIS       => in.nextToken(); Term.This(None)
        case _          => syntaxError("error in interpolated string: identifier or block expected")
      }
    }, result = Term.Interpolate(_, _, _))

  def interpolatePat(): Pat.Interpolate =
    interpolate[Pat, Pat.Interpolate](arg = () => dropAnyBraces(pattern()), result = Pat.Interpolate(_, _, _))

/* ------------- NEW LINES ------------------------------------------------- */

  def newLineOpt(): Unit = {
    if (in.token == NEWLINE) in.nextToken()
  }

  def newLinesOpt(): Unit = {
    if (in.token == NEWLINE || in.token == NEWLINES)
      in.nextToken()
  }

  def newLineOptWhenFollowedBy(token: Offset): Unit = {
    // note: next is defined here because current == NEWLINE
    if (in.token == NEWLINE && in.next.token == token) newLineOpt()
  }

  def newLineOptWhenFollowing(p: Token => Boolean): Unit = {
    // note: next is defined here because current == NEWLINE
    if (in.token == NEWLINE && p(in.next.token)) newLineOpt()
  }

/* ------------- TYPES ---------------------------------------------------- */

  /** {{{
   *  TypedOpt ::= [`:' Type]
   *  }}}
   */
  def typedOpt(): Option[Type] =
    if (in.token == COLON) { in.nextToken(); Some(typ()) }
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
    if (in.token == LPAREN) {
      in.nextToken()
      val r = expr()
      accept(RPAREN)
      r
    } else {
      accept(LPAREN)
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

  def expr(location: Location): Term = (in.token: @scala.annotation.switch) match {
    case IF =>
      in.skipToken()
      val cond = condExpr()
      newLinesOpt()
      val thenp = expr()
      if (in.token == ELSE) { in.nextToken(); Term.If.ThenElse(cond, thenp, expr()) }
      else { Term.If.Then(cond, thenp) }
    case TRY =>
      in.skipToken()
      val body: Term = in.token match {
        case LBRACE => inBracesOrUnit(block())
        case LPAREN => inParensOrUnit(expr())
        case _      => expr()
      }
      val catchopt =
        if (in.token != CATCH) None
        else {
          in.nextToken()
          if (in.token != LBRACE) Some(expr())
          else inBraces {
            if (in.token == CASE) Some(Term.Cases(caseClauses()))
            else Some(expr())
          }
        }
      val finallyopt = in.token match {
        case FINALLY => in.nextToken(); Some(expr())
        case _       => None
      }
      Term.Try(body, catchopt, finallyopt)
    case WHILE =>
      in.skipToken()
      val cond = condExpr()
      newLinesOpt()
      val body = expr()
      Term.While(cond, body)
    case DO =>
      in.skipToken()
      val body = expr()
      if (isStatSep) in.nextToken()
      accept(WHILE)
      val cond = condExpr()
      Term.Do(body, cond)
    case FOR =>
      in.nextToken()
      val enums =
        if (in.token == LBRACE) inBracesOrNil(enumerators())
        else inParensOrNil(enumerators())
      newLinesOpt()
      if (in.token == YIELD) {
        in.nextToken()
        Term.ForYield(enums, expr())
      } else {
        Term.For(enums, expr())
      }
    case RETURN =>
      in.skipToken()
      Term.Return(if (isExprIntro) Some(expr()) else None)
    case THROW =>
      in.skipToken()
      Term.Throw(expr())
    case IMPLICIT =>
      implicitClosure(location)
    case _ =>
      var t: Term = postfixExpr()
      if (in.token == EQUALS) {
        t match {
          case ref: Term.Ref =>
            in.skipToken()
            t = Term.Assign(ref, expr())
          case app: Term.Apply =>
            in.skipToken()
            t = Term.Update(app, expr())
          case _ =>
        }
      } else if (in.token == COLON) {
        val colonPos = in.skipToken()
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
      } else if (in.token == MATCH) {
        in.skipToken()
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
      if (in.token == ARROW && (location != InTemplate || lhsIsTypedParamList)) {
        in.skipToken()
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
      if (in.token != COLON) name
      else {
        in.nextToken()
        Term.Ascribe(expr, typeOrInfixType(location))
      }
    }.get
    val param = param0.mapMods(Mod.Implicit() +: _)
    accept(ARROW)
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
        newLineOptWhenFollowing(isExprIntroToken)
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
      if (isLiteral) literal()
      else in.token match {
        case INTERPOLATIONID =>
          interpolateTerm()
        case XMLSTART =>
          xmlLiteral()
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          path()
        case USCORE =>
          in.nextToken()
          Term.Placeholder()
        case LPAREN =>
          makeTupleTermParens(commaSeparated(expr()))
        case LBRACE =>
          canApply = false
          blockExpr()
        case NEW =>
          canApply = false
          in.nextToken()
          val (edefs, parents, self, stats, hasExplicitBody) = template()
          Term.New(Aux.Template(edefs, parents, self, stats)(hasExplicitBody = hasExplicitBody))
        case _ =>
          syntaxError("illegal start of simple expression")
      }
    simpleExprRest(t, canApply = canApply)
  }

  def simpleExprRest(t: Term, canApply: Boolean): Term = {
    if (canApply) newLineOptWhenFollowedBy(LBRACE)
    in.token match {
      case DOT =>
        in.nextToken()
        simpleExprRest(selector(t), canApply = true)
      case LBRACKET =>
        t match {
          case _: Term.Name | _: Term.Select | _: Term.Apply =>
            var app: Term = t
            while (in.token == LBRACKET)
              app = Term.ApplyType(app, exprTypeArgs())

            simpleExprRest(app, canApply = true)
          case _ =>
            t
        }
      case LPAREN | LBRACE if (canApply) =>
        simpleExprRest(Term.Apply(t, argumentExprs()), canApply = true)
      case USCORE =>
        in.skipToken()
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
    if (in.token != LBRACE && in.token != LPAREN) prefixExpr() :: Nil
    else {
      val args = argumentExprs()
      in.token match {
        case DOT | LBRACKET | LPAREN | LBRACE | USCORE =>
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
        case Term.Ascribe(t, Type.Placeholder(Aux.TypeBounds(None, None))) if isName && in.name == "*" =>
          in.nextToken()
          Arg.Repeated(t)
        case Term.Assign(t: Term.Name, rhs) =>
          Arg.Named(t, rhs)
        case other =>
          other
      }
    }
    in.token match {
      case LBRACE   => List(blockExpr())
      case LPAREN   => inParens(if (in.token == RPAREN) Nil else args())
      case _        => Nil
    }
  }

  /** A succession of argument lists. */
  def multipleArgumentExprs(): List[List[Arg]] = {
    if (in.token != LPAREN) Nil
    else argumentExprs() :: multipleArgumentExprs()
  }

  /** {{{
   *  BlockExpr ::= `{' (CaseClauses | Block) `}'
   *  }}}
   */
  def blockExpr(): Term = {
    inBraces {
      if (in.token == CASE) Term.Cases(caseClauses())
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
    Aux.Case(pattern(), guard(), { accept(ARROW); blockStatSeq() })

  /** {{{
   *  CaseClauses ::= CaseClause {CaseClause}
   *  CaseClause  ::= case Pattern [Guard] `=>' Block
   *  }}}
   */
  def caseClauses(): List[Aux.Case] = {
    val cases = caseSeparated { caseClause() }
    if (cases.isEmpty)  // trigger error if there are no cases
      accept(CASE)
    cases
  }

  /** {{{
   *  Guard ::= if PostfixExpr
   *  }}}
   */
  def guard(): Option[Term] =
    if (in.token == IF) { in.nextToken(); Some(postfixExpr()) }
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
      in.nextToken()
      enums ++= enumerator(isFirst = false)
    }
    enums.toList
  }

  def enumerator(isFirst: Boolean, allowNestedIf: Boolean = true): List[Enum] =
    if (in.token == IF && !isFirst) Enum.Guard(guard().get) :: Nil
    else generator(!isFirst, allowNestedIf)

  /** {{{
   *  Generator ::= Pattern1 (`<-' | `=') Expr [Guard]
   *  }}}
   */
  def generator(eqOK: Boolean, allowNestedIf: Boolean = true): List[Enum] = {
    val hasVal = in.token == VAL
    if (hasVal)
      in.nextToken()

    val pat   = noSeq.pattern1()
    val point = in.offset
    val hasEq = in.token == EQUALS

    if (hasVal) {
      if (hasEq) deprecationWarning("val keyword in for comprehension is deprecated")
      else syntaxError("val in for comprehension must be followed by assignment")
    }

    if (hasEq && eqOK) in.nextToken()
    else accept(LARROW)
    val rhs = expr()

    def loop(): List[Enum] =
      if (in.token != IF) Nil
      else Enum.Guard(guard().get) :: loop()

    val tail =
      if (allowNestedIf) loop()
      else Nil

    // why max? IDE stress tests have shown that lastOffset could be less than start,
    // I guess this happens if instead if a for-expression we sit on a closing paren.
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

    def functionArgType(): ParamType = paramType()
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
        else { in.nextToken(); Pat.Alternative(pat, pattern()) }
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
      case p @ (_: Term.Name | _: Pat.Wildcard) if in.token == COLON =>
        p match {
          case name: Term.Name if !name.isVarPattern =>
            syntaxError("Pattern variables must start with a lower-case letter. (SLS 8.1.1.)")
          case _ =>
        }
        in.nextToken()
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

      if (in.token != AT) p
      else p match {
        case Pat.Wildcard() =>
          in.nextToken()
          pattern3()
        case name: Term.Name if name.isVarPattern =>
          in.nextToken()
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
      def isCloseDelim = in.token match {
        case RBRACE => isXML
        case RPAREN => !isXML
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
      def isComma                = in.token == COMMA
      def isDelimiter            = in.token == RPAREN || in.token == RBRACE
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
    def simplePattern(onError: () => Nothing): Pat = in.token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
        val sid = stableId()
        in.token match {
          case INTLIT | LONGLIT | FLOATLIT | DOUBLELIT =>
            sid match {
              case Term.Name("-") =>
                return literal(isNegated = true)
              case _ =>
            }
          case _ =>
        }
        val targs = in.token match {
          case LBRACKET => typeArgs()
          case _        => Nil
        }
        (in.token, sid) match {
          case (LPAREN, _)                 => Pat.Extract(sid, targs, argumentPatterns())
          case (_, _) if targs.nonEmpty    => syntaxError("pattern must be a value")
          case (_, sid: Term.Ref with Pat) => sid
          case _                           => unreachable
        }
      case USCORE =>
        in.nextToken()
        Pat.Wildcard()
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
           STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL =>
        literal()
      case INTERPOLATIONID =>
        interpolatePat()
      case LPAREN =>
        makeTuplePatParens(noSeq.patterns())
      case XMLSTART =>
        xmlLiteralPattern()
      case _ =>
        onError()
    }
  }
  /** The implementation of the context sensitive methods for parsing outside of patterns. */
  object outPattern extends PatternContextSensitive {
    def argType(): Type = typ()
    def functionArgType(): ParamType = paramType()
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
    if (in.token == RPAREN) Nil
    else seqPatterns()
  }
  def xmlLiteralPattern(): Pat

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

  /** {{{
   *  AccessQualifier ::= `[' (Id | this) `]'
   *  }}}
   */
  def accessQualifierOpt(): Option[Mod.AccessQualifier] = {
    if (in.token != LBRACKET) None
    else {
      in.nextToken()
      val res = if (in.token != THIS) termName().toEitherName
                else { in.nextToken(); Term.This(None) }
      accept(RBRACKET)
      Some(res)
    }
  }

  /** {{{
   *  AccessModifier ::= (private | protected) [AccessQualifier]
   *  }}}
   */
  def accessModifierOpt(): Option[Mod] = in.token match {
    case PRIVATE   => in.nextToken(); Some(Mod.Private(accessQualifierOpt()) )
    case PROTECTED => in.nextToken(); Some(Mod.Protected(accessQualifierOpt()) )
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
      if (advance) in.nextToken()
      mods :+ mod
    }
    def acceptable = if (isLocal) isLocalModifier else true
    def loop(mods: List[Mod]): List[Mod] =
      if (!acceptable) mods
      else (in.token match {
        case ABSTRACT            => loop(addMod(mods, Mod.Abstract()))
        case FINAL               => loop(addMod(mods, Mod.Final()))
        case SEALED              => loop(addMod(mods, Mod.Sealed()))
        case IMPLICIT            => loop(addMod(mods, Mod.Implicit()))
        case LAZY                => loop(addMod(mods, Mod.Lazy()))
        case OVERRIDE            => loop(addMod(mods, Mod.Override()))
        case PRIVATE | PROTECTED =>
          mods.foreach { m =>
            if(m.isInstanceOf[Mod.Access]) syntaxError("duplicate private/protected qualifier")
          }
          val optmod = accessModifierOpt()
          optmod.map { mod => loop(addMod(mods, mod, advance = false)) }.getOrElse(mods)
        case NEWLINE if !isLocal => in.nextToken(); loop(mods)
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
    if (in.token == LPAREN) Mod.Annot(t, multipleArgumentExprs())
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
  def paramClauses(ownerIsType: Boolean, ownerIsCase: Boolean = false): (List[List[Aux.Param.Named]], List[Aux.Param.Named]) = {
    var parsedImplicits = false
    def paramClause(): List[Aux.Param.Named] = {
      if (in.token == RPAREN)
        return Nil

      if (in.token == IMPLICIT) {
        in.nextToken()
        parsedImplicits = true
      }
      commaSeparated(param(ownerIsCase, ownerIsType, isImplicit = parsedImplicits))
    }
    val paramss = new ListBuffer[List[Aux.Param.Named]]
    var implicits = List.empty[Aux.Param.Named]
    newLineOptWhenFollowedBy(LPAREN)
    while (!parsedImplicits && in.token == LPAREN) {
      in.nextToken()
      val clause = paramClause()
      if (!parsedImplicits)
        paramss += clause
      else
        implicits = clause
      accept(RPAREN)
      newLineOptWhenFollowedBy(LPAREN)
    }
    (paramss.toList, implicits)
  }

  /** {{{
   *  ParamType ::= Type | `=>' Type | Type `*'
   *  }}}
   */
  def paramType(): ParamType = in.token match {
    case ARROW =>
      in.nextToken()
      ParamType.ByName(typ())
    case _ =>
      val t = typ()
      if (!isRawStar) t
      else {
        in.nextToken()
        ParamType.Repeated(t)
      }
  }

  def param(ownerIsCase: Boolean, ownerIsType: Boolean, isImplicit: Boolean): Aux.Param.Named = {
    var mods: List[Mod] = annots(skipNewLines = false)
    if (ownerIsType) {
      mods ++= modifiers()
      mods.getAll[Mod.Lazy].foreach { m =>
        syntaxError(m, "lazy modifier not allowed here. Use call-by-name parameters instead")
      }
      in.token match {
        case VAL => mods = mods :+ Mod.ValParam(); in.nextToken()
        case VAR => mods = mods :+ Mod.VarParam(); in.nextToken()
        case _   =>
      }
    }
    val name = termName()
    val tpt = {
      accept(COLON)
      val tpt = paramType()
      if (tpt.isInstanceOf[ParamType.ByName]) {
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
      if (in.token != EQUALS) None
      else {
        in.nextToken()
        Some(expr())
      }
    Aux.Param.Named(mods, name, Some(tpt), default)
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
  def typeParamClauseOpt(ownerIsType: Boolean, ctxBoundsAllowed: Boolean): List[Aux.TypeParam] = {
    newLineOptWhenFollowedBy(LBRACKET)
    if (in.token != LBRACKET) Nil
    else inBrackets(commaSeparated {
      val mods = annots(skipNewLines = true)
      typeParam(mods, ownerIsType, ctxBoundsAllowed)
    })
  }

  def typeParam(annots: List[Mod.Annot], ownerIsType: Boolean, ctxBoundsAllowed: Boolean): Aux.TypeParam = {
    val mods =
      if (ownerIsType && isName) {
        if (in.name == "+") {
          in.nextToken()
          annots :+ Mod.Covariant()
        } else if (in.name == "-") {
          in.nextToken()
          annots :+ Mod.Contravariant()
        } else annots
      } else annots
    val nameopt =
      if (isName) Some(typeName())
      else if (in.token == USCORE) { in.nextToken(); None }
      else syntaxError("identifier or `_' expected")
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    val bounds = typeBounds()
    val contextBounds = new ListBuffer[Type]
    val viewBounds = new ListBuffer[Type]
    if (ctxBoundsAllowed) {
      while (in.token == VIEWBOUND) {
        if (settings.future) {
          val msg = ("Use an implicit parameter instead.\n" +
                     "Example: Instead of `def f[A <% Int](a: A)` " +
                     "use `def f[A](a: A)(implicit ev: A => Int)`.")
          deprecationWarning(s"View bounds are deprecated. $msg")
        }
        in.nextToken()
        viewBounds += typ()
      }
      while (in.token == COLON) {
        in.nextToken()
        contextBounds += typ()
      }
    }
    nameopt match {
      case Some(name) => Aux.TypeParam.Named(mods, name, tparams, contextBounds.toList, viewBounds.toList, bounds)
      case None => Aux.TypeParam.Anonymous(mods, tparams, contextBounds.toList, viewBounds.toList, bounds)
    }
  }

  /** {{{
   *  TypeBounds ::= [`>:' Type] [`<:' Type]
   *  }}}
   */
  def typeBounds(): Aux.TypeBounds = {
    val lo = bound(SUPERTYPE)
    val hi = bound(SUBTYPE)
    Aux.TypeBounds(lo, hi)
  }

  def bound(tok: Token): Option[Type] =
    if (in.token == tok) { in.nextToken(); Some(typ()) } else None

/* -------- DEFS ------------------------------------------- */


  /** {{{
   *  Import  ::= import ImportExpr {`,' ImportExpr}
   *  }}}
   */
  def importStmt(): Import = {
    accept(IMPORT)
    Import(commaSeparated(importClause()))
  }

  /** {{{
   *  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
   *  }}}
   */
  def importClause(): Import.Clause = {
    val sid = stableId()
    def dotselectors = { accept(DOT); Import.Clause(sid, importSelectors()) }
    sid match {
      case Term.Select(sid: Term.Ref, name: Term.Name) if sid.isStableId =>
        if (in.token == DOT) dotselectors
        else Import.Clause(sid, Import.Selector.Name(name.toBothName) :: Nil)
      case _ => dotselectors
    }
  }

  /** {{{
   *  ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
   *  }}}
   */
  def importSelectors(): List[Import.Selector] =
    if (in.token != LBRACE) List(importWildcardOrName())
    else inBraces(commaSeparated(importSelector()))

  def importWildcardOrName(): Import.Selector =
    if (in.token == USCORE) { in.nextToken(); Import.Selector.Wildcard() }
    else { val name = termName(); Import.Selector.Name(name.toBothName) }

  /** {{{
   *  ImportSelector ::= Id [`=>' Id | `=>' `_']
   *  }}}
   */
  def importSelector(): Import.Selector = {
    import Import.{Selector => Sel}
    importWildcardOrName() match {
      case from: Sel.Name if in.token == ARROW =>
        in.nextToken()
        importWildcardOrName() match {
          case to: Sel.Name     => Sel.Rename(from.name, to.name)
          case to: Sel.Wildcard => Sel.Unimport(from.name)
          case _                => unreachable
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
      if (in.token != VAL) syntaxError(m, "lazy not allowed here. Only vals can be lazy")
    }
    in.token match {
      case VAL | VAR =>
        patDefOrDcl(mods)
      case DEF =>
        funDefOrDclOrCtor(mods)
      case TYPE =>
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
    val isMutable = in.token == VAR
    in.nextToken()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2())
    val tp: Option[Type] = typedOpt()

    if (tp.isEmpty || in.token == EQUALS) {
      accept(EQUALS)
      val rhs =
        if (in.token == USCORE && tp.nonEmpty && isMutable && lhs.forall(_.isInstanceOf[Term.Name])) {
          in.nextToken()
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
    in.nextToken()
    if (in.token != THIS) funDefRest(mods, termName())
    else {
      in.skipToken()
      val (paramss, implicits) = paramClauses(ownerIsType = true)
      newLineOptWhenFollowedBy(LBRACE)
      val (argss, stats) = in.token match {
        case LBRACE => constrBlock()
        case _      => accept(EQUALS); constrExpr()
      }
      Ctor.Secondary(mods, paramss, implicits, argss, stats)
    }
  }

  def funDefRest(mods: List[Mod], name: Term.Name): Member.Def = {
    def warnProcedureDeprecation =
      deprecationWarning(in.lastOffset, s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit`.")
    val tparams = typeParamClauseOpt(ownerIsType = false, ctxBoundsAllowed = true)
    val (paramss, implicits) = paramClauses(ownerIsType = false)
    newLineOptWhenFollowedBy(LBRACE)
    var restype = fromWithinReturnType(typedOpt())
    if (isStatSep || in.token == RBRACE) {
      if (restype.isEmpty) {
        warnProcedureDeprecation
        Decl.Procedure(mods, name, tparams, paramss, implicits)
      } else
        Decl.Def(mods, name, tparams, paramss, implicits, restype.get)
    } else if (restype.isEmpty && in.token == LBRACE) {
      warnProcedureDeprecation
      Defn.Procedure(mods, name, tparams, paramss, implicits, {
        accept(LBRACE)
        var r = blockStatSeq()
        accept(RBRACE)
        r
      })
    } else {
      var newmods = mods
      val rhs = {
        if (in.token == EQUALS) {
          in.nextTokenAllow("macro")
          if (isMacro) {
            in.nextToken()
            newmods = newmods :+ Mod.Macro()
          }
        } else {
          accept(EQUALS)
        }
        expr()
      }
      Defn.Def(newmods, name, tparams, paramss, implicits, restype, rhs)
    }
  }

  /** {{{
   *  ConstrExpr      ::=  SelfInvocation
   *                    |  ConstrBlock
   *  }}}
   */
  def constrExpr(): (List[List[Arg]], List[Stmt.Block]) =
    if (in.token == LBRACE) constrBlock()
    else (selfInvocation(), Nil)

  /** {{{
   *  SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
   *  }}}
   */
  def selfInvocation(): List[List[Arg]] = {
    accept(THIS)
    newLineOptWhenFollowedBy(LBRACE)
    var t: List[List[Arg]] = List(argumentExprs())
    newLineOptWhenFollowedBy(LBRACE)
    while (in.token == LPAREN || in.token == LBRACE) {
      t = t :+ argumentExprs()
      newLineOptWhenFollowedBy(LBRACE)
    }
    t
  }

  /** {{{
   *  ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
   *  }}}
   */
  def constrBlock(): (List[List[Arg]], List[Stmt.Block]) = {
    in.skipToken()
    val argss = selfInvocation()
    val stats =
      if (!isStatSep) Nil
      else { in.nextToken(); blockStatSeq() }
    accept(RBRACE)
    (argss, stats)
  }

  /** {{{
   *  TypeDef ::= type Id [TypeParamClause] `=' Type
   *            | FunSig `=' Expr
   *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
   *  }}}
   */
  def typeDefOrDcl(mods: List[Mod]): Member.AbstractOrAliasType = {
    in.nextToken()
    newLinesOpt()
    val name = typeName()
    val tparams = typeParamClauseOpt(ownerIsType = true, ctxBoundsAllowed = false)
    in.token match {
      case EQUALS =>
        in.nextToken()
        Defn.Type(mods, name, tparams, typ())
      case t if t == SUPERTYPE || t == SUBTYPE || t == COMMA || t == RBRACE || isStatSep(t) =>
        Decl.Type(mods, name, tparams, typeBounds())
      case _ =>
        syntaxError("`=', `>:', or `<:' expected")
    }
  }

  /** Hook for IDE, for top-level classes/objects. */
  def topLevelTmplDef: Member.Template with Stmt.TopLevel =
    tmplDef(annots(skipNewLines = true) ++ modifiers())

  /** {{{
   *  TmplDef ::= [case] class ClassDef
   *            |  [case] object ObjectDef
   *            |  [override] trait TraitDef
   *  }}}
   */
  def tmplDef(mods: List[Mod]): Member.Template with Stmt.Template = {
    mods.getAll[Mod.Lazy].foreach { syntaxError(_, "classes cannot be lazy") }
    in.token match {
      case TRAIT =>
        traitDef(mods)
      case CLASS =>
        classDef(mods)
      case CASECLASS =>
        classDef(mods :+ Mod.Case())
      case OBJECT =>
        objectDef(mods)
      case CASEOBJECT =>
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
    in.nextToken()
    // TODO:
    // if (ofCaseClass && in.token != LPAREN)
    //  syntaxError(in.lastOffset, "case classes without a parameter list are not allowed;\n"+
    //                             "use either case objects or case classes with an explicit `()' as a parameter list.")
    // TODO:
    // if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods.isImplicit)))) {
    //  in.token match {
    //    case LBRACKET   => syntaxError("no type parameters allowed here")
    //    case EOF        => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
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
    in.nextToken()
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
    in.nextToken()
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
      parents += (in.token match {
        case LPAREN => Aux.Parent(parentTpe, multipleArgumentExprs())
        case _      => Aux.Parent(parentTpe, Nil)
      })
    }
    readAppliedParent()
    while (in.token == WITH) { in.nextToken(); readAppliedParent() }
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
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      // @S: pre template body cannot stub like post body can!
      val (self, body) = templateBody(isPre = true)
      if (in.token == WITH && self.name.isEmpty && self.decltpe.isEmpty) {
        val edefs = body.map(ensureEarlyDef)
        in.nextToken()
        val parents = templateParents()
        val (self1, body1, hasBraces) = templateBodyOpt(parenMeansSyntaxError = false)
        (edefs, parents, self1, body1, hasBraces)
      } else {
        (Nil, Nil, self, body, false)
      }
    } else {
      val parents = templateParents()
      val (self, body, hasBraces) = templateBodyOpt(parenMeansSyntaxError = false)
      (Nil, parents, self, body, hasBraces)
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
    val (early, parents, self, body, hasExplicitBody) = (
      if (in.token == EXTENDS /* || in.token == SUBTYPE && mods.isTrait */) {
        in.nextToken()
        template()
      }
      else {
        newLineOptWhenFollowedBy(LBRACE)
        val (self, body, hasExplicitBody) = templateBodyOpt(parenMeansSyntaxError = owner.isTrait || owner.isTerm)
        (Nil, Nil, self, body, hasExplicitBody)
      }
    )
    Aux.Template(early, parents, self, body)(hasExplicitBody = hasExplicitBody)
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
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      val (self, stats) = templateBody(isPre = false)
      (self, stats, true)
    } else {
      if (in.token == LPAREN) {
        if (parenMeansSyntaxError) syntaxError("traits or objects may not have parameters")
        else abort("unexpected opening parenthesis")
      }
      (Aux.Self(None, None), Nil, false)
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

  def statSeq[T <: Tree](statpf: PartialFunction[Token, T],
                         errorMsg: String = "illegal start of definition"): List[T] = {
    val stats = new ListBuffer[T]
    while (!isStatSeqEnd) {
      if (statpf.isDefinedAt(in.token)) stats += statpf(in.token)
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
  def topStat: PartialFunction[Token, Stmt.TopLevel] = {
    case PACKAGE  =>
      in.skipToken()
      packageOrPackageObject()
    case IMPORT =>
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
    var self: Aux.Self = Aux.Self(None, None)
    var firstOpt: Option[Term] = None
    if (isExprIntro) {
      val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
      if (in.token == ARROW) {
        first match {
          case Term.Placeholder() =>
            self = Aux.Self(None, None)
          case name: Name =>
            self = Aux.Self(Some(name.toTermName), None)
          case Term.Ascribe(name: Name, tpt) =>
            self = Aux.Self(Some(name.toTermName), Some(tpt))
          case Term.Ascribe(Term.Placeholder(), tpt) =>
            self = Aux.Self(None, Some(tpt))
          case Term.Ascribe(tree @ Term.This(None), tpt) =>
            self = Aux.Self(None, Some(tpt))
          case _ =>
        }
        in.nextToken()
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
  def templateStat: PartialFunction[Token, Stmt.Template] = {
    case IMPORT =>
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
      if (in.token != RBRACE) acceptStatSep()
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
      if (in.token == IMPORT) {
        stats += importStmt()
        acceptStatSepOpt()
      }
      else if (isDefIntro || isLocalModifier || isAnnotation) {
        if (in.token == IMPLICIT) {
          in.skipToken()
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
        in.nextToken()
      }
      else {
        val addendum = if (isModifier) " (no modifiers allowed here)" else ""
        syntaxError("illegal start of statement" + addendum)
      }
    }
    stats.toList
  }


  def packageOrPackageObject(): Stmt.TopLevel =
    if (in.token == OBJECT) {
      in.nextToken()
      packageObject()
    } else {
      Pkg(qualId(), inBracesOrNil(topStatSeq()))(hasBraces = true)
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
      while (in.token == SEMI) in.nextToken()
      if (in.token == PACKAGE) {
        in.nextToken()
        if (in.token == OBJECT) {
          in.nextToken()
          ts += packageObject()
          if (in.token != EOF) {
            acceptStatSep()
            ts ++= topStatSeq()
          }
        } else {
          val qid = qualId()

          if (in.token == EOF) {
            refs += qid
          } else if (isStatSep) {
            in.nextToken()
            refs += qid
            val (nrefs, nstats) = packageStats()
            refs ++= nrefs
            ts ++= nstats
          } else {
            ts += inBraces(Pkg(qid, topStatSeq())(hasBraces = true))
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
      case init :+ last => Aux.CompUnit(init.foldLeft(Pkg(last, stats)(hasBraces = false)) { (acc, ref) => Pkg(ref, acc :: Nil)(hasBraces = false) } :: Nil)
    }
  }
}

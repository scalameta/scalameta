package cbc

import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import cbc.util.settings
import cbc.util.Chars.{isOperatorPart, isScalaLetter}
import cbc.Tokens._
import cbc.TokenInfo._
import scala.reflect.core.{Tree, Term, Pat, Type, Defn, Decl, Lit, Stmt,
                           Import, Aux, Ident, RichMods, Mod, Enum, Ctor,
                           Arg, Pkg, Symbol}
import scala.reflect.ClassTag

object ParserInfo {
  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )
  val unaryOps = Set("-", "+", "~", "!")
  def isUnaryOp(s: String): Boolean = unaryOps contains s
  implicit class IdentInfo(val id: Ident) extends AnyVal {
    import id._
    def isRightAssocOp: Boolean = value.last != ':'
    def isUnaryOp: Boolean = ParserInfo.isUnaryOp(value)
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
    def isVarPattern: Boolean = id match {
      case _: Term.Ident => !isBackquoted && value.head.isLower && value.head.isLetter
      case _             => false
    }
    def isInterpolationId: Boolean = ???
  }
}
import ParserInfo._

case class ParseAbort(msg: String) extends Exception(s"abort: $msg")
case class ParseSyntaxError(offset: Offset, msg: String) extends Exception(s"syntax error at $offset: $msg")
case class ParseIncompleteInputError(msg: String) extends Exception("incomplete input: $msg")

class SourceParser(val source: Source) extends Parser {
  def this(code: String) = this(StringSource(code))
  /** The parse starting point depends on whether the source file is self-contained:
   *  if not, the AST will be supplemented.
   */
  def parseStartRule = () => compilationUnit()

  lazy val in = { val s = new SourceFileScanner(source); s.init(); s }

  // warning don't stop parsing
  def warning(offset: Offset, msg: String): Unit = ???
  def deprecationWarning(offset: Offset, msg: String): Unit = ???

  // errors do
  def abort(msg: String): Nothing = throw ParseAbort(msg)
  def syntaxError(offset: Offset, msg: String): Nothing = throw ParseSyntaxError(offset, msg)
  def incompleteInputError(msg: String): Nothing = throw ParseIncompleteInputError(msg)

  /** the markup parser */
  // private[this] lazy val xmlp = new MarkupParser(this, preserveWS = true)
  // object symbXMLBuilder extends SymbolicXMLBuilder(this, preserveWS = true)
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
  def source: Source

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

  def isIdentExcept(except: String) = isIdent && in.name != except
  def isIdentOf(name: String)       = isIdent && in.name == name

  def isUnaryOp = isIdent && ParserInfo.isUnaryOp(in.name)
  def isRawStar = isIdent && in.name == "*"
  def isRawBar  = isIdent && in.name == "|"

  def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT
  def isMacro = in.token == IDENTIFIER && in.name == "macro"

  def isLiteralToken(token: Token) = token match {
    case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
         STRINGLIT | SYMBOLLIT | TRUE | FALSE | NULL => true
    case _                                                        => false
  }
  def isLiteral = isLiteralToken(in.token)

  def isExprIntroToken(token: Token): Boolean = isLiteralToken(token) || (token match {
    case IDENTIFIER | BACKQUOTED_IDENT |
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

  def isStatSeqEnd = in.token == RBRACE || in.token == EOF

  def isCaseDefEnd = in.token == RBRACE || in.token == CASE || in.token == EOF

  def isStatSep(token: Token): Boolean =
    token == NEWLINE || token == NEWLINES || token == SEMI

  def isStatSep: Boolean = isStatSep(in.token)


/* ---------- TREE CONSTRUCTION ------------------------------------------- */

  /** Convert tree to formal parameter list. */
  def convertToParams(tree: Term): List[Aux.Param] = tree match {
    case Term.Tuple(ts) => ts map convertToParam
    case _              => List(convertToParam(tree))
  }

  /** Convert tree to formal parameter. */
  def convertToParam(tree: Term): Aux.Param = tree match {
    case id: Ident =>
      Aux.Param(name = Some(id.toTermIdent), decltpe = None)
    case Term.Placeholder() =>
      Aux.Param.empty
    case Term.Ascribe(id: Ident, tpt) =>
      Aux.Param(name = Some(id.toTermIdent), decltpe = Some(tpt))
    case Term.Ascribe(Term.Placeholder(), tpt) =>
      Aux.Param(decltpe = Some(tpt))
    case _ =>
      syntaxError("not a legal formal parameter")
  }

  def convertToTypeId(ref: Term.Ref): Option[Type] = ref match {
    case Term.Select(qual: Term.Ref, name) =>
      Some(Type.Select(qual, name.toTypeIdent))
    case Term.SuperSelect(qual, supertpe, selector) =>
      Some(Type.SuperSelect(qual, supertpe, selector.toTypeIdent))
    case id: Term.Ident =>
      Some(id.toTypeIdent)
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

  def makeTuple[T <: Tree](bodyf: => List[T], zero: () => T, tuple: List[T] => T): T = {
    val body = inParens(if (in.token == RPAREN) Nil else bodyf)
    body match {
      case Nil         => zero()
      case only :: Nil => only
      case _           => tuple(body)
    }
  }

  def makeTupleTerm(bodyf: => List[Term]): Term =
    makeTuple[Term](bodyf, () => Lit.Unit(), Term.Tuple(_))

  def makeTupleType(bodyf: => List[Type]): Type =
    makeTuple[Type](bodyf, () => syntaxError(???), Type.Tuple(_))

  def makeTuplePat(bodyf: => List[Pat]): Pat =
    makeTuple[Pat](bodyf, () => Lit.Unit(), Pat.Tuple(_))

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */

  /** Modes for infix types. */
  object InfixMode extends Enumeration {
    val FirstOp, LeftOp, RightOp = Value
  }

  final case class OpInfo[T: OpCtx](lhs: T, operator: Term.Ident, targs: List[Type]) {
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
    implicit object `Term Context` extends OpCtx[Term] {
      def opinfo(tree: Term): OpInfo[Term] = {
        val id = ident()
        val targs = if (in.token == LBRACKET) exprTypeArgs() else Nil
        OpInfo(tree, id, targs)
      }
      def binop(opinfo: OpInfo[Term], rhs: Term): Term = {
        val arguments: List[Arg] = rhs match {
          case Term.Tuple(args) => args map assignmentToMaybeNamedArg
          case _                => List(rhs)
        }
        if (!opinfo.operator.isRightAssocOp) {
          Term.ApplyRight(opinfo.lhs, opinfo.operator, opinfo.targs, rhs)
        } else {
          val select = Term.Select(opinfo.lhs, opinfo.operator)
          val selectTargs = if (opinfo.targs.isEmpty) select else Term.ApplyType(select, opinfo.targs)
          Term.Apply(selectTargs, arguments)
        }
      }
    }
    implicit object `Pat Context` extends OpCtx[Pat] {
      def opinfo(tree: Pat): OpInfo[Pat] = {
        val id = ident()
        if (in.token == LBRACKET) syntaxError("infix patterns cannot have type arguments")
        OpInfo(tree, id, Nil)
      }
      def binop(opinfo: OpInfo[Pat], rhs: Pat): Pat = {
        val arguments = rhs match {
          case Pat.Tuple(args) => args
          case _               => List(rhs)
        }
        Pat.Extract(opinfo.operator, Nil, opinfo.lhs :: arguments)
      }
    }
  }

  def opctx[T](implicit ctx: OpCtx[T]) = ctx

  def checkHeadAssoc[T: OpCtx](leftAssoc: Boolean) = checkAssoc(opctx.head.operator, leftAssoc)

  def checkAssoc(op: Ident, leftAssoc: Boolean): Unit = (
    if (op.isRightAssocOp == leftAssoc)
      syntaxError("left- and right-associative operators with same precedence may not be mixed")
  )

  def finishPostfixOp(base: List[OpInfo[Term]], opinfo: OpInfo[Term]): Term =
    Term.Select(reduceStack(base, opinfo.lhs), opinfo.operator)

  def finishBinaryOp[T: OpCtx](opinfo: OpInfo[T], rhs: T): T = opctx.binop(opinfo, rhs)

  def reduceStack[T: OpCtx](base: List[OpInfo[T]], top: T): T = {
    val id           = Term.Ident(in.name)
    val opPrecedence = if (isIdent) id.precedence else 0
    val leftAssoc    = !isIdent || !id.isRightAssocOp

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
    def functionArgType(): Type

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
          val tuple = Type.Tuple(ts)
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
        case LPAREN   => Type.Tuple(inParens(types()))
        case USCORE   => in.nextToken(); wildcardType()
        case _        =>
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
      Type.Project(qual, typeIdent())
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

    // TODO: warn about def f: Unit { } case
    def compoundTypeRest(t: Option[Type]): Type = {
      val ts = new ListBuffer[Type] ++ t
      while (in.token == WITH) {
        in.nextToken()
        ts += annotType()
      }
      newLineOptWhenFollowedBy(LBRACE)
      val types         = ts.toList
      val refinements   = if (in.token == LBRACE) refinement() else Nil
      (types, refinements) match {
        case (typ :: Nil, Nil) => typ
        case _                 => Type.Compound(types, refinements)
      }
    }

    def infixTypeRest(t: Type, mode: InfixMode.Value): Type = {
      if (isIdent && in.name != "*") {
        val id = Term.Ident(in.name)
        val leftAssoc = !id.isRightAssocOp
        if (mode != InfixMode.FirstOp) checkAssoc(id, leftAssoc = mode == InfixMode.LeftOp)
        val op = typeIdent()
        newLineOptWhenFollowing(isTypeIntroToken)
        def mkOp(t1: Type) = Type.Apply(op, List(t, t1))
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
    def functionTypes(): List[Type] = commaSeparated(functionArgType())
  }

  // TODO: can we get away without this?
  implicit class IdentToIdent(id: Ident) {
    def toTypeIdent: Type.Ident = id match {
      case id: Type.Ident => id
      case _              => Type.Ident(id.value, id.isBackquoted)
    }
    def toTermIdent: Term.Ident = id match {
      case id: Term.Ident => id
      case _              => Term.Ident(id.value, id.isBackquoted)
    }
  }

  def ident(): Term.Ident =
    if (!isIdent) syntaxError(expectedMsg(IDENTIFIER))
    else {
      val name = in.name
      val isBackquoted = in.token == BACKQUOTED_IDENT
      in.nextToken()
      Term.Ident(name, isBackquoted)
    }

  /** For when it's known already to be a type name. */
  def typeIdent(): Type.Ident = ident().toTypeIdent

  /** {{{
   *  Path       ::= StableId
   *              |  [Ident `.'] this
   *  ModType ::= Path [`.' type]
   *  }}}
   */
  // TODO: foo.this can be either term or type name depending on the context
  // TODO: this has to be rewritten
  def path(thisOK: Boolean = true): Term.Ref =
    if (in.token == THIS) {
      in.nextToken()
      val thisnone = Term.This(None)
      if (!thisOK || in.token == DOT) {
        accept(DOT)
        selectors(thisnone)
      } else {
        thisnone
      }
    } else if (in.token == SUPER) {
      in.nextToken()
      val mixinQual = mixinQualifierOpt()
      accept(DOT)
      val supersel = Term.SuperSelect(None, mixinQual, ident())
      if (in.token == DOT) {
        in.skipToken()
        selectors(supersel)
      } else {
        supersel
      }
    } else {
      val id = ident()
      if (in.token != DOT || lookingAhead { in.token == TYPE }) id
      else {
        in.nextToken()
        if (in.token == THIS) {
          in.nextToken()
          val thisid = Term.This(Some(id.toTypeIdent))
          if (thisOK && in.token != DOT) thisid
          else {
            accept(DOT)
            selectors(thisid)
          }
        } else if (in.token == SUPER) {
          in.nextToken()
          val mixinQual = mixinQualifierOpt()
          accept(DOT)
          val supersel = Term.SuperSelect(Some(id.toTypeIdent), mixinQual, ident())
          if (in.token != DOT) supersel
          else {
            in.skipToken()
            selectors(supersel)
          }
        } else {
          selectors(id)
        }
      }
    }

  def selector(t: Term): Term.Select = Term.Select(t, ident())
  def selectors(t: Term.Ref): Term.Ref ={
    val t1 = selector(t)
    if (in.token == DOT) {
      in.nextToken()
      selectors(t1)
    }
    else t1
  }

  /** {{{
  *   MixinQualifier ::= `[' Id `]'
  *   }}}
  */
  def mixinQualifierOpt(): Option[Type.Ident] =
    if (in.token == LBRACKET) Some(inBrackets(typeIdent()))
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
  def qualId(): Term.Ref = ???
  /*{
    val start = in.offset
    val id = ident()
    if (in.token == DOT) {
      in.skipToken()
      selectors(id, typeOK = false)
    }
    else id
  }*/

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
      case TRUE                   => Lit.True()
      case FALSE                  => Lit.False()
      case NULL                   => Lit.Null()
      case SYMBOLLIT              => Lit.Symbol(scala.Symbol(in.strVal))
      case _                      => syntaxError("illegal literal")
    }
    in.nextToken()
    res
  }

  def interpolate[Ctx, Ret](arg: () => Ctx, result: (Term.Ident, List[Lit.String], List[Ctx]) => Ret): Ret = {
    def part() =
      if (in.token != STRINGPART && in.token != STRINGLIT) syntaxError(expectedMsg(STRINGPART))
      else {
        val lit = Lit.String(in.strVal.intern())
        in.nextToken()
        lit
      }
    val interpolator = Term.Ident(in.name) // ident() for INTERPOLATIONID
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
        case IDENTIFIER => ident()
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
      Lit.True()
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
      val elsep = if (in.token == ELSE) { in.nextToken(); Some(expr()) }
                  else None
      Term.If(cond, thenp, elsep)
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
      Term.Return(if (isExprIntro) expr() else Lit.Unit())
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
      val id = ident()
      if (in.token != COLON) id
      else {
        in.nextToken()
        Term.Ascribe(expr, typeOrInfixType(location))
      }
    }
    val param = param0.copy(mods = Mod.Implicit() :: param0.mods)
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
    val ctx  = opctx[Term]
    val base  = ctx.stack

    def loop(top: Term): Term =
      if (!isIdent) top
      else {
        ctx.push(reduceStack(base, top))
        newLineOptWhenFollowing(isExprIntroToken)
        if (isExprIntro) loop(prefixExpr())
        else finishPostfixOp(base, ctx.pop())
      }

    reduceStack(base, loop(prefixExpr()))
  }

  /** {{{
   *  PrefixExpr   ::= [`-' | `+' | `~' | `!' | `&'] SimpleExpr
   *  }}}
   */
  def prefixExpr(): Term =
    if (!isUnaryOp) simpleExpr()
    else {
      val op = ident()
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
          makeTupleTerm(commaSeparated(expr()))
        case LBRACE =>
          canApply = false
          blockExpr()
        case NEW =>
          canApply = false
          // TODO: right entry point for templates?
          Term.New(templateOpt())
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
          case _: Term.Ident | _: Term.Select | _: Term.Apply =>
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
    case Term.Assign(id: Term.Ident, rhs) => Arg.Named(id, rhs)
    case t                                => t
  }

  /** {{{
   *  ArgumentExprs ::= `(' [Exprs] `)'
   *                  | [nl] BlockExpr
   *  }}}
   */
  def argumentExprs(): List[Arg] = {
    def args(): List[Arg] = commaSeparated(
      if (isIdent) assignmentToMaybeNamedArg(expr()) else expr()
    )
    in.token match {
      case LBRACE   => List(blockExpr())
      case LPAREN   => inParens(if (in.token == RPAREN) Nil else args())
      case _        => Nil
    }
    // TODO: handle var args
    /*
        if (in.token == USCORE) {
          //todo: need to handle case where USCORE is a wildcard in a type
          val uscorePos = in.skipToken()
          if (isIdent && in.name == nme.STAR) {
            in.nextToken()
            t = Term.Ascribe(t, Ident(tpnme.WILDCARD_STAR))
          } else {
            syntaxError("`*' expected")
          }
    */
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

  /** {{{
   *  Block ::= BlockStatSeq
   *  }}}
   *  @note  Return tree does not carry position.
   */
  def block(): Term.Block = Term.Block(blockStatSeq())

  def caseClause(): Aux.Case =
    Aux.Case(pattern(), guard(), caseBlock())

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

  // IDE HOOK (so we can memoize case blocks) // needed?
  def caseBlock(): Option[Term] = {
    accept(ARROW)
    blockStatSeq() match {
      case Nil                 => None
      case (stat: Term) :: Nil => Some(stat)
      case stats               => Some(Term.Block(stats))
    }
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

    def functionArgType(): Type = {
      // TODO: why to do with annot here?
      val (t, annot) = paramType()
      t
    }
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
      case id: Term.Ident if in.token == COLON =>
        if (!id.isVarPattern)
          syntaxError("Pattern variables must start with a lower-case letter. (SLS 8.1.1.)")
        else {
          in.skipToken()
          Pat.Typed(id, compoundType())
        }
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
        case id: Term.Ident if id.isVarPattern =>
          in.nextToken()
          Pat.Bind(id, pattern3())
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
      // See SI-3189, SI-4832 for motivation. Cf SI-3480 for counter-motivation.
      def isCloseDelim = in.token match {
        case RBRACE => isXML
        case RPAREN => !isXML
        case _      => false
      }
      def checkWildStar: Option[Pat.SeqWildcard] = top match {
        case Pat.Wildcard() if isSequenceOK && isRawStar => peekingAhead (
          // TODO: used to be Start(top) | EmptyTree, why start had param?
          if (isCloseDelim) Some(Pat.SeqWildcard())
          else None
        )
        case _ => None
      }
      def loop(top: Pat): Pat = reduceStack(ctx.stack, top) match {
        case next if isIdentExcept("|") => ctx.push(next); loop(simplePattern(badPattern3))
        case next                       => next
      }
      checkWildStar getOrElse loop(top)
    }

    def badPattern3(): Nothing = {
      def isComma                = in.token == COMMA
      def isDelimiter            = in.token == RPAREN || in.token == RBRACE
      def isCommaOrDelimiter     = isComma || isDelimiter
      val (isUnderscore, isStar) = opctx[Pat].stack match {
        case OpInfo(Pat.Wildcard(), Term.Ident("*", _), _) :: _ => (true,   true)
        case OpInfo(_, Term.Ident("*", _), _) :: _              => (false,  true)
        case _                                                  => (false, false)
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
              case Term.Ident("-", _) =>
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
          case _                           => abort("unreachable")
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
        makeTuplePat(noSeq.patterns())
      case XMLSTART =>
        xmlLiteralPattern()
      case _ =>
        onError()
    }
  }
  /** The implementation of the context sensitive methods for parsing outside of patterns. */
  object outPattern extends PatternContextSensitive {
    def argType(): Type = typ()
    def functionArgType(): Type = {
      // TODO: what to do with annot here?
      val (t, annot) = paramType()
      t
    }
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

  private def addMod(mods: List[Mod], mod: Mod): List[Mod] = {
    if (mods exists (_ == mod)) syntaxError("repeated modifier")
    in.nextToken()
    mods :+ mod
  }
  private def addMod(mods: List[Mod], optmod: Option[Mod]): List[Mod] =
    optmod.map(mods :+ _).getOrElse(mods)

  /** {{{
   *  AccessQualifier ::= `[' (Id | this) `]'
   *  }}}
   */
  def accessQualifierOpt(): Option[Mod.AccessQualifier] = {
    if (in.token != LBRACKET) None
    else {
      in.nextToken()
      val res = if (in.token != THIS) typeIdent()
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
          if (mods exists { _.isInstanceOf[Mod.Access] })
            syntaxError("duplicate private/protected qualifier")
          loop(addMod(mods, accessModifierOpt()))
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
  def paramClauses[Owner <: Tree](): (List[List[Aux.Param]], List[Aux.Param]) = ???
  /*{
    var implicitmod = 0
    var caseParam = ofCaseClass
    def paramClause(): List[ValDef] = {
      if (in.token == RPAREN)
        return Nil

      if (in.token == IMPLICIT) {
        in.nextToken()
        implicitmod = Flags.IMPLICIT
      }
      commaSeparated(param(owner, implicitmod, caseParam  ))
    }
    val vds = new ListBuffer[List[ValDef]]
    val start = in.offset
    newLineOptWhenFollowedBy(LPAREN)
    if (ofCaseClass && in.token != LPAREN)
      syntaxError(in.lastOffset, "case classes without a parameter list are not allowed;\n"+
                                 "use either case objects or case classes with an explicit `()' as a parameter list.")
    while (implicitmod == 0 && in.token == LPAREN) {
      in.nextToken()
      vds += paramClause()
      accept(RPAREN)
      caseParam = false
      newLineOptWhenFollowedBy(LPAREN)
    }
    val result = vds.toList
    if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods.isImplicit)))) {
      in.token match {
        case LBRACKET   => syntaxError("no type parameters allowed here")
        case EOF        => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
        case _          => syntaxError(start, "auxiliary constructor needs non-implicit parameter list")
      }
    }
    addEvidenceParams(owner, result, contextBounds)
  }*/

  /** {{{
   *  ParamType ::= Type | `=>' Type | Type `*'
   *  }}}
   */
  def paramType(): (Type, Option[Mod]) = {
    in.token match {
      case ARROW  =>
        in.nextToken()
        (typ(), Some(Mod.ByNameParam()))
      case _      =>
        val t = typ()
        if (isRawStar) {
          in.nextToken()
          (t, Some(Mod.VarargParam()))
        }
        else (t, None)
    }
  }

  def param(owner: Ident, isImplicit: Boolean): Aux.Param = ???
  /* {
    val start = in.offset
    val annots = annots(skipNewLines = false)
    var mods = Modifiers(Flags.PARAM)
    if (owner.isTypeName) {
      mods = modifiers() | Flags.PARAMACCESSOR
      if (mods.isLazy) syntaxError("lazy modifier not allowed here. Use call-by-name parameters instead")
      in.token match {
        case v @ (VAL | VAR) =>
          if (v == VAR) mods |= Flags.MUTABLE
          in.nextToken()
        case _ =>
          if (mods.flags != Flags.PARAMACCESSOR) accept(VAL)
          if (!caseParam) mods |= Flags.PrivateLocal
      }
      if (caseParam) mods |= Flags.CASEACCESSOR
    }
    val nameOffset = in.offset
    val name = ident()
    var bynamemod = 0
    val tpt =
      if ((settings.YmethodInfer && !owner.isTypeName) && in.token != COLON) {
        TypeTree()
      } else { // XX-METHOD-INFER
        accept(COLON)
        if (in.token == ARROW) {
          if (owner.isTypeName && !mods.isLocalToThis)
            syntaxError(
              (if (mods.isMutable) "`var'" else "`val'") +
              " parameters may not be call-by-name")
          else if (implicitmod != 0)
            syntaxError("implicit parameters may not be call-by-name")
          else bynamemod = Flags.BYNAMEPARAM
        }
        paramType()
      }
    val default =
      if (in.token == EQUALS) {
        in.nextToken()
        mods |= Flags.DEFAULTPARAM
        expr()
      } else EmptyTree
    ValDef((mods | implicitmod.toLong | bynamemod) withAnnotations annots, name.toTermName, tpt, default)
  }*/

  /** {{{
   *  TypeParamClauseOpt    ::= [TypeParamClause]
   *  TypeParamClause       ::= `[' VariantTypeParam {`,' VariantTypeParam} `]']
   *  VariantTypeParam      ::= {Annotation} [`+' | `-'] TypeParam
   *  FunTypeParamClauseOpt ::= [FunTypeParamClause]
   *  FunTypeParamClause    ::= `[' TypeParam {`,' TypeParam} `]']
   *  TypeParam             ::= Id TypeParamClauseOpt TypeBounds {<% Type} {":" Type}
   *  }}}
   */
  def typeParamClauseOpt[Owner <: Symbol](): List[Aux.TypeParam] = ???
  /*{
    def typeParam(ms: List[Mod]): TypeDef = {
      var mods = ms | Flags.PARAM
      val start = in.offset
      if (owner.isTypeName && isIdent) {
        if (in.name == raw.PLUS) {
          in.nextToken()
          mods |= Flags.COVARIANT
        } else if (in.name == raw.MINUS) {
          in.nextToken()
          mods |= Flags.CONTRAVARIANT
        }
      }
      val nameOffset = in.offset
      val pname: TypeName = wildcardOrIdent().toTypeName
      val param = {
        val tparams = typeParamClauseOpt(pname, null) // @M TODO null --> no higher-order context bounds for now
        TypeDef(mods, pname, tparams, typeBounds())
      }
      if (contextBoundBuf ne null) {
        while (in.token == VIEWBOUND) {
          val msg = "Use an implicit parameter instead.\nExample: Instead of `def f[A <% Int](a: A)` use `def f[A](a: A)(implicit ev: A => Int)`."
          if (settings.future)
            deprecationWarning(s"View bounds are deprecated. $msg")
          in.skipToken()
          contextBoundBuf += Type.Function(List(Ident(pname)), typ())
        }
        while (in.token == COLON) {
          in.skipToken()
          contextBoundBuf += {
            AppliedTypeTree(typ(), List(Ident(pname)))
          }
        }
      }
      param
    }
    newLineOptWhenFollowedBy(LBRACKET)
    if (in.token == LBRACKET) inBrackets(commaSeparated(typeParam(NoMods withAnnotations annots(skipNewLines = true))))
    else Nil
  }*/

  /** {{{
   *  TypeBounds ::= [`>:' Type] [`<:' Type]
   *  }}}
   */
  def typeBounds(): Aux.TypeBounds = Aux.TypeBounds(bound(SUPERTYPE), bound(SUBTYPE))

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
    accept(DOT)
    Import.Clause(sid, importSelectors())
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
    else Import.Selector.Name(ident().value)

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
          case _                => abort("unreachable")
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
  def defOrDclOrCtor(mods: List[Mod]): Symbol = {
    if (mods.has[Mod.Lazy] && in.token != VAL)
      syntaxError("lazy not allowed here. Only vals can be lazy")
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

  def nonLocalDefOrDcl: Symbol with Stmt.Template = {
    val anns = annots(skipNewLines = true)
    defOrDclOrCtor(modifiers() ++ anns) match {
      case s: Stmt.Template => s
      case _                => syntaxError(???)
    }
  }

  /** {{{
   *  PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
   *  ValDcl ::= Id {`,' Id} `:' Type
   *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
   *  }}}
   */
  def patDefOrDcl(mods: List[Mod]): Symbol.Field = {
    val isMutable = in.token == VAR
    in.nextToken()
    val lhs: List[Pat] = commaSeparated(noSeq.pattern2())
    val tp: Option[Type] = typedOpt()

    if (tp.isEmpty || in.token == EQUALS) {
      accept(EQUALS)
      val rhs =
        if (in.token == USCORE && !tp.isEmpty && isMutable &&
            (lhs.toList forall (_.isInstanceOf[Term.Ident]))) {
          in.nextToken()
          None
        } else Some(expr())

      if (isMutable) Defn.Var(mods, lhs, tp, rhs)
      else Defn.Val(mods, lhs, tp, rhs.get)
    } else {
      if (mods.has[Mod.Lazy]) syntaxError("lazy values may not be abstract")
      val ids = lhs.map {
        case id: Term.Ident => id
        case _              => syntaxError("pattern definition may not be abstract")
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
  def funDefOrDclOrCtor(mods: List[Mod]): Symbol with Stmt.Template = {
    in.nextToken()
    if (in.token != THIS) funDefRest(mods, ident())
    else {
      in.skipToken()
      val (paramss, implicits) = paramClauses[Ctor]()
      newLineOptWhenFollowedBy(LBRACE)
      val (argss, stats) = in.token match {
        case LBRACE => constrBlock()
        case _      => accept(EQUALS); constrExpr()
      }
      Ctor.Secondary(mods, paramss, implicits, argss, stats)
    }
  }

  def funDefRest(mods: List[Mod], name: Term.Ident): Symbol.Def = {
    def warnProcedureDeprecation =
      deprecationWarning(in.lastOffset, s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit`.")
    val tparams = typeParamClauseOpt()
    val (paramss, implicits) = paramClauses()
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
      Defn.Procedure(mods, name, tparams, paramss, implicits, block())
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
  def typeDefOrDcl(mods: List[Mod]): Symbol.Type = {
    in.nextToken()
    newLinesOpt()
    val name = typeIdent()
    val tparams = typeParamClauseOpt()
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
  def topLevelTmplDef: Symbol.Template with Stmt.TopLevel =
    tmplDef(annots(skipNewLines = true) ++ modifiers())

  /** {{{
   *  TmplDef ::= [case] class ClassDef
   *            |  [case] object ObjectDef
   *            |  [override] trait TraitDef
   *  }}}
   */
  def tmplDef(mods: List[Mod]): Symbol.Template = {
    if (mods.has[Mod.Lazy]) syntaxError("classes cannot be lazy")
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
        syntaxError("expected start of definition")
    }
  }

  /** {{{
   *  ClassDef ::= Id [TypeParamClause] {Annotation}
   *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
   *  }}}
   */
  def classDef(mods: List[Mod]): Defn.Class = {
    in.nextToken()
    Defn.Class(mods, typeIdent(), typeParamClauseOpt(), primaryCtor(), templateOpt[Defn.Class]())
  }

  def primaryCtor(): Ctor.Primary = {
    val mods = constructorAnnots() ++ accessModifierOpt()
    val (paramss, implicits) = paramClauses[Ctor.Primary]()
    Ctor.Primary(mods, paramss, implicits)
  }

  /** {{{
   *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
   *  }}}
   */
  def traitDef(mods: List[Mod]): Defn.Trait = {
    in.nextToken()
    Defn.Trait(mods, typeIdent(), typeParamClauseOpt(), templateOpt[Defn.Trait]())
  }

  /** {{{
   *  ObjectDef       ::= Id ClassTemplateOpt
   *  }}}
   */
  def objectDef(mods: List[Mod]): Defn.Object = {
    in.nextToken()
    Defn.Object(mods, ident(), templateOpt[Defn.Object]())
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
  def template(): (List[Defn.Val], List[Aux.Parent], Aux.Self, List[Stmt.Template]) = {
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      // @S: pre template body cannot stub like post body can!
      val (self, body) = templateBody(isPre = true)
      if (in.token == WITH && (self eq Aux.Self.empty)) {
        val edefs: List[Defn.Val] = earlyDefs()
        in.nextToken()
        val parents = templateParents()
        val (self1, body1) = templateBodyOpt(parenMeansSyntaxError = false)
        (edefs, parents, self1, body1)
      } else {
        (Nil, Nil, self, body)
      }
    } else {
      val parents = templateParents()
      val (self, body) = templateBodyOpt(parenMeansSyntaxError = false)
      (Nil, parents, self, body)
    }
  }

  def earlyDefs(): List[Defn.Val] = ???

  /** {{{
   *  ClassTemplateOpt ::= `extends' ClassTemplate | [[`extends'] TemplateBody]
   *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[`extends'] TemplateBody] | `<:' TemplateBody
   *  TraitExtends     ::= `extends' | `<:'
   *  }}}
   */
  def templateOpt[Owner <: Stmt.Template](): Aux.Template = {
    val (early, parents, self, body): (List[Defn.Val], List[Aux.Parent], Aux.Self, List[Stmt.Template]) = (
      if (in.token == EXTENDS /* || in.token == SUBTYPE && mods.isTrait */) {
        in.nextToken()
        template()
      }
      else {
        newLineOptWhenFollowedBy(LBRACE)
        val (self, body) = templateBodyOpt(parenMeansSyntaxError = ??? /* mods.isTrait || name.isTermName */)
        // TODO: validate that nils here corerspond to the source code
        (Nil, Nil, self, body)
      }
    )

    Aux.Template(early, parents, self, body)
  }

/* -------- TEMPLATES ------------------------------------------- */

  /** {{{
   *  TemplateBody ::= [nl] `{' TemplateStatSeq `}'
   *  }}}
   * @param isPre specifies whether in early initializer (true) or not (false)
   */
  def templateBody(isPre: Boolean): (Aux.Self, List[Stmt.Template]) =
    inBraces(templateStatSeq(isPre = isPre))

  def templateBodyOpt(parenMeansSyntaxError: Boolean): (Aux.Self, List[Stmt.Template]) = {
    newLineOptWhenFollowedBy(LBRACE)
    if (in.token == LBRACE) {
      val (self, stats) = templateBody(isPre = false)
      (self, stats)
    } else {
      if (in.token == LPAREN) {
        if (parenMeansSyntaxError) syntaxError(s"traits or objects may not have parameters")
        else abort("unexpected opening parenthesis")
      }
      (Aux.Self.empty, Nil)
    }
  }

  /** {{{
   *  Refinement ::= [nl] `{' RefineStat {semi RefineStat} `}'
   *  }}}
   */
  def refinement(): List[Stmt.Refine] = inBraces(refineStatSeq())

  def existentialStats(): List[Stmt.Existential] = ???
  /* used to be:
  refinement() flatMap {
        case t @ TypeDef(_, _, _, TypeBoundsTree(_, _)) => Some(t)
        case t @ ValDef(_, _, _, EmptyTree) => Some(t)
        case EmptyTree => None
        case _ => syntaxError(/* t.pos */ ???, "not a legal existential clause"); None
      }
  */

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
    var self: Aux.Self = Aux.Self.empty
    var firstOpt: Option[Term] = None
    if (isExprIntro) {
      val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
      if (in.token == ARROW) {
        first match {
          case Term.Ascribe(tree @ Term.This(None), tpt) =>
            self = Aux.Self(None, Some(tpt))
          case _ =>
            convertToParam(first) match {
              case param @ Aux.Param(_, name, tpt, _) =>
                self = Aux.Self(name, tpt)
              case _ =>
            }
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
        case _               => syntaxError(???)
      }
    } else if (!isStatSep) {
      syntaxError(
        "illegal start of declaration"+
        (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
         else ""))
    } else None

  def localDef(implicitMod: Option[Mod.Implicit]): Stmt.Block = {
    val mods = (implicitMod ++: annots(skipNewLines = true)) ++ localModifiers()
    if (mods.has[Mod.Implicit] || mods.has[Mod.Lazy]) {
      defOrDclOrCtor(mods) match {
        case sb: Stmt.Block => sb
        case _              => syntaxError(???)
      }
    } else tmplDef(mods)
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
          if (isIdent) stats += implicitClosure(InBlock)
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


  def packageOrPackageObject(): Pkg with Stmt.TopLevel =
    if (in.token == OBJECT)
      packageObject()
    else {
      Pkg.Named(qualId(), inBracesOrNil(topStatSeq()))
    }

  def packageObject(): Pkg.Object = ???
  // makePackageObject(objectDef(NoMods))

  /** {{{
   *  CompilationUnit ::= {package QualId semi} TopStatSeq
   *  }}}
   */
  def compilationUnit(): Pkg = {
    def packageStats(): List[Stmt.TopLevel] = {
      val ts = new ListBuffer[Stmt.TopLevel]
      while (in.token == SEMI) in.nextToken()
      if (in.token == PACKAGE) {
        in.nextToken()
        if (in.token == OBJECT) {
          ts += packageObject()
          if (in.token != EOF) {
            acceptStatSep()
            ts ++= topStatSeq()
          }
        } else {
          val qid = qualId()

          if (in.token == EOF) {
            ts += Pkg.Named(qid, Nil)
          } else if (isStatSep) {
            in.nextToken()
            ts += Pkg.Named(qid, packageStats())
          } else {
            ts += inBraces(Pkg.Named(qid, topStatSeq()))
            acceptStatSepOpt()
            ts ++= topStatSeq()
          }
        }
      } else {
        ts ++= topStatSeq()
      }
      ts.toList
    }

    packageStats() match {
      case (stat: Pkg) :: Nil => stat
      case stats                  => Pkg.Empty(stats)
    }
  }
}

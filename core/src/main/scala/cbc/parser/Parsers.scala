package cbc.parser

import scala.collection.{ mutable, immutable }
import mutable.{ ListBuffer, StringBuilder }
import cbc.{ settings, ModifierFlags => Flags }
import cbc.util.SourceFile
import cbc.util.Chars.isScalaLetter
import cbc.parser.Tokens._
import cbc._, Trees._, Constants._, Names._
import scala.reflect.core.{Term, Pat, Type, Defn, Decl, Lit, Stmt,
                           Import, Aux, Ident, Annot, Enum, Ctor,
                           TypeParam, Param, Arg, Package, Symbol}
import scala.reflect.ClassTag

trait ParsersCommon extends ScannersCommon { self =>
  /** This is now an abstract class, only to work around the optimizer:
   *  methods in traits are never inlined.
   */
  abstract class ParserCommon {
    val in: ScannerCommon
    def deprecationWarning(off: Offset, msg: String): Unit
    def accept(token: Token): Int

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
  }
}

trait Parsers extends Scanners with MarkupParsers with ParsersCommon { self =>
  class SourceFileParser(val source: SourceFile) extends Parser {
    /** The parse starting point depends on whether the source file is self-contained:
     *  if not, the AST will be supplemented.
     */
    def parseStartRule = () => compilationUnit()

    lazy val in = { val s = new SourceFileScanner(source); s.init(); s }

    // warning don't stop parsing
    def warning(offset: Offset, msg: String): Unit = ???
    def deprecationWarning(offset: Offset, msg: String): Unit = ???

    // errors do
    def abort(msg: String): Nothing = ???
    def syntaxError(offset: Offset, msg: String): Nothing = ???
    def incompleteInputError(msg: String): Nothing = ???

    /** the markup parser */
    private[this] lazy val xmlp = new MarkupParser(this, preserveWS = true)
    object symbXMLBuilder extends SymbolicXMLBuilder(this, preserveWS = true)
    def xmlLiteral() : Tree = xmlp.xLiteral
    def xmlLiteralPattern() : Tree = xmlp.xLiteralPattern
  }

  type Location = Int
  final val Local: Location = 0
  final val InBlock: Location = 1
  final val InTemplate: Location = 2

  // These symbols may not yet be loaded (e.g. in the ide) so don't go
  // through definitions to obtain the names.
  lazy val ScalaValueClassNames = Seq(tpnme.AnyVal,
      tpnme.Unit,
      tpnme.Boolean,
      tpnme.Byte,
      tpnme.Short,
      tpnme.Char,
      tpnme.Int,
      tpnme.Long,
      tpnme.Float,
      tpnme.Double)

  import nme.raw

  abstract class Parser extends ParserCommon { parser =>
    val in: Scanner
    def source: SourceFile

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

    class ParserTreeBuilder extends TreeBuilder {
      def source = parser.source
    }
    val treeBuilder = new ParserTreeBuilder
    import treeBuilder.{source => _,  _}

    /** The types of the context bounds of type parameters of the surrounding class
     */
    private var classContextBounds: List[Tree] = Nil
    @inline private def savingClassContextBounds[T](op: => T): T = {
      val saved = classContextBounds
      try op
      finally classContextBounds = saved
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
    def abort(msg: String): Nothing
    def incompleteInputError(msg: String): Nothing
    def syntaxError(offset: Offset, msg: String): Nothing

    def syntaxError(msg: String): Nothing = syntaxError(in.offset, msg)
    def warning(msg: String): Unit = warning(in.offset, msg)

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

    def isIdentExcept(except: Name) = isIdent && in.name != except
    def isIdentOf(name: Name)       = isIdent && in.name == name

    def isUnaryOp = isIdent && ident(peek = true).isUnaryOp
    def isRawStar = isIdent && in.name == raw.STAR
    def isRawBar  = isIdent && in.name == raw.BAR

    def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT
    def isMacro = in.token == IDENTIFIER && in.name == nme.MACROkw

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
    def convertToParams(tree: Term): List[Param.Function] = tree match {
      case Term.Tuple(ts) => ts map convertToParam
      case _              => List(convertToParam(tree))
    }

    /** Convert tree to formal parameter. */
    def convertToParam(tree: Term): Param.Function = tree match {
      case id: Ident =>
        Param.Function(name = Some(id.toTermIdent), typ = None)
      case Term.Ascribe(id: Ident, tpt) =>
        Param.Function(name = Some(id.toTermIdent), typ = Some(tpt))
      case _ =>
        syntaxError(/* tree.pos */ ???, "not a legal formal parameter")
    }

    /** Convert (qual)ident to type identifier. */
    /*def convertToTypeId(tree: Tree): Tree = {
      convertToTypeName(tree) getOrElse {
        syntaxError(/* tree.pos */ ???, "identifier expected")
        errorTypeTree()
      }
    }*/

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

    def readUserAnnots(part: => Annot.UserDefined): List[Annot.UserDefined] =
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

    sealed class OpCtx[T <: Tree] {
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
          if (opinfo.operator.isRightAssocOp) {
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
        syntaxError(???, "left- and right-associative operators with same precedence may not be mixed")
    )

    def finishPostfixOp(base: List[OpInfo[Term]], opinfo: OpInfo[Term]): Term =
      Term.Select(reduceStack(base, opinfo.lhs), opinfo.operator)

    def finishBinaryOp[T: OpCtx](opinfo: OpInfo[T], rhs: T): T = opctx.binop(opinfo, rhs)

    def reduceStack[T: OpCtx](base: List[OpInfo[T]], top: T): T = {
      val id           = Term.Ident(in.name.toString)
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
       *  AnnotType        ::=  SimpleType {Annotation}
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
          case USCORE   => wildcardType()
          case _        =>
            /*
            path(thisOK = false, typeOK = true) match {
              case r @ SingletonTypeTree(_) => r
              case r => convertToTypeId(r)
            }
            */
            ???
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
       *  CompoundType ::= AnnotType {with AnnotType} [Refinement]
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
        Type.Compound(ts.toList, refinements)
      }

      def infixTypeRest(t: Type, mode: InfixMode.Value): Type = {
        if (isIdent && in.name != nme.STAR) {
          val id = Term.Ident(in.name.toString)
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

    def ident(peek: Boolean = false): Term.Ident =
      if (!isIdent) syntaxError(expectedMsg(IDENTIFIER))
      else {
        val name = in.name.toString
        val isBackquoted = in.token == BACKQUOTED_IDENT
        if (!peek) in.nextToken()
        Term.Ident(name, isBackquoted)
      }

    /** For when it's known already to be a type name. */
    def typeIdent(peek: Boolean = false): Type.Ident = ident(peek).toTypeIdent

    def selector(t: Term): Term.Select = Term.Select(t, ident())

    /** {{{
     *  Path       ::= StableId
     *              |  [Ident `.'] this
     *  AnnotType ::= Path [`.' type]
     *  }}}
     */
    // TODO: foo.this can be either term or type name depending on the context
    // TODO: this has to be rewritten
    /*def path(thisOK: Boolean, typeOK: Boolean): Tree =
      if (in.token == THIS) {
        in.nextToken()
        val thisnone = Term.This(None)
        if (!thisOK || in.token == DOT) {
          accept(DOT)
          selectors(thisnone, typeOK)
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
          selectors(supersel, typeOK)
        } else {
          supersel
        }
      } else {
        val id = ident()
        if (in.token != DOT) id
        else {
          if (in.token == THIS) {
            in.nextToken()
            val thisid = Term.This(Some(id.toTypeIdent))
            if (thisOK && in.token != DOT) thisid
            else {
              accept(DOT)
              selectors(thisid, typeOK)
            }
          } else if (in.token == SUPER) {
            in.nextToken()
            val mixinQual = mixinQualifierOpt()
            accept(DOT)
            val supersel = Term.SuperSelect(Some(id.toTypeIdent), mixinQual, ident())
            if (in.token != DOT) supersel
            else {
              in.skipToken()
              selectors(supersel, typeOK)
            }
          } else {
            selectors(id, typeOK)
          }
        }
      }*/

    /*def selectors(t: Term.Ref, typeOK: Boolean): Tree =
      if (typeOK && in.token == TYPE) {
        in.nextToken()
        SingletonTypeTree(t)
      }
      else {
        val t1 = selector(t)
        if (in.token == DOT) { selectors(t1, typeOK) }
        else t1
      }*/

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
    def stableId(): Term.Ref = ???
    //  path(thisOK = false, typeOK = false)

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
        if (in.token != STRINGLIT) syntaxError(expectedMsg(STRINGLIT))
        else {
          val lit = Lit.String(in.strVal.intern())
          in.nextToken()
          lit
        }
      val interpolator = Term.Ident(in.name.toString) // ident() for INTERPOLATIONID
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
      (t /: userAnnots(skipNewLines = false)) { case (t, annot) => Type.Annotate(t, annot :: Nil) }

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
        val elsep = if (in.token == ELSE) { in.nextToken(); expr() }
                    else Lit.Unit()
        Term.If(cond, thenp, elsep)
      case TRY =>
        in.skipToken()
        val body: Term = in.token match {
          case LBRACE => inBracesOrUnit(block())
          case LPAREN => inParensOrUnit(expr())
          case _      => expr()
        }
        val catchp: Option[Aux.Catch] =
          if (in.token != CATCH) None
          else {
            in.nextToken()
            Some {
              if (in.token != LBRACE) expr()
              else if (in.token == CASE) Aux.Cases(inBracesOrNil(caseClauses()))
              else inBracesOrNil(List(expr())).head
            }
          }
        val finallyp: Option[Term] = in.token match {
          case FINALLY => in.nextToken(); Some(expr())
          case _       => None
        }
        Term.Try(body, catchp, finallyp)
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
            t = Term.Annotate(t, userAnnots(skipNewLines = false))
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
          t = Term.Match(t, Aux.Cases(inBracesOrNil(caseClauses())))
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
      val param = param0.copy(annots = param0.annots :+ Annot.Implicit())
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

    def xmlLiteral(): Tree

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
          // TODO: add support for xml
          // case XMLSTART =>
          //  xmlLiteral()
          // TODO: term path
          // case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          //  path(thisOK = true, typeOK = false)
          case USCORE =>
            Term.Placeholder()
          case LPAREN =>
            makeTupleTerm(commaSeparated(expr()))
          case LBRACE =>
            canApply = false
            blockExpr()
          case NEW =>
            canApply = false
            Term.New(template())
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
        if (in.token == CASE) Term.PartialFunction(Aux.Cases(caseClauses()))
        else block()
      }
    }

    /** {{{
     *  Block ::= BlockStatSeq
     *  }}}
     *  @note  Return tree does not carry position.
     */
    def block(): Term = Term.Block(blockStatSeq())

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
    def caseBlock(): Term = {
      accept(ARROW)
      block()
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
        if (hasEq) deprecationWarning(in.offset, "val keyword in for comprehension is deprecated")
        else syntaxError(in.offset, "val in for comprehension must be followed by assignment")
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
            syntaxError(in.offset, "Pattern variables must start with a lower-case letter. (SLS 8.1.1.)")
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
          case next if isIdentExcept(raw.BAR) => ctx.push(next); loop(simplePattern(badPattern3))
          case next                           => next
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
      def simplePattern(onError: () => Nothing): Pat = {
        in.token match {
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
            in.token match {
              case LPAREN => Pat.Extract(sid, targs, argumentPatterns())
              case _      => syntaxError("pattern must be a value")
            }
          case USCORE =>
            in.nextToken()
            Pat.Wildcard()
          case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT |
               STRINGLIT | INTERPOLATIONID | SYMBOLLIT | TRUE | FALSE | NULL =>
            literal()
          case LPAREN =>
            makeTuplePat(noSeq.patterns())
          // TODO: xml support
          // case XMLSTART =>
          //   xmlLiteralPattern()
          case _ =>
            onError()
        }
      }
    }
    /** The implementation of the context sensitive methods for parsing outside of patterns. */
    object outPattern extends PatternContextSensitive {
      def argType(): Tree = typ()
      def functionArgType(): Tree = {
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
    def startAnnotType() = outPattern.annotType()
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

    /** Drop `private` modifier when followed by a qualifier.
     *  Contract `abstract` and `override` to ABSOVERRIDE
     */
    /*private def normalizeModifers(mods: List[Annot]): List[Annot] =
      if (mods.isPrivate && mods.hasAccessBoundary)
        normalizeModifers(mods &~ Flags.PRIVATE)
      else if (mods hasAllFlags (Flags.ABSTRACT | Flags.OVERRIDE))
        normalizeModifers(mods &~ (Flags.ABSTRACT | Flags.OVERRIDE) | Flags.ABSOVERRIDE)
      else
        mods

    private def addMod(mods: List[Annot], mod: Long): List[Annot] = {
      if (mods hasFlag mod) syntaxError(in.offset, "repeated modifier")
      in.nextToken()
      (mods | mod)
    }*/

    /** {{{
     *  AccessQualifier ::= `[' (Id | this) `]'
     *  }}}
     */
    /*def accessQualifierOpt(mods: List[Annot]): List[Annot] = {
      var result = mods
      if (in.token == LBRACKET) {
        in.nextToken()
        if (mods.hasAccessBoundary)
          syntaxError("duplicate private/protected qualifier")
        result = if (in.token == THIS) { in.nextToken(); mods | Flags.LOCAL }
                 else Modifiers(mods.flags, identForType())
        accept(RBRACKET)
      }
      result
    }*/

    private val flagAnnots: Map[Int, () => Annot] = Map(
      ABSTRACT  -> (() => Annot.Abstract()),
      FINAL     -> (() => Annot.Final()),
      IMPLICIT  -> (() => Annot.Implicit()),
      LAZY      -> (() => Annot.Lazy()),
      OVERRIDE  -> (() => Annot.Override()),
      SEALED    -> (() => Annot.Sealed())
    )

    def modifiers(): List[Annot] = ???
    def localModifiers(): List[Annot] = ???

    /** {{{
     *  AccessModifier ::= (private | protected) [AccessQualifier]
     *  }}}
     */
    /*def accessModifierOpt(): List[Annot] = normalizeModifers {
      in.token match {
        case m @ (PRIVATE | PROTECTED)  => in.nextToken() ; accessQualifierOpt(Modifiers(flagTokens(m)))
        case _                          => NoMods
      }
    }*/

    /** {{{
     *  Modifiers ::= {Modifier}
     *  Modifier  ::= LocalModifier
     *              |  AccessModifier
     *              |  override
     *  }}}
     */
    /*def modifiers(): List[Annot] = normalizeModifers {
      def loop(mods: List[Annot]): List[Annot] = in.token match {
        case PRIVATE | PROTECTED =>
          loop(accessQualifierOpt(addMod(mods, flagTokens(in.token))))
        case ABSTRACT | FINAL | SEALED | OVERRIDE | IMPLICIT | LAZY =>
          loop(addMod(mods, flagTokens(in.token)))
        case NEWLINE =>
          in.nextToken()
          loop(mods)
        case _ =>
          mods
      }
      loop(NoMods)
    }*/


    /** {{{
     *  LocalModifiers ::= {LocalModifier}
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
     *  }}}
     */
    /*def localModifiers(): List[Annot] = {
      def loop(mods: List[Annot]): List[Annot] =
        if (isLocalModifier) loop(addMod(mods, flagTokens(in.token)))
        else mods

      loop(NoMods)
    }*/

    /** {{{
     *  Annotations      ::= {`@' SimpleType {ArgumentExprs}}
     *  ConsrAnnotations ::= {`@' SimpleType ArgumentExprs}
     *  }}}
     */
    def userAnnots(skipNewLines: Boolean): List[Annot.UserDefined] = readUserAnnots {
      val t = userAnnot()
      if (skipNewLines) newLineOpt()
      t
    }
    def userConstructorAnnots(): List[Annot.UserDefined] = readUserAnnots {
      Annot.UserDefined(exprSimpleType(), argumentExprs() :: Nil)
    }

    def userAnnot(): Annot.UserDefined = {
      val t = exprSimpleType()
      if (in.token == LPAREN) Annot.UserDefined(t, multipleArgumentExprs())
      else Annot.UserDefined(t, Nil)
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
    def paramClauses[Owner <: Tree](contextBounds: List[Tree]): (List[List[Param.Def]], List[Param.Def]) = ???
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
          case LBRACKET   => syntaxError(in.offset, "no type parameters allowed here")
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
    def paramType(): (Type, Option[Annot.Param]) = {
      val start = in.offset
      in.token match {
        case ARROW  =>
          in.nextToken()
          (typ(), Some(Annot.ByName()))
        case _      =>
          val t = typ()
          if (isRawStar) {
            in.nextToken()
            (t, Some(Annot.Vararg()))
          }
          else (t, None)
      }
    }

    def param(owner: Ident, isImplicit: Boolean): Param.Def = ???
    /* {
      val start = in.offset
      val annots = userAnnots(skipNewLines = false)
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
                in.offset,
                (if (mods.isMutable) "`var'" else "`val'") +
                " parameters may not be call-by-name")
            else if (implicitmod != 0)
              syntaxError(
                in.offset,
                "implicit parameters may not be call-by-name")
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
    /*def typeParamClauseOpt(owner: Name, contextBoundBuf: ListBuffer[Tree]): List[TypeDef] = {
      def typeParam(ms: List[Annot]): TypeDef = {
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
              deprecationWarning(in.offset, s"View bounds are deprecated. $msg")
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
      if (in.token == LBRACKET) inBrackets(commaSeparated(typeParam(NoMods withAnnotations userAnnots(skipNewLines = true))))
      else Nil
    }*/

    def typeTypeParams(): List[TypeParam.Type] = ???
    def defTypeParams(): List[TypeParam.Def] = ???

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
          }
        case other => other
      }
    }

    implicit class ModsMethods(mods: List[Annot]) {
      def has[T](implicit tag: ClassTag[T]): Boolean =
        mods.exists { _.getClass == tag.runtimeClass }
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
    def defOrDcl(mods: List[Annot]): Symbol = {
      if (mods.has[Annot.Lazy] && in.token != VAL)
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

    def nonLocalDefOrDcl: Symbol with Stmt.TopLevel = {
      val annots = userAnnots(skipNewLines = true)
      defOrDcl(modifiers() ++ annots) match {
        case s: Stmt.TopLevel => s
        case _                => syntaxError(???)
      }
    }

    /** {{{
     *  PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  ValDcl ::= Id {`,' Id} `:' Type
     *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
     *  }}}
     */
    def patDefOrDcl(mods: List[Annot]): Symbol.Field = {
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
        if (mods.has[Annot.Lazy]) syntaxError("lazy values may not be abstract")
        val ids = lhs.map {
          case id: Term.Ident => id
          case _              => syntaxError("pattern definition may not be abstract")
        }

        if (isMutable) Decl.Var(mods, ids, tp.get)
        else Decl.Var(mods, ids, tp.get)
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
    def funDefOrDclOrCtor(mods: List[Annot]): Symbol with Stmt.Template = {
      in.nextToken()
      if (in.token == THIS) {
        in.skipToken()
        val (vparamss, implparams) = paramClauses[Ctor](classContextBounds)
        newLineOptWhenFollowedBy(LBRACE)
        val (argss, stats) = in.token match {
          case LBRACE => constrBlock()
          case _      => accept(EQUALS); constrExpr()
        }
        Ctor.Secondary(annots = mods, paramss = vparamss,
                       implicits = implparams, primaryCtorArgss = argss,
                       stats = stats)
      }
      else {
        val nameOffset = in.offset
        val name = identOrMacro()
        funDefRest(mods, name)
      }
    }

    def funDefRest(mods: List[Annot], name: Name): Symbol.Def = {
      val result = {
        var newmods = mods
        // contextBoundBuf is for context bounded type parameters of the form
        // [T : B] or [T : => B]; it contains the equivalent implicit parameter type,
        // i.e. (B[T] or T => B)
        val contextBoundBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, contextBoundBuf)
        val vparamss = paramClauses(name, contextBoundBuf.toList, ofCaseClass = false)
        newLineOptWhenFollowedBy(LBRACE)
        var restype = fromWithinReturnType(typedOpt())
        val rhs =
          if (isStatSep || in.token == RBRACE) {
            if (restype.isEmpty) {
              if (settings.future)
                deprecationWarning(in.lastOffset, s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit`.")
              restype = scalaUnitConstr
            }
            newmods |= Flags.DEFERRED
            EmptyTree
          } else if (restype.isEmpty && in.token == LBRACE) {
            if (settings.future)
              deprecationWarning(in.offset, s"Procedure syntax is deprecated. Convert procedure `$name` to method by adding `: Unit =`.")
            restype = scalaUnitConstr
            blockExpr()
          } else {
            if (in.token == EQUALS) {
              in.nextTokenAllow(nme.MACROkw)
              if (isMacro) {
                in.nextToken()
                newmods |= Flags.MACRO
              }
            } else {
              accept(EQUALS)
            }
            expr()
          }
        DefDef(newmods, name.toTermName, tparams, vparamss, restype, rhs)
      }
      result
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
    def typeDefOrDcl(mods: List[Annot]): Symbol.Type = {
      in.nextToken()
      newLinesOpt()
      val name = identForType()
      // @M! a type alias as well as an abstract type may declare type parameters
      val tparams = typeParamClauseOpt(name, null)
      in.token match {
        case EQUALS =>
          in.nextToken()
          TypeDef(mods, name, tparams, typ())
        case t if t == SUPERTYPE || t == SUBTYPE || t == COMMA || t == RBRACE || isStatSep(t) =>
          TypeDef(mods | Flags.DEFERRED, name, tparams, typeBounds())
        case _ =>
          syntaxError("`=', `>:', or `<:' expected")
      }
    }

    /** Hook for IDE, for top-level classes/objects. */
    def topLevelTmplDef: Symbol.Template with Stmt.TopLevel = {
      val annots = userAnnots(skipNewLines = true)
      val mods   = modifiers() withAnnotations annots
      tmplDef(mods)
    }

    /** {{{
     *  TmplDef ::= [case] class ClassDef
     *            |  [case] object ObjectDef
     *            |  [override] trait TraitDef
     *  }}}
     */
    def tmplDef(mods: List[Annot]): Symbol.Template = {
      if (mods.isLazy) syntaxError("classes cannot be lazy")
      in.token match {
        case TRAIT =>
          classDef(mods | Flags.TRAIT | Flags.ABSTRACT)
        case CLASS =>
          classDef(mods)
        case CASECLASS =>
          classDef(mods | Flags.CASE)
        case OBJECT =>
          objectDef(pos, mods)
        case CASEOBJECT =>
          objectDef(mods | Flags.CASE)
        case _ =>
          syntaxError("expected start of definition")
      }
    }

    /** {{{
     *  ClassDef ::= Id [TypeParamClause] {Annotation}
     *               [AccessModifier] ClassParamClauses RequiresTypeOpt ClassTemplateOpt
     *  TraitDef ::= Id [TypeParamClause] RequiresTypeOpt TraitTemplateOpt
     *  }}}
     */
    def classDef(mods: List[Annot]): Defn.Class = {
      in.nextToken()
      val nameOffset = in.offset
      val name = identForType()
      savingClassContextBounds {
        val contextBoundBuf = new ListBuffer[Tree]
        val tparams = typeParamClauseOpt(name, contextBoundBuf)
        classContextBounds = contextBoundBuf.toList
        if (!classContextBounds.isEmpty && mods.isTrait) {
          val viewBoundsExist = if (settings.future) "" else " nor view bounds `<% ...'"
            syntaxError(s"traits cannot have type parameters with context bounds `: ...'$viewBoundsExist")
          classContextBounds = List()
        }
        val constrAnnots = if (!mods.isTrait) constructorAnnotations() else Nil
        val (constrMods, vparamss) =
          if (mods.isTrait) (Modifiers(Flags.TRAIT), List())
          else (accessModifierOpt(), paramClauses(name, classContextBounds, ofCaseClass = mods.isCase))
        var mods1 = mods
        if (mods.isTrait) {
          if (settings.YvirtClasses && in.token == SUBTYPE) mods1 |= Flags.DEFERRED
        } else if (in.token == SUBTYPE) {
          syntaxError("classes are not allowed to be virtual")
        }
        val template = templateOpt(mods1, name, constrMods withAnnotations constrAnnots, vparamss)
        val result = TreeGen.mkClassDef(mods1, name, tparams, template)
        result
      }
    }

    /** {{{
     *  ObjectDef       ::= Id ClassTemplateOpt
     *  }}}
     */
    def objectDef(mods: List[Annot]): Defn.Object = {
      in.nextToken()
      val nameOffset = in.offset
      val name = ident()
      val mods1 = if (in.token == SUBTYPE) mods | Flags.DEFERRED else mods
      val template = templateOpt(mods1, name, NoMods, Nil)
      ModuleDef(mods1, name.toTermName, template)
    }

    /** {{{
     *  ClassParents       ::= AnnotType {`(' [Exprs] `)'} {with AnnotType}
     *  TraitParents       ::= AnnotType {with AnnotType}
     *  }}}
     */
    def templateParents(): List[Aux.Parent] = {
      val parents = new ListBuffer[Aux.Parent]
      def readAppliedParent() = {
        val parent = startAnnotType()
        parents += (in.token match {
          case LPAREN => (parent /: multipleArgumentExprs())(Apply.apply)
          case _      => parent
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
    def template(): Aux.Template = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        // @S: pre template body cannot stub like post body can!
        val (self, body) = templateBody(isPre = true)
        if (in.token == WITH && (self eq noSelfType)) {
          val earlyDefs: List[Tree] = body.map(ensureEarlyDef).filter(_.nonEmpty)
          in.nextToken()
          val parents = templateParents()
          val (self1, body1) = templateBodyOpt(parenMeansSyntaxError = false)
          (parents, self1, earlyDefs ::: body1)
        } else {
          (List(), self, body)
        }
      } else {
        val parents = templateParents()
        val (self, body) = templateBodyOpt(parenMeansSyntaxError = false)
        (parents, self, body)
      }
    }

    def ensureEarlyDef(tree: Tree): Tree = tree match {
      case vdef @ ValDef(mods, _, _, _) if !mods.isDeferred =>
        copyValDef(vdef)(mods = mods | Flags.PRESUPER)
      case tdef @ TypeDef(mods, name, tparams, rhs) =>
        deprecationWarning(/* tdef.pos.point */ ???, "early type members are deprecated. Move them to the regular body: the semantics are the same.")
        treeCopy.TypeDef(tdef, mods | Flags.PRESUPER, name, tparams, rhs)
      case docdef @ DocDef(comm, rhs) =>
        treeCopy.DocDef(docdef, comm, rhs)
      case stat if !stat.isEmpty =>
        syntaxError(/* stat.pos */ ???, "only concrete field definitions allowed in early object initialization section")
        EmptyTree
      case _ =>
        EmptyTree
    }

    /** {{{
     *  ClassTemplateOpt ::= `extends' ClassTemplate | [[`extends'] TemplateBody]
     *  TraitTemplateOpt ::= TraitExtends TraitTemplate | [[`extends'] TemplateBody] | `<:' TemplateBody
     *  TraitExtends     ::= `extends' | `<:'
     *  }}}
     */
    def templateOpt(mods: List[Annot], name: Name, constrMods: List[Annot], vparamss: List[List[ValDef]]): Aux.Template = {
      val (parents, self, body) = (
        if (in.token == EXTENDS || in.token == SUBTYPE && mods.isTrait) {
          in.nextToken()
          template()
        }
        else {
          newLineOptWhenFollowedBy(LBRACE)
          val (self, body) = templateBodyOpt(parenMeansSyntaxError = mods.isTrait || name.isTermName)
          (List(), self, body)
        }
      )
      def anyvalConstructor() = (
        // Not a well-formed constructor, has to be finished later - see note
        // regarding AnyVal constructor in AddInterfaces.
        DefDef(NoMods, nme.CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(Nil, Lit.Unit()))
      )

      {
        // Exclude only the 9 primitives plus AnyVal.
        if (inScalaRootPackage && ScalaValueClassNames.contains(name))
          Template(parents, self, anyvalConstructor :: body)
        else
          TreeGen.mkTemplate(TreeGen.mkParents(mods, parents),
                             self, constrMods, vparamss, body)
      }
    }

/* -------- TEMPLATES ------------------------------------------- */

    /** {{{
     *  TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     *  }}}
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateBody(isPre: Boolean) = inBraces(templateStatSeq(isPre = isPre)) match {
      case (self, Nil)  => (self, EmptyTree.asList)
      case result       => result
    }
    def templateBodyOpt(parenMeansSyntaxError: Boolean): (Aux.Self, List[Stmt.Template]) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) {
        templateBody(isPre = false)
      } else {
        if (in.token == LPAREN) {
          if (parenMeansSyntaxError) syntaxError(s"traits or objects may not have parameters")
          else abort("unexpected opening parenthesis")
        }
        (Aux.Self.empty, List())
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

    def statSeq[T <: Tree](stat: PartialFunction[Token, List[T]],
                           errorMsg: String = "illegal start of definition"): List[T] = {
      val stats = new ListBuffer[T]
      def default(tok: Token) =
        if (isStatSep) Nil
        else syntaxError(errorMsg)
      while (!isStatSeqEnd) {
        stats ++= stat.applyOrElse(in.token, default)
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
    def topStat: PartialFunction[Token, List[Stmt.TopLevel]] = {
      case PACKAGE  =>
        in.skipToken()
        packageOrPackageObject() :: Nil
      case IMPORT =>
        in.flushDoc
        importStmt() :: Nil
      case _ if isAnnotation || isTemplateIntro || isModifier =>
        topLevelTmplDef
    }

    /** {{{
     *  TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStats
     *  }}}
     * @param isPre specifies whether in early initializer (true) or not (false)
     */
    def templateStatSeq(isPre : Boolean): (Self, List[Stmt.Template]) = checkNoEscapingPlaceholders {
      var self: Aux.Self = Aux.Self.empty
      var firstOpt: Option[Term] = None
      if (isExprIntro) {
        in.flushDoc
        val first = expr(InTemplate) // @S: first statement is potentially converted so cannot be stubbed.
        if (in.token == ARROW) {
          first match {
            case Term.Ascribe(tree @ Term.This(None), tpt) =>
              self = makeSelfDef(nme.WILDCARD, tpt)
            case _ =>
              convertToParam(first) match {
                case param @ Param.Function(_, name, tpt) =>
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
    def templateStat: PartialFunction[Token, List[Stmt.Template]] = {
      case IMPORT =>
        in.flushDoc
        importClause()
      case _ if isDefIntro || isModifier || isAnnotation =>
        nonLocalDefOrDcl
      case _ if isExprIntro =>
        in.flushDoc
        expr(InTemplate) :: Nil
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

    def refineStat(): List[Stmt.Refine] =
      if (isDclIntro) { // don't IDE hook
        defOrDcl(NoMods)
      } else if (!isStatSep) {
        syntaxError(
          "illegal start of declaration"+
          (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
           else ""))
      } else Nil

    def localDef(implicitMod: Int): List[Stmt.Block] = {
      val annots = userAnnots(skipNewLines = true)
      val pos = in.offset
      val mods = (localModifiers() | implicitMod.toLong) withAnnotations annots
      val defs: List[Stmt.Block] =
        if (!(mods hasFlag ~(Flags.IMPLICIT | Flags.LAZY))) defOrDcl(mods)
        else List(tmplDef(mods))

      in.token match {
        case RBRACE | CASE  => defs :+ Lit.Unit()
        case _              => defs
      }
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
            val start = in.skipToken()
            if (isIdent) stats += implicitClosure(InBlock)
            else stats ++= localDef(Flags.IMPLICIT)
          } else {
            stats ++= localDef(0)
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

    def packageObject(): Package.Object = ???
    // makePackageObject(objectDef(NoMods))

    /** {{{
     *  CompilationUnit ::= {package QualId semi} TopStatSeq
     *  }}}
     */
    def compilationUnit(): Package = {
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
            in.flushDoc
            val qid = qualId()

            if (in.token == EOF) {
              ts += Package.Named(qid, List())
            } else if (isStatSep) {
              in.nextToken()
              ts += Package.Named(qid, packageStats())
            } else {
              ts += inBraces(Package.Named(qid, topStatSeq()))
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
        case (stat: Package) :: Nil => stat
        case stats                  => Package.Empty(stats)
      }
    }
  }
}

object Parsers extends Parsers

package scala.meta
package internal
package quasiquotes

import org.scalameta._
import org.scalameta.adt.{Liftables => AdtLiftables}
import org.scalameta.invariants._

import scala.annotation.tailrec
import scala.meta.dialects
import scala.meta.parsers._
import scala.meta.tokenizers._
import scala.meta.internal.trees._
import scala.meta.internal.trees.{Liftables => AstLiftables, Reflection => AstReflection}
import scala.meta.internal.parsers.Messages
import scala.meta.internal.parsers.Absolutize._
import scala.meta.quasiquotes._

import scala.runtime.ScalaRunTime
import scala.language.implicitConversions
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.quoted._
import scala.meta.trees.Origin

object ReificationMacros {
  def termImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) =
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Term).asExprOf[scala.meta.Tree]
  def termParamImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) =
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.TermParam).asExprOf[scala.meta.Term.Param]
  def typeImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Type).asExprOf[scala.meta.Type]
  def typeParamImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]) = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.TypeParam).asExprOf[scala.meta.Type.Param]
  def caseOrPatternImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Tree] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.CaseOrPattern).asExprOf[scala.meta.Tree]
  def initImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Init] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Init).asExprOf[scala.meta.Init]
  def selfImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Self] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Self).asExprOf[scala.meta.Self]
  def templateImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Template] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Template).asExprOf[scala.meta.Template]
  def modImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Mod] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Mod).asExprOf[scala.meta.Mod]
  def enumeratorImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Enumerator] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Enumerator).asExprOf[scala.meta.Enumerator]
  def importerImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Importer] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Importer).asExprOf[scala.meta.Importer]
  def importeeImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Importee] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Importee).asExprOf[scala.meta.Importee]
  def sourceImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Expr[scala.meta.Source] = 
    new ReificationMacros().expandApply(scExpr, argsExpr, QuasiquoteType.Source).asExprOf[scala.meta.Source]

  def unapplyImpl(using Quotes)(scCallExpr: Expr[QuasiquoteUnapply], scrutineeExpr: Expr[Any]): Expr[Any] =
    val (stringContext, quasiquoteType) = scCallExpr match
      case '{ ($sc: StringContext).q } => (sc, QuasiquoteType.Term)
      case '{ ($sc: StringContext).param } => (sc, QuasiquoteType.TermParam)
      case '{ ($sc: StringContext).t } => (sc, QuasiquoteType.Type)
      case '{ ($sc: StringContext).tparam } => (sc, QuasiquoteType.TypeParam)
      case '{ ($sc: StringContext).p } => (sc, QuasiquoteType.CaseOrPattern)
      case '{ ($sc: StringContext).init } => (sc, QuasiquoteType.Init)
      case '{ ($sc: StringContext).self } => (sc, QuasiquoteType.Self)
      case '{ ($sc: StringContext).template } => (sc, QuasiquoteType.Template)
      case '{ ($sc: StringContext).mod } => (sc, QuasiquoteType.Mod)
      case '{ ($sc: StringContext).enumerator } => (sc, QuasiquoteType.Enumerator)
      case '{ ($sc: StringContext).importer } => (sc, QuasiquoteType.Importer)
      case '{ ($sc: StringContext).importee } => (sc, QuasiquoteType.Importee)
      case '{ ($sc: StringContext).source } => (sc, QuasiquoteType.Source)
      case _ => quotes.reflect.report.errorAndAbort("Expected call to quasiquote extension method. ")
    new ReificationMacros().expandUnapply(stringContext, scrutineeExpr, quasiquoteType)
}

class ReificationMacros(using val topLevelQuotes: Quotes) { rei =>
  import topLevelQuotes.reflect._
  import scala.meta.{Tree => MetaTree, Dialect}
  import scala.meta.inputs.{Position => MetaPosition, _}
  type MetaParser = (Input, Dialect) => MetaTree
  lazy val RootPackage = topLevelQuotes.reflect.Symbol.classSymbol("_root_")

  private sealed trait Mode {
    def isTerm: Boolean = this.isInstanceOf[Mode.Term]
    def isPattern: Boolean = this.isInstanceOf[Mode.Pattern]
    def hasHoles: Boolean = this match
      case Mode.Term(_, holes) => !holes.isEmpty
      case Mode.Pattern(_, holes, _) => !holes.isEmpty
    
    def multiline: Boolean
  }
  private object Mode {
    case class Term(multiline: Boolean, holes: List[TermHole]) extends Mode
    case class Pattern(multiline: Boolean, holes: List[PatternHole], unapplySelector: Expr[Any]) extends Mode
  }
  private case class TermHole(name: String, arg: Term, var reifier: Option[Term])
  private case class PatternHole(name: String, posStart: Int, posEnd: Int, tpe: Option[TypeRepr], var symbol: Option[Symbol], var reifier: Option[Term])

  lazy val ListClass = TypeRepr.of[scala.List]

  def expandUnapply(strCtx: Expr[StringContext], unapplySelector: Expr[Any], qType: QuasiquoteType): Expr[Any] = {
    val mode = extractModePattern(strCtx, unapplySelector)
    expand(strCtx, qType, mode)
  }

  def expandApply(strCtx: Expr[StringContext], args: Expr[Seq[Any]], qType: QuasiquoteType): Expr[Any] = {
    val mode = extractModeTerm(strCtx, args)
    expand(strCtx, qType, mode)
  }

  private def expand(strCtx: Expr[StringContext], qType: QuasiquoteType, mode: Mode) = {
    val input = metaInput()
    val (dialect, dialectExpr) = instantiateDialect(mode)
    val parser = instantiateParser(qType)
    val skeleton = parseSkeleton(parser, input, dialect)
    reifySkeleton(skeleton, mode, qType, dialectExpr, input)
  }

  private def metaInput() = {
    val pos = Position.ofMacroExpansion
    val reflectInput = pos.sourceFile
    val content = new String(reflectInput.content.get)
    val start = {
      var i = 0
      while (content(pos.start + i) != '"') i += 1 // skip method name
      while (content(pos.start + i) == '"') i += 1 // skip quotations
      pos.start + i
    }
    val end = {
      var i = 0
      while (content(pos.end - 1 - i) == '"') i += 1 // skip quotations
      pos.end - i
    }
    val metaInput = Input.VirtualFile(reflectInput.path, content)
    Input.Slice(metaInput, start, end)
  }

  private def extractModeTerm(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Mode = {
    val pos = Position.ofMacroExpansion
    def isMultiline() = pos.startLine != pos.endLine
    def mkHole(argi: (Expr[Any], Int)) = {
      val (arg, i) = argi
      val name = "quasiquote" + "$hole$" + i
      TermHole(name, arg.asTerm, reifier = None)
    }
    val Varargs(args) = argsExpr: @unchecked
    val holes = args.zipWithIndex.map(mkHole)
    Mode.Term(isMultiline(), holes.toList)
  }

  private def extractModePattern(strCtxExpr: Expr[StringContext], selectorExpr: Expr[Any]): Mode = {
    val '{StringContext(${Varargs(parts)}: _*)} = strCtxExpr: @unchecked
    val pos = Position.ofMacroExpansion

    // EXPERIMENT: Workaround mechanism for getting type declarations from unapply pattern.
    // Only works for `val q"${: T}" = (...)`, does nothing for pattern matching cases.
    // Even then, it stops working if macro entry method is a transparent inline method
    // (which we want to have, so for now it's turned off). 
    val argTypes: Option[List[TypeRepr]] = None
      // try
      //   Symbol.spliceOwner.owner.owner.owner.tree match
      //     case ValDef(_, tpe, Some(Match(_, List(CaseDef(Unapply(Select(Block(List(typeDef), _), "unapplySeq"), _, _), _, _))))) if typeDef == Symbol.spliceOwner.owner.owner.tree =>
      //       if parts.length > 2 then
      //         tpe.tpe match
      //           case AppliedType(tuple, lst) => Some(lst)
      //       else
      //         Some(List(tpe.tpe))
      //     case _ => None
      // catch
      //   case _ => None
    
    def isMultiline() = pos.startLine != pos.endLine
    def mkHole(i: Int) = {
      val name = "quasiquote" + "$hole$" + i
      val posStart = parts(i).asTerm.pos.end
      val posEnd = parts(i + 1).asTerm.pos.start
      val argType = argTypes.map(_(i))
      PatternHole(name, posStart, posEnd, argType, None, None)
    }
    Mode.Pattern(isMultiline(), List.range(0, parts.length - 1).map(mkHole(_)), selectorExpr)
  }

  private def instantiateDialect(mode: Mode): (Dialect, Expr[Dialect]) = {
    val dialectExpr = Expr.summon[scala.meta.Dialect]
    // For now only a predefined set of dialects is supported.
    // See comment in the scala 2 counterpart file.
    val underlyingDialect = {
      def instantiateStandardDialect(sym: Symbol): Option[Dialect] = {
        val dialectsSym = Symbol.classSymbol("scala.meta.dialects.package$").companionModule
        if (dialectsSym == sym.owner.companionModule) {
          if (dialectsSym.methodMember(sym.name).isEmpty) None
          else Dialect.standards.get(sym.name)
        } else None
      }

      // allow `scala.meta.dialects.Scala211`
      val standardDialectReference = dialectExpr.flatMap(expr => instantiateStandardDialect(expr.asTerm.tpe.termSymbol))

      // allow `scala.meta.dialects.current`
      def standardDialectSingleton = dialectExpr.flatMap { _.asTerm.tpe match
        case termRef @ TermRef(prefix, _) => // TermRef(scala.meta.Dialect, method current)
          instantiateStandardDialect(prefix.memberType(termRef.termSymbol).termSymbol)
        case _ => None
      }

      standardDialectReference.orElse(standardDialectSingleton).getOrElse {
        val suggestion =
          s"to fix this, import something from scala.meta.dialects, e.g. scala.meta.dialects.${Dialect.current}"
        val message =
          s"${dialectExpr.get.show} of type ${dialectExpr.get.asTerm.tpe.show} is not supported by quasiquotes ($suggestion)"
        report.errorAndAbort(message)
      }
    }
    (if (mode.isTerm) underlyingDialect.unquoteTerm(mode.multiline)
    else underlyingDialect.unquotePat(mode.multiline), dialectExpr.get)
  }
  private def instantiateParser(qType: QuasiquoteType): MetaParser = {
    val parserModule = Symbol.classSymbol(qType.parserClass())
    val parsersModuleClass =
      Class.forName("scala.meta.quasiquotes.package$", true, this.getClass.getClassLoader)
    val parsersModule = parsersModuleClass.getField("MODULE$").get(null)
    val parserModuleGetter = parsersModule.getClass.getDeclaredMethod(parserModule.name.toString)
    val parserModuleInstance = parserModuleGetter.invoke(parsersModule)
    val parserMethod =
      parserModuleInstance.getClass.getDeclaredMethods.find(_.getName == "parse").head
    (input: Input, dialect: Dialect) => {
      try parserMethod.invoke(parserModuleInstance, input, dialect).asInstanceOf[MetaTree]
      catch { case ex: java.lang.reflect.InvocationTargetException => throw ex.getTargetException }
    }
  }
  private def parseSkeleton(parser: MetaParser, input: Input, dialect: Dialect): MetaTree = {
    try {
      parser(input, dialect)
    } catch {
      case TokenizeException(pos, message) => report.errorAndAbort(message)
      case ParseException(pos, message) => report.errorAndAbort(message)
    }
  }
  private def reifySkeleton(meta: MetaTree, mode: Mode, qType: QuasiquoteType, dialectExpr: Expr[Dialect], input: Input): Expr[Any] = {
    val pendingQuasis = mutable.Stack[Quasi]()

    val useParsedSource = !mode.hasHoles // otherwise, syntax will not make much sense
    val vDef =
      if (useParsedSource)
        '{new Origin.ParsedSource(scala.meta.inputs.Input.String(${Expr(input.text.replace("$$", "$"))}))}
      else
        '{scala.compiletime.summonInline[Origin.DialectOnly]}

    val (valDef, originRef) = 
      val sym = Symbol.newVal(Symbol.spliceOwner, "origin", if (useParsedSource) TypeRepr.of[Origin.ParsedSource] else TypeRepr.of[Origin.DialectOnly], Flags.EmptyFlags, Symbol.noSymbol)
      (ValDef(sym, Some(vDef.asTerm.changeOwner(sym))), Ref(sym))

    object Internal extends InternalTrait with ExprLifts(using quotes)(mode.isPattern, dialectExpr) {
      import internalQuotes.reflect._
      def liftTree(tree: MetaTree): internalQuotes.reflect.Tree = {
        this.liftableSubTree0(tree).asInstanceOf[internalQuotes.reflect.Tree]
      }
      def liftOptionTree[T: Type](maybeTree: Option[MetaTree]): internalQuotes.reflect.Tree = {
        maybeTree match {
          case Some(tree: Quasi) => liftQuasi0(tree, optional = true)
          case Some(otherTree) =>
            if mode.isTerm then 
              Apply(TypeApply(Select.unique('{scala.Some}.asTerm, "apply"), List(TypeTree.of[T])), List(liftTree(otherTree).asInstanceOf[Term]))
            else 
              TypedOrTest(Unapply(TypeApply(Select.unique(Ref(Symbol.classSymbol("scala.Some").companionModule), "unapply"), List(TypeTree.of[T])), Nil, List(liftTree(otherTree))), TypeTree.of[Some[T]])
          case None =>
            '{scala.None}.asTerm match
              case Inlined(_, _, none) => none
        }
      }
      def liftTrees[T: Type](trees: Seq[MetaTree]): Tree = {
        @tailrec
        def loop(trees: Seq[MetaTree], acc: Option[Tree], prefix: List[MetaTree]): Tree =
          trees match {
            case (quasi: Quasi) +: rest if quasi.rank == 1 =>
              if (acc.isEmpty) {
                if (prefix.isEmpty) loop(rest, Some(liftQuasi0(quasi)), Nil)
                else
                  loop(
                    rest,
                    prefix.foldRight(acc)((curr, acc) => {
                      // Note copied from the Scala 2 counterpart file.
                      // NOTE: We cannot do just q"${liftTree(curr)} +: ${liftQuasi(quasi)}"
                      // because that creates a synthetic temp variable that doesn't make any sense in a pattern.
                      // Neither can we do q"${liftQuasi(quasi)}.+:(${liftTree(curr)})",
                      // because that still wouldn't work in pattern mode.
                      // Finally, we can't do something like q"+:(${liftQuasi(quasi)}, (${liftTree(curr)}))",
                      // because would violate evaluation order guarantees that we must keep.
                      val currElement = liftTree(curr)
                      val alreadyLiftedList = acc.getOrElse(liftQuasi(quasi))
                      val tree = 
                        if (mode.isTerm) Apply(TypeApply(Select.unique(alreadyLiftedList.asInstanceOf[Term], "+:"), List(TypeTree.of[T])), List(currElement.asInstanceOf[Term]))
                        else Unapply(TypeApply(Select.unique('{+:}.asTerm, "unapply"), List(TypeTree.of[T], TypeTree.of[List], TypeTree.of[List[T]])), Nil, List(currElement, alreadyLiftedList))
                      Some(tree)
                    }),
                    Nil
                  )
              } else {
                Predef.require(prefix.isEmpty && debug(trees, acc, prefix))
                if (mode.isTerm) then
                  val tree = Apply(TypeApply(Select.unique(acc.get.asInstanceOf[Term], "++"), List(TypeTree.of[T])), List(liftQuasi(quasi).asInstanceOf[Term]))
                  loop(rest, Some(tree), Nil)
                else report.errorAndAbort(Messages.QuasiquoteAdjacentEllipsesInPattern(quasi.rank))
              }
            case other +: rest =>
              if (acc.isEmpty) loop(rest, acc, prefix :+ other)
              else {
                Predef.require(prefix.isEmpty && debug(trees, acc, prefix))
                val otherTree = liftTree(other)
                val tree =
                  if mode.isTerm then Apply(TypeApply(Select.unique(acc.get.asInstanceOf[Term], ":+"), List(TypeTree.of[T])), List(otherTree.asInstanceOf[Term]))
                  else TypedOrTest(Unapply(TypeApply(Select.unique('{:+}.asTerm, "unapply"), List(TypeTree.of[T], TypeTree.of[List], TypeTree.of[List[T]])), Nil, List(acc.get, otherTree)), TypeTree.of[List[Any]])
                loop(rest, Some(tree), Nil)
              }
            case _ =>
              if (acc.isEmpty) {
                val args = prefix.map(liftTree(_)).toList
                val targs = List(TypeTree.of[T])
                if mode.isTerm then
                  Select.overloaded('{List}.asTerm, "apply", List(TypeRepr.of[T]), args.asInstanceOf[List[Term]])
                else
                  TypedOrTest(Unapply(TypeApply(Select.unique('{List}.asTerm, "unapplySeq"), List(TypeTree.of[T])), Nil, args), TypeTree.of[List[T]])
              }
              else acc.get
          }
        loop(trees, None, Nil)
      }
      def liftTreess(treess: List[List[MetaTree]]): Tree = {
        val tripleDotQuasis = treess.flatten.collect {
          case quasi: Quasi if quasi.rank == 2 => quasi
        }
        if (tripleDotQuasis.isEmpty) {
          val args = treess.map(liftTrees)
          if mode.isTerm then
            Select.overloaded('{List}.asTerm, "apply", List(TypeRepr.of[Any]), args.asInstanceOf[List[Term]])
          else 
            TypedOrTest(Unapply(TypeApply(Select.unique('{List}.asTerm, "unapplySeq"), List(TypeTree.of[Any])), Nil, args), TypeTree.of[List[Any]])
        } else if (tripleDotQuasis.length == 1) {
          if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
          else
            report.errorAndAbort(
              "implementation restriction: can't mix ...$ with anything else in parameter lists." + EOL +
                "See https://github.com/scalameta/scalameta/issues/406 for details."
            )
        } else {
          report.errorAndAbort(Messages.QuasiquoteAdjacentEllipsesInPattern(2))
        }
      }
      def liftQuasi0(quasi: Quasi, optional: Boolean = false): Tree = {
        try {
          pendingQuasis.push(quasi)
          if (quasi.rank == 0) {
            val inferredPt = {
              val unwrappedPt = quasi.pt.wrap(pendingQuasis.map(_.rank).sum).toTpe
              if optional then 
                unwrappedPt.asType match
                case '[t] => rei.topLevelQuotes.reflect.TypeRepr.of[Option[t]]
              else unwrappedPt
            }
            val lifted = mode match {
              case Mode.Term(_, _) =>
                inferredPt.asType match
                  case '[t] => '{scala.meta.internal.quasiquotes.Lift[t](${quasi.termHole.arg.asExpr})}.asTerm
              case Mode.Pattern(_, _, _) =>
                // Note copied from the Scala 2 counterpart file.
                // NOTE: Here, we would like to say q"$InternalUnlift[$inferredPt](${quasi.hole.arg})".
                // Unfortunately, the way how patterns work prevents us from having it this easy:
                // 1) unapplications can't have explicitly specified type arguments
                // 2) pattern language is very limited and can't express what we want to express in Unlift
                // Therefore, we're forced to take a two-step unquoting scheme: a) match everything in the corresponding hole,
                // b) call Unlift.unapply as a normal method in the right-hand side part of the pattern matching clause.
                val hole = quasi.patternHole
                val symbol = Symbol.newBind(Symbol.spliceOwner, hole.name, Flags.EmptyFlags, inferredPt.asInstanceOf[TypeRepr])
                val reifier =
                  hole.tpe match
                    case Some(tpe) => Select.overloaded('{scala.meta.internal.quasiquotes.Unlift}.asTerm, "unapply", List(tpe.asInstanceOf[TypeRepr]), List(Ref(symbol)))
                    case None => Select.overloaded('{scala.meta.internal.quasiquotes.Unlift}.asTerm, "unapply", List(inferredPt.asInstanceOf[TypeRepr]), List(Ref(symbol)))
                hole.symbol = Some(symbol).asInstanceOf[Option[rei.topLevelQuotes.reflect.Symbol]]
                hole.reifier = Some(reifier).asInstanceOf[Option[rei.topLevelQuotes.reflect.Term]]
                Bind(symbol, Wildcard())
            }
            lifted
          } else {
            quasi.tree match {
              case quasi: Quasi if quasi.rank == 0 => liftQuasi0(quasi)
              case _ => report.errorAndAbort("complex ellipses are not supported yet")
            }
          }
        } finally {
          pendingQuasis.pop()
        }
      }

      def liftOrigin(origin: Origin): internalQuotes.reflect.Tree = {
        if (useParsedSource) origin match {
          case Origin.Parsed(_, beg, end) => '{Origin.Parsed(${originRef.asExprOf[Origin.ParsedSource]}, ${Expr(beg)}, ${Expr(end)})}.asTerm
          case x => report.errorAndAbort("likely missing positions in the parser")
        }
        else originRef.asInstanceOf[internalQuotes.reflect.Tree]
      }

      // Depending on pattern types, this is able to cause some custom errors to be thrown
      // We cannot obtain the types of the pattern in Scala 3 for now
      protected def unquotesName(q: scala.meta.internal.trees.Quasi): Boolean = {
        false
      }
    }

    implicit class XtensionRankedClazz(clazz: Class[_]) {
      def unwrap: Class[_] = {
        if (clazz.isArray) clazz.getComponentType.unwrap
        else clazz
      }
      def wrap(rank: Int): Class[_] = {
        if (rank == 0) clazz
        else ScalaRunTime.arrayClass(clazz).wrap(rank - 1)
      }
      def toTpe: TypeRepr = {
        if (clazz.isArray) {
          clazz.getComponentType.toTpe.asType match
            case '[t] => TypeRepr.of[List[t]]
        } else {
          val name = clazz.getName.replace("$", ".")
          TypeIdent(Symbol.classSymbol(name)).tpe
        }
      }
    }
    implicit class XtensionQuasiHole(quasi: scala.meta.internal.trees.Quasi) {
      import quotes.reflect._
      def termHole: TermHole = {
        val pos = quasi.pos.absolutize
        (mode: @unchecked) match
          case Mode.Term(_, holes) =>
            val maybeHole =
              holes.find(h => pos.start <= h.arg.pos.start && h.arg.pos.start <= pos.end)
            maybeHole.getOrElse{
              val message = "this code path should've been unreachable"
              val relevantValues = 
                s"""
                |quasi = ${quasi}
                |quasi.pos.absolutize = ${quasi.pos.absolutize}
                |holes = ${holes}
                |""".stripMargin
              throw new UnreachableError(message + relevantValues)
            }
      }
      def patternHole: PatternHole = {
        val pos = quasi.pos.absolutize
        (mode: @unchecked) match
          case Mode.Pattern(_, holes, _) =>
            val maybeHole =
              holes.find(h => pos.start <= h.posStart && h.posStart <= pos.end)
            maybeHole.getOrElse{
              val message = "this code path should've been unreachable"
              val relevantValues = 
                s"""
                |quasi = ${quasi}
                |quasi.pos.absolutize = ${quasi.pos.absolutize}
                |holes = ${holes}
                |""".stripMargin
              throw new UnreachableError(message + relevantValues)
            }
      }
    }

    mode match {
      case Mode.Term(_, _) =>
        val internalResult = Internal.liftTree(meta).asInstanceOf[Term].asExpr
        if (sys.props("quasiquote.debug") != null) {
          println(internalResult)
        }
        val resType =
          qType match
            case QuasiquoteType.Term => TypeRepr.of[scala.meta.Tree]
            case QuasiquoteType.TermParam => TypeRepr.of[scala.meta.Term.Param]
            case QuasiquoteType.Type => TypeRepr.of[scala.meta.Type]
            case QuasiquoteType.TypeParam => TypeRepr.of[scala.meta.Type.Param]
            case QuasiquoteType.CaseOrPattern => TypeRepr.of[scala.meta.Tree]
            case QuasiquoteType.Init => TypeRepr.of[scala.meta.Init]
            case QuasiquoteType.Self => TypeRepr.of[scala.meta.Self]
            case QuasiquoteType.Template => TypeRepr.of[scala.meta.Template]
            case QuasiquoteType.Mod => TypeRepr.of[scala.meta.Mod]
            case QuasiquoteType.Enumerator => TypeRepr.of[scala.meta.Enumerator]
            case QuasiquoteType.Importer => TypeRepr.of[scala.meta.Importer]
            case QuasiquoteType.Importee => TypeRepr.of[scala.meta.Importee]
            case QuasiquoteType.Source => TypeRepr.of[scala.meta.Source]
        resType.asType match
          case '[t] => 
            Block(
              List(valDef),
              '{
                scala.meta.internal.quasiquotes.Unlift[t]($internalResult)
              }.asTerm
            ).asExprOf[t]
  
      case Mode.Pattern(_, holes, unapplySelector) =>
        import Internal.internalQuotes.reflect._

        val pattern = Internal.liftTree(meta)

        val args = holes.map(hole => hole.reifier.get.asInstanceOf[Term])
        val argsContents = args.flatMap { _.tpe match
          case AppliedType(tpe, content) if tpe =:= TypeRepr.of[Option] => content
        }
        val lst = Select.overloaded('{List}.asTerm, "apply", List(TypeRepr.of[Option[Any]]), args).asExprOf[List[Option[Any]]]

        val n = args.length
        val (returned, isBooleanExtractor) = 
          if (n == 0) {
            // boolean extractor
            ('{true}, true)
          } else if (n == 1) {
            // returning Tuple1 requires us to manually unapply later, so we return the raw value instead
            ('{${args.head.asExprOf[Option[Any]]}.get}, false)
          } else {
            (Select.overloaded(Ref(Symbol.classSymbol("scala.Tuple" + n).companionModule), "apply", argsContents, args.map(arg => '{${arg.asExprOf[Option[Any]]}.get}.asTerm)).asExpr, false)
          }
        returned.asTerm.tpe.asType match
          case '[t] =>
            def thenp(inputExpr: Expr[Any])(using Quotes) = {
              import quotes.reflect._
              if (isBooleanExtractor) then '{true}
              else
                '{
                  val tpl = $lst
                  if tpl.toList.forall(_.asInstanceOf[Option[Any]].isDefined) then Some(${returned.asExprOf[t]})
                  else None
                }
            }
            def elsep(using Quotes) = 
              if (isBooleanExtractor) '{false}
              else '{scala.None}
            
            def matchp(input: Expr[Any])(using Quotes) = {
              val matchTerm = Match(
                input.asTerm,
                List(
                  CaseDef(pattern, None, thenp(input).asTerm),
                  CaseDef(Wildcard(), None, elsep.asTerm)
                )
              )
              if (isBooleanExtractor) matchTerm.asExprOf[Boolean]
              else matchTerm.asExprOf[Option[t]]
            }

            val internalResult =
              if (isBooleanExtractor) {
                Block(
                  List(valDef.asInstanceOf[Internal.internalQuotes.reflect.Statement]),
                  '{ 
                    ${matchp(unapplySelector)}.asInstanceOf[Boolean]
                  }.asTerm
                ).asExprOf[Boolean]
              } else {
                Block(
                  List(valDef.asInstanceOf[Internal.internalQuotes.reflect.Statement]),
                  '{
                    ${matchp(unapplySelector)}.asInstanceOf[Option[t]]
                  }.asTerm
                ).asExprOf[Option[t]]
              }
            
            if (sys.props("quasiquote.debug") != null) {
              println(internalResult)
            }

            internalResult

    }
  }
}

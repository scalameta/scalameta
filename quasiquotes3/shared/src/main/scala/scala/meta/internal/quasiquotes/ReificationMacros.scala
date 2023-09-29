package scala.meta
package internal
package quasiquotes

import scala.runtime.ScalaRunTime
import scala.language.implicitConversions
import scala.collection.mutable
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
import scala.compat.Platform.EOL
import scala.meta.quasiquotes.Qunapply
import scala.meta.quasiquotes.q

import scala.quoted._

object ReificationMacros {
  def quasiquoteImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]], dialectExpr: Expr[Dialect]): Expr[Any] =
    new ReificationMacros().expandApply(scExpr, argsExpr, dialectExpr, QuasiquoteType.Q)
  def typeImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]], dialectExpr: Expr[Dialect]) = 
    new ReificationMacros().expandApply(scExpr, argsExpr, dialectExpr, QuasiquoteType.T)
  def caseOrPatternImpl(using Quotes)(scExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]], dialectExpr: Expr[Dialect]) = 
    new ReificationMacros().expandApply(scExpr, argsExpr, dialectExpr, QuasiquoteType.P)


  def unapplyQuasiquoteImpl(using Quotes)(qtodoExpr: Expr[Qunapply], argsExpr: Expr[Any], dialectExpr: Expr[Dialect]): Expr[Option[Seq[Any]]] =
    val scExpr: Expr[StringContext] =
      qtodoExpr match
        case '{ ($stringContext: StringContext).q } => stringContext
        case _ => quotes.reflect.report.errorAndAbort("Expected call to extension method `q(StringContext)`")
    new ReificationMacros().expandUnapply(scExpr, argsExpr, dialectExpr, QuasiquoteType.Q)
}

class ReificationMacros(using val quotes: Quotes) {
  import quotes.reflect._
  import scala.meta.{Tree => MetaTree, Dialect => Dialect}
  import scala.meta.inputs.{Position => MetaPosition, _}
  type MetaParser = (Input, Dialect) => MetaTree
  lazy val RootPackage = quotes.reflect.Symbol.classSymbol("_root_")

  private sealed trait Mode {
    def isTerm: Boolean = this.isInstanceOf[Mode.Term]
    def isPattern: Boolean = this.isInstanceOf[Mode.Pattern]
    def multiline: Boolean
    def holes: List[Hole]
  }
  private object Mode {
    case class Term(multiline: Boolean, holes: List[Hole]) extends Mode
    case class Pattern(multiline: Boolean, holes: List[Hole], unapplySelector: Expr[Any]) extends Mode
  }
  private case class Hole(name: String, arg: Term, var reifier: Option[Term])

  lazy val ListClass = TypeRepr.of[scala.List]

  def expandUnapply(strCtx: Expr[StringContext], unapplySelector: Expr[Any], dialectExpr: Expr[Dialect], qType: QuasiquoteType): Expr[Option[Seq[Any]]] = {
    val mode = extractModePattern(strCtx, unapplySelector)
    expand(strCtx, dialectExpr, qType, mode).asExprOf[Option[Seq[Any]]]
  }

  def expandApply(strCtx: Expr[StringContext], args: Expr[Seq[Any]], dialectExpr: Expr[Dialect], qType: QuasiquoteType): Expr[Any] = {
    val mode = extractModeTerm(strCtx, args)
    expand(strCtx, dialectExpr, qType, mode)
  }

  private def expand(strCtx: Expr[StringContext], dialectExpr: Expr[Dialect], qType: QuasiquoteType, mode: Mode) = {
    val input = metaInput()
    val dialect = instantiateDialect(dialectExpr, mode)
    val parser = instantiateParser(qType)
    val skeleton = parseSkeleton(parser, input, dialect)
    reifySkeleton(skeleton, mode, qType)
  }

  private def metaInput() = {
    val pos = Position.ofMacroExpansion
    val reflectInput = pos.sourceFile
    val start = pos.start + 2
    val end = pos.end - 1
    val metaInput = Input.VirtualFile(reflectInput.path, new String(reflectInput.content.get))
    Input.Slice(metaInput, start, end)
  }

  private def extractModeTerm(strCtxExpr: Expr[StringContext], argsExpr: Expr[Seq[Any]]): Mode = {
    val pos = Position.ofMacroExpansion
    def isMultiline() = {
      pos.startLine != pos.endLine
    }
    def mkHole(argi: (Expr[Any], Int)) = {
      val (arg, i) = argi
      val name = "quasiquote" + "$hole$" + i
      Hole(name, arg.asTerm, reifier = None)
    }
    val Varargs(args) = argsExpr
    val holes = args.zipWithIndex.map(mkHole)
    Mode.Term(isMultiline(), holes.toList)
  }

  private def extractModePattern(strCtxExpr: Expr[StringContext], selectorExpr: Expr[Any]): Mode = {
    val pos = Position.ofMacroExpansion
    def isMultiline() = {
      pos.startLine != pos.endLine
    }
    def mkHole(argi: (Expr[Any], Int)) = {
      val (arg, i) = argi
      val name = "quasiquote" + "$hole$" + i
      Hole(name, arg.asTerm, reifier = None)
    }
    Mode.Pattern(isMultiline(), List(mkHole(selectorExpr, 0)), selectorExpr) // probably incorrect
  }

  private def instantiateDialect(dialectExpr: Expr[Dialect], mode: Mode): Dialect = {
    // NOTE: We want to have a higher-order way to abstract over differences in dialects
    // and we're using implicits for that (implicits are values => values are higher-order => good).
    //
    // However, quasiquotes use macros, and macros are first-order, so we have a problem here.
    // Concretely, here we need to convert an implicit argument to a macro (the `dialectTree` tree)
    // into an instance of `Dialect` that we'll pass to the parser.
    //
    // For now I'll just prohibit quasiquotes for situations when `dialectTree` doesn't point to one of the predefined dialects.
    // A natural extension to this would be to allow any static value, not just predefined dialects.
    // Furthermore, we could further relax this restriction by doing parsing for a superset of all dialects and then
    // delaying validation of resulting ASTs until runtime.
    val underlyingDialect = {
      def instantiateStandardDialect(sym: Symbol): Option[Dialect] = {
        val dialectsSym = Symbol.classSymbol("scala.meta.dialects.package$")
        if (dialectsSym != sym.owner) return None
        if (dialectsSym.methodMember(sym.name) == Symbol.noSymbol) return None // TODO Seems redundant ??
        Dialect.standards.get(sym.name.toString)
      }
      val standardDialectSingleton = instantiateStandardDialect(dialectExpr.asTerm.tpe.termSymbol)
      standardDialectSingleton
        .getOrElse({
          val suggestion =
            s"to fix this, import something from scala.meta.dialects, e.g. scala.meta.dialects.${Dialect.current}"
          val message =
            s"${dialectExpr.show} of type ${dialectExpr.asTerm.tpe.show} is not supported by quasiquotes ($suggestion)"
          report.errorAndAbort(message)
        })
    }
    if (mode.isTerm) dialects.QuasiquoteTerm(underlyingDialect, mode.multiline)
    else dialects.QuasiquotePat(underlyingDialect, mode.multiline)
  }
  private def instantiateParser(qType: QuasiquoteType): MetaParser = {
    val parserModule = 
      qType match
        case QuasiquoteType.Q => Symbol.classSymbol("scala.meta.quasiquotes.Api.XTensionQuasiquoteTerm")
        case QuasiquoteType.T => Symbol.classSymbol("scala.meta.quasiquotes.Api.XTensionQuasiquoteType")
        case QuasiquoteType.P => Symbol.classSymbol("scala.meta.quasiquotes.Api.XTensionQuasiquoteCaseOrPattern")
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
      case TokenizeException(pos, message) => report.errorAndAbort(message) // todo set correct position
      case ParseException(pos, message) => report.errorAndAbort(message) // todo set correct position
    }
  }
  private def reifySkeleton(meta: MetaTree, mode: Mode, qType: QuasiquoteType): Expr[Any] = {
    val pendingQuasis = mutable.Stack[Quasi]()
    object Internal extends InternalTrait with ExprLifts {
      def liftTree(tree: MetaTree)(using Quotes): Expr[Any] = {
        this.liftableSubTree(tree)
      }
      def liftOptionTree(maybeTree: Option[MetaTree]): Expr[Option[Any]] = {
        maybeTree match {
          case Some(tree: Quasi) => liftQuasi0(tree, optional = true).asExprOf[Option[Any]]
          case Some(otherTree) => '{Some(${liftTree(otherTree)})}
          case None => '{None}
        }
      }
      def liftTrees[T: Type](trees: Seq[MetaTree])(using Quotes): Expr[List[Any]] = {
        @tailrec
        def loop(trees: Seq[MetaTree], acc: Option[Expr[List[Any]]], prefix: List[MetaTree]): Expr[List[Any]] =
          trees match {
            case (quasi: Quasi) +: rest if quasi.rank == 1 =>
              if (acc.isEmpty) {
                if (prefix.isEmpty) loop(rest, Some(liftQuasi(quasi).asExprOf[List[Any]]), Nil)
                else
                  loop(
                    rest,
                    prefix.foldRight(acc)((curr, acc) => {
                      // NOTE: We cannot do just q"${liftTree(curr)} +: ${liftQuasi(quasi)}"
                      // because that creates a synthetic temp variable that doesn't make any sense in a pattern.
                      // Neither can we do q"${liftQuasi(quasi)}.+:(${liftTree(curr)})",
                      // because that still wouldn't work in pattern mode.
                      // Finally, we can't do something like q"+:(${liftQuasi(quasi)}, (${liftTree(curr)}))",
                      // because would violate evaluation order guarantees that we must keep.
                      val currElement = liftTree(curr).asExprOf[T]
                      val alreadyLiftedList = acc.getOrElse(liftQuasi(quasi)).asExprOf[List[T]]
                      if (mode.isTerm) Some('{$currElement +: $alreadyLiftedList})//q"$currElement +: $alreadyLiftedList"
                      else Some('{$currElement +: $alreadyLiftedList})//pq"$currElement +: $alreadyLiftedList"
                    }),
                    Nil
                  )
              } else {
                // require(prefix.isEmpty && debug(trees, acc, prefix))
                if (mode.isTerm) then 
                  liftQuasi(quasi) match
                    case '{$expr: List[t]} =>
                      loop(rest, Some('{${acc.get.asExprOf[List[t]]} ++ ${expr}}), Nil)
                else report.errorAndAbort(Messages.QuasiquoteAdjacentEllipsesInPattern(quasi.rank))// TODO set position: quasi.pos
              }
            case other +: rest =>
              if (acc.isEmpty) loop(rest, acc, prefix :+ other)
              else {
                // require(prefix.isEmpty && debug(trees, acc, prefix))
                if (mode.isTerm) 
                  liftTree(other) match
                    case '{$expr: t} => loop(rest, Some('{${acc.get.asExprOf[List[t]]} :+ ${expr}}), Nil)
                else 
                  liftTree(other) match
                    case '{$expr: t} => loop(rest, Some('{${acc.get.asExprOf[List[t]]} :+ ${expr}}), Nil) //loop(rest, pq"$acc :+ ${liftTree(other)}", Nil)
              }
            case _ =>
              // NOTE: Luckily, at least this quasiquote works fine both in term and pattern modes
              if (acc.isEmpty) {
                val args = prefix.map(liftTree(_).asExprOf[T]).toList
                '{List[T](${Varargs(args)}: _*)}
              }
              else acc.get.asExprOf[List[Any]]
          }
        loop(trees, None, Nil)
      }
      //   def liftTreess(treess: List[List[MetaTree]]): ReflectTree = {
      //     val tripleDotQuasis = treess.flatten.collect {
      //       case quasi: Quasi if quasi.rank == 2 => quasi
      //     }
      //     if (tripleDotQuasis.isEmpty) {
      //       Liftable.liftList[List[MetaTree]](Liftables.liftableSubTrees).apply(treess)
      //     } else if (tripleDotQuasis.length == 1) {
      //       if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
      //       else
      //         c.abort(
      //           tripleDotQuasis(0).pos,
      //           "implementation restriction: can't mix ...$ with anything else in parameter lists." + EOL +
      //             "See https://github.com/scalameta/scalameta/issues/406 for details."
      //         )
      //     } else {
      //       c.abort(tripleDotQuasis(1).pos, Messages.QuasiquoteAdjacentEllipsesInPattern(2))
      //     }
      //   }
      def liftQuasi0(quasi: Quasi, optional: Boolean = false): Expr[Any] = {
        try {
          pendingQuasis.push(quasi)
          if (quasi.rank == 0) {
            val inferredPt = {
              val unwrappedPt = quasi.pt.wrap(pendingQuasis.map(_.rank).sum).toTpe
              if (optional) AppliedType(TypeRepr.of[Option], List(unwrappedPt)) else unwrappedPt
            }
            val lifted = mode match {
              case Mode.Term(_, _) =>
                inferredPt.asType match
                  case '[t] =>
                    '{scala.meta.internal.quasiquotes.Lift[t](${quasi.hole.arg.asExpr})}
              // case Mode.Pattern(_, _, _) =>
                // // NOTE: Here, we would like to say q"$InternalUnlift[$inferredPt](${quasi.hole.arg})".
                // // Unfortunately, the way how patterns work prevents us from having it this easy:
                // // 1) unapplications can't have explicitly specified type arguments
                // // 2) pattern language is very limited and can't express what we want to express in Unlift
                // // Therefore, we're forced to take a two-step unquoting scheme: a) match everything in the corresponding hole,
                // // b) call Unlift.unapply as a normal method in the right-hand side part of the pattern matching clause.
                // val hole = quasi.hole
                // val unliftedPt = hole.arg match {
                //   case pq"_: $explicitPt" => explicitPt
                //   case pq"$_: $explicitPt" => explicitPt
                //   case _ => TypeTree(inferredPt)
                // }
                // hole.reifier =
                //   atPos(quasi.pos)(q"$InternalUnlift.unapply[$unliftedPt](${hole.name})")
                // pq"${hole.name}"
            }
            // atPos(quasi.pos)(lifted)
            lifted
          } else {
            quasi.tree match {
              case quasi: Quasi if quasi.rank == 0 => liftQuasi(quasi)
              case _ => report.errorAndAbort("complex ellipses are not supported yet") //c.abort(quasi.pos, "complex ellipses are not supported yet")
            }
          }
        } finally {
          pendingQuasis.pop()
        }
      }

      // TODO
      // Depending on pattern types, this is able to cause some custom errors to be thrown
      // I do not think this is possible to port to scala 3.
      protected def unquotesName(q: scala.meta.internal.trees.Quasi): Boolean = {
        val tpe = q.hole.arg.tpe
        tpe != (null) && tpe <:< (TypeRepr.of[scala.meta.Term.Name])
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
    // private implicit class XtensionRankedTree(tree: quotes.reflect.TypeRepr) {
    //   def wrap(rank: Int): TypeRepr = {
    //     if (rank == 0) tree
    //     else AppliedType(ListClass, List(tree.wrap(rank - 1))) //AppliedTypeTree(tq"$ListClass", List(tree.wrap(rank - 1)))
    //   }
    // }
    implicit class XtensionQuasiHole(quasi: scala.meta.internal.trees.Quasi) {
      import quotes.reflect._
      def hole: Hole = {
        val pos = quasi.pos.absolutize // hmm
        val maybeHole =
          mode.holes.find(h => pos.start <= h.arg.pos.start && h.arg.pos.start <= pos.end)
        maybeHole.getOrElse(
          //unreachable(debug(quasi, quasi.pos.absolutize, mode.holes, mode.holes.map(_.arg.pos)))
          report.errorAndAbort("OOO")
        )
      }
    }

    mode match {
      case Mode.Term(_, _) =>
        val internalResult = Internal.liftTree(meta)
        if (sys.props("quasiquote.debug") != null) {
          println(internalResult)
          // println(showRaw(internalResult))
        }
        val resType =
          qType match
            case QuasiquoteType.Q => TypeRepr.of[scala.meta.Tree]
            case QuasiquoteType.T => TypeRepr.of[scala.meta.Type]
            case QuasiquoteType.P => TypeRepr.of[scala.meta.Tree]
        val a = resType.asType match
          case '[t] =>
            '{scala.meta.internal.quasiquotes.Unlift[scala.meta.Tree]($internalResult)} // replace scala.meta.Tree depending on StringContext prefix
        println(a.show) // TODO debug info
        a
      case Mode.Pattern(_, holes, unapplySelector) => ???
        // // inspired by https://github.com/densh/joyquote/blob/master/src/main/scala/JoyQuote.scala
        // val pattern = Internal.liftTree(meta)
        // val (thenp, elsep) = {
        //   if (holes.isEmpty) (q"true", q"false")
        //   else {
        //     val resultNames = holes.zipWithIndex.map({ case (_, i) =>
        //       TermName(QuasiquotePrefix + "$result$" + i)
        //     })
        //     val resultPatterns = resultNames.map(name => pq"_root_.scala.Some($name)")
        //     val resultTerms = resultNames.map(name => q"$name")
        //     val thenp = q"""
        //       (..${holes.map(_.reifier)}) match {
        //         case (..$resultPatterns) => _root_.scala.Some((..$resultTerms))
        //         case _ => _root_.scala.None
        //       }
        //     """
        //     (thenp, q"_root_.scala.None")
        //   }
        // }
        // val matchp = pattern match {
        //   case Bind(_, Ident(termNames.WILDCARD)) => q"input match { case $pattern => $thenp }"
        //   case _ => q"input match { case $pattern => $thenp; case _ => $elsep }"
        // }
        // val internalResult =
        //   q"new { def unapply(input: _root_.scala.meta.Tree) = $matchp }.unapply($unapplySelector)"
        // if (sys.props("quasiquote.debug") != null) {
        //   println(internalResult)
        //   // println(showRaw(internalResult))
        // }
        // internalResult
    }
  }
}
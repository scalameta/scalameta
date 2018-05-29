package scala.meta
package internal
package quasiquotes

import scala.runtime.ScalaRunTime
import scala.language.implicitConversions
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable
import org.scalameta._
import org.scalameta.adt.{Liftables => AdtLiftables}
import org.scalameta.invariants._
import scala.meta.dialects
import scala.meta.parsers._
import scala.meta.tokenizers._
import scala.meta.internal.trees._
import scala.meta.internal.trees.{Liftables => AstLiftables, Reflection => AstReflection}
import scala.meta.internal.parsers.Messages
import scala.meta.internal.parsers.Absolutize._
import scala.compat.Platform.EOL

class ReificationMacros(val c: Context) extends AstReflection with AdtLiftables with AstLiftables {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe.{Tree => _, Symbol => _, Type => _, Position => _, _}
  import c.universe.{Tree => ReflectTree, Symbol => ReflectSymbol, Position => ReflectPosition}
  import scala.meta.{Tree => MetaTree, Dialect => Dialect}
  import scala.meta.inputs.{Position => MetaPosition, _}
  type MetaParser = (Input, Dialect) => MetaTree
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
  val XtensionParsersDialectApply = "shadow extension method conflict"

  // NOTE: only Mode.Pattern really needs holes, and that's only because of Scala's limitations
  // read a comment in liftUnquote for more information on that
  case class Hole(name: TermName, arg: ReflectTree, var reifier: ReflectTree)
  sealed trait Mode {
    def isTerm: Boolean = this.isInstanceOf[Mode.Term]
    def isPattern: Boolean = this.isInstanceOf[Mode.Pattern]
    def multiline: Boolean
    def holes: List[Hole]
  }
  object Mode {
    case class Term(multiline: Boolean, holes: List[Hole]) extends Mode
    case class Pattern(multiline: Boolean, holes: List[Hole], unapplySelector: ReflectTree) extends Mode
  }

  import definitions._
  val InternalLift = c.mirror.staticModule("scala.meta.internal.quasiquotes.Lift")
  val InternalUnlift = c.mirror.staticModule("scala.meta.internal.quasiquotes.Unlift")
  val QuasiquotePrefix = c.freshName("quasiquote")

  def apply(args: ReflectTree*)(dialect: ReflectTree): ReflectTree = expand(dialect)
  def unapply(scrutinee: ReflectTree)(dialect: ReflectTree): ReflectTree = expand(dialect)
  def expand(dialectTree: ReflectTree): ReflectTree = {
    val (input, mode) = extractQuasiquotee()
    val dialect = instantiateDialect(dialectTree, mode)
    val parser = instantiateParser(c.macroApplication.symbol)
    val skeleton = parseSkeleton(parser, input, dialect)
    reifySkeleton(skeleton, mode)
  }

  private def extractQuasiquotee(): (Input, Mode) = {
    val reflectInput = c.macroApplication.pos.source
    val (parts, mode) = {
      def isMultiline(firstPart: ReflectTree) = {
        reflectInput.content(firstPart.pos.start - 2) == '"'
      }
      def mkHole(argi: (ReflectTree, Int)) = {
        val (arg, i) = argi
        val name = TermName(QuasiquotePrefix + "$hole$" + i)
        Hole(name, arg, reifier = EmptyTree)
      }
      try {
        c.macroApplication match {
          case q"$_($_.apply(..$parts)).$_.apply[..$_](..$args)($_)" =>
            require(parts.length == args.length + 1)
            val holes = args.zipWithIndex.map(mkHole)
            (parts, Mode.Term(isMultiline(parts.head), holes))
          case q"$_($_.apply(..$parts)).$_.unapply[..$_](${unapplySelector: Ident})($_)" =>
            require(unapplySelector.name == TermName("<unapply-selector>"))
            val args = c.internal.subpatterns(unapplySelector).get
            require(parts.length == args.length + 1)
            val holes = args.zipWithIndex.map(mkHole)
            (parts, Mode.Pattern(isMultiline(parts.head), holes, unapplySelector))
        }
      } catch {
        case ex: Exception =>
          c.abort(c.macroApplication.pos, s"fatal error initializing quasiquote macro: ${showRaw(c.macroApplication)}")
      }
    }
    val metaInput = {
      val (firstPart, lastPart @ Literal(Constant(s_lastpart: String))) = (parts.head, parts.last)
      val start = firstPart.pos.start // looks like we can trust this position to point to the character right after the opening quotes
      val end = { // we have to infer this for ourselves, because there's no guarantee we run under -Yrangepos
        var remaining = s_lastpart.length
        var curr = lastPart.pos.start - 1
        while (remaining > 0) {
          curr += 1
          if (reflectInput.content(curr) == '$') {
            curr += 1
            require(reflectInput.content(curr) == '$')
          }
          remaining -= 1
        }
        curr + 1
      }
      val metaInput = Input.VirtualFile(reflectInput.path, new String(reflectInput.content))
      Input.Slice(metaInput, start, end)
    }
    (metaInput, mode)
  }

  private implicit def metaPositionToReflectPosition(pos: MetaPosition): ReflectPosition = {
    // WONTFIX: https://github.com/scalameta/scalameta/issues/383
    c.macroApplication.pos.focus.withPoint(pos.absolutize.start)
  }

  private def instantiateDialect(dialectTree: ReflectTree, mode: Mode): Dialect = {
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
      def instantiateStandardDialect(sym: ReflectSymbol): Option[Dialect] = {
        val dialectsSym = c.mirror.staticModule("scala.meta.dialects.package").moduleClass
        if (dialectsSym != sym.owner) return None
        if (dialectsSym.info.member(sym.name) == NoSymbol) return None
        Dialect.standards.get(sym.name.toString)
      }
      val standardDialectReference = instantiateStandardDialect(dialectTree.symbol) // allow `scala.meta.dialects.Scala211`
      val standardDialectSingleton = instantiateStandardDialect(dialectTree.tpe.termSymbol) // allow `scala.meta.Dialect.current`
      standardDialectReference.orElse(standardDialectSingleton).getOrElse({
        val suggestion = s"to fix this, import something from scala.meta.dialects, e.g. scala.meta.dialects.${Dialect.current}"
        val message = s"$dialectTree of type ${dialectTree.tpe} is not supported by quasiquotes ($suggestion)"
        c.abort(c.enclosingPosition, message)
      })
    }
    if (mode.isTerm) dialects.QuasiquoteTerm(underlyingDialect, mode.multiline)
    else dialects.QuasiquotePat(underlyingDialect, mode.multiline)
  }

  private def instantiateParser(interpolator: ReflectSymbol): MetaParser = {
    val parserModule = interpolator.owner.owner.companion
    val parsersModuleClass = Class.forName("scala.meta.quasiquotes.package$", true, this.getClass.getClassLoader)
    val parsersModule = parsersModuleClass.getField("MODULE$").get(null)
    val parserModuleGetter = parsersModule.getClass.getDeclaredMethod(parserModule.name.toString)
    val parserModuleInstance = parserModuleGetter.invoke(parsersModule)
    val parserMethod = parserModuleInstance.getClass.getDeclaredMethods().find(_.getName == "parse").head
    (input: Input, dialect: Dialect) => {
      try parserMethod.invoke(parserModuleInstance, input, dialect).asInstanceOf[MetaTree]
      catch { case ex: java.lang.reflect.InvocationTargetException => throw ex.getTargetException }
    }
  }

  private def parseSkeleton(parser: MetaParser, input: Input, dialect: Dialect): MetaTree = {
    try {
      parser(input, dialect)
    } catch {
      case TokenizeException(pos, message) => c.abort(pos, message)
      case ParseException(pos, message) => c.abort(pos, message)
    }
  }

  private def reifySkeleton(meta: MetaTree, mode: Mode): ReflectTree = {
    val pendingQuasis = mutable.Stack[Quasi]()
    implicit class XtensionRankedClazz(clazz: Class[_]) {
      def unwrap: Class[_] = {
        if (clazz.isArray) clazz.getComponentType.unwrap
        else clazz
      }
      def wrap(rank: Int): Class[_] = {
        if (rank == 0) clazz
        else ScalaRunTime.arrayClass(clazz).wrap(rank - 1)
      }
      def toTpe: u.Type = {
        if (clazz.isArray) {
          appliedType(ListClass, clazz.getComponentType.toTpe)
        } else {
          def loop(owner: u.Symbol, parts: List[String]): u.Symbol = parts match {
            case part :: Nil =>
              if (clazz.getName.endsWith("$")) owner.info.decl(TermName(part))
              else owner.info.decl(TypeName(part))
            case part :: rest =>
              loop(owner.info.decl(TermName(part)), rest)
            case Nil =>
              unreachable(debug(clazz))
          }
          val name = scala.reflect.NameTransformer.decode(clazz.getName)
          val result = loop(mirror.RootPackage, name.stripSuffix("$").split(Array('.', '$')).toList)
          if (result.isModule) result.asModule.info else result.asClass.toType
        }
      }
    }
    implicit class XtensionRankedTree(tree: u.Tree) {
      def wrap(rank: Int): u.Tree = {
        if (rank == 0) tree
        else AppliedTypeTree(tq"$ListClass", List(tree.wrap(rank - 1)))
      }
    }
    implicit class XtensionQuasiHole(quasi: Quasi) {
      def hole: Hole = {
        val pos = quasi.pos.absolutize
        val maybeHole = mode.holes.find(h => pos.start <= h.arg.pos.point && h.arg.pos.point <= pos.end)
        maybeHole.getOrElse(unreachable(debug(quasi, quasi.pos.absolutize, mode.holes, mode.holes.map(_.arg.pos))))
      }
    }
    object Lifts {
      def liftTree(tree: MetaTree): ReflectTree = {
        Liftables.liftableSubTree(tree)
      }
      def liftOptionTree(maybeTree: Option[MetaTree]): ReflectTree = {
        maybeTree match {
          case Some(tree: Quasi) => liftQuasi(tree, optional = true)
          case Some(otherTree) => q"_root_.scala.Some(${liftTree(otherTree)})"
          case None => q"_root_.scala.None"
        }
      }
      def liftTrees(trees: List[MetaTree]): ReflectTree = {
        def loop(trees: List[MetaTree], acc: ReflectTree, prefix: List[MetaTree]): ReflectTree = trees match {
          case (quasi: Quasi) +: rest if quasi.rank == 1 =>
            if (acc.isEmpty) {
              if (prefix.isEmpty) loop(rest, liftQuasi(quasi), Nil)
              else loop(rest, prefix.foldRight(acc)((curr, acc) => {
                // NOTE: We cannot do just q"${liftTree(curr)} +: ${liftQuasi(quasi)}"
                // because that creates a synthetic temp variable that doesn't make any sense in a pattern.
                // Neither can we do q"${liftQuasi(quasi)}.+:(${liftTree(curr)})",
                // because that still wouldn't work in pattern mode.
                // Finally, we can't do something like q"+:(${liftQuasi(quasi)}, (${liftTree(curr)}))",
                // because would violate evaluation order guarantees that we must keep.
                val currElement = liftTree(curr)
                val alreadyLiftedList = acc.orElse(liftQuasi(quasi))
                if (mode.isTerm) q"$currElement +: $alreadyLiftedList"
                else pq"$currElement +: $alreadyLiftedList"
              }), Nil)
            } else {
              require(prefix.isEmpty && debug(trees, acc, prefix))
              if (mode.isTerm) loop(rest, q"$acc ++ ${liftQuasi(quasi)}", Nil)
              else c.abort(quasi.pos, Messages.QuasiquoteAdjacentEllipsesInPattern(quasi.rank))
            }
          case other +: rest =>
            if (acc.isEmpty) loop(rest, acc, prefix :+ other)
            else {
              require(prefix.isEmpty && debug(trees, acc, prefix))
              if (mode.isTerm) loop(rest, q"$acc :+ ${liftTree(other)}", Nil)
              else loop(rest, pq"$acc :+ ${liftTree(other)}", Nil)
            }
          case Nil =>
            // NOTE: Luckily, at least this quasiquote works fine both in term and pattern modes
            if (acc.isEmpty) q"$ListModule(..${prefix.map(liftTree)})"
            else acc
        }
        loop(trees.toList, EmptyTree, Nil)
      }
      def liftTreess(treess: List[List[MetaTree]]): ReflectTree = {
        val tripleDotQuasis = treess.flatten.collect{ case quasi: Quasi if quasi.rank == 2 => quasi }
        if (tripleDotQuasis.length == 0) {
          Liftable.liftList[List[MetaTree]](Liftables.liftableSubTrees).apply(treess.toList)
        } else if (tripleDotQuasis.length == 1) {
          if (treess.flatten.length == 1) liftQuasi(tripleDotQuasis(0))
          else c.abort(tripleDotQuasis(0).pos,
            "implementation restriction: can't mix ...$ with anything else in parameter lists." + EOL +
            "See https://github.com/scalameta/scalameta/issues/406 for details.")
        } else {
          c.abort(tripleDotQuasis(1).pos, Messages.QuasiquoteAdjacentEllipsesInPattern(2))
        }
      }
      def liftQuasi(quasi: Quasi, optional: Boolean = false): ReflectTree = {
        try {
          pendingQuasis.push(quasi)
          if (quasi.rank == 0) {
            val inferredPt = {
              val unwrappedPt = quasi.pt.wrap(pendingQuasis.map(_.rank).sum).toTpe
              if (optional) appliedType(definitions.OptionClass, unwrappedPt) else unwrappedPt
            }
            val lifted = mode match {
              case Mode.Term(_, _) =>
                val liftedPt = inferredPt
                q"$InternalLift[$liftedPt](${quasi.hole.arg})"
              case Mode.Pattern(_, _, _) =>
                // NOTE: Here, we would like to say q"$InternalUnlift[$inferredPt](${quasi.hole.arg})".
                // Unfortunately, the way how patterns work prevents us from having it this easy:
                // 1) unapplications can't have explicitly specified type arguments
                // 2) pattern language is very limited and can't express what we want to express in Unlift
                // Therefore, we're forced to take a two-step unquoting scheme: a) match everything in the corresponding hole,
                // b) call Unlift.unapply as a normal method in the right-hand side part of the pattern matching clause.
                val hole = quasi.hole
                val unliftedPt = hole.arg match {
                  case pq"_: $explicitPt" => explicitPt
                  case pq"$_: $explicitPt" => explicitPt
                  case _ => TypeTree(inferredPt)
                }
                hole.reifier = atPos(quasi.pos)(q"$InternalUnlift.unapply[$unliftedPt](${hole.name})")
                pq"${hole.name}"
            }
            atPos(quasi.pos)(lifted)
          } else {
            quasi.tree match {
              case quasi: Quasi if quasi.rank == 0 => liftQuasi(quasi)
              case _ => c.abort(quasi.pos, "complex ellipses are not supported yet")
            }
          }
        } finally {
          pendingQuasis.pop()
        }
      }
    }
    object Liftables {
      // NOTE: we could write just `implicitly[Liftable[MetaTree]].apply(meta)`
      // but that would bloat the code significantly with duplicated instances for denotations and sigmas
      implicit def liftableSubTree[T <: MetaTree]: Liftable[T] = Liftable((tree: T) => materializeAst[MetaTree].apply(tree))
      implicit def liftableSubTrees[T <:MetaTree]: Liftable[List[T]] = Liftable((trees: List[T]) => Lifts.liftTrees(trees))
      implicit def liftableSubTreess[T <: MetaTree]: Liftable[List[List[T]]] = Liftable((treess: List[List[T]]) => Lifts.liftTreess(treess))
      implicit def liftableOptionSubTree[T <: MetaTree]: Liftable[Option[T]] = Liftable((x: Option[T]) => Lifts.liftOptionTree(x))
    }
    mode match {
      case Mode.Term(_, _) =>
        val internalResult = Lifts.liftTree(meta)
        if (sys.props("quasiquote.debug") != null) {
          println(internalResult)
          // println(showRaw(internalResult))
        }
        q"$InternalUnlift[${c.macroApplication.tpe}]($internalResult)"
      case Mode.Pattern(_, holes, unapplySelector) =>
        // inspired by https://github.com/densh/joyquote/blob/master/src/main/scala/JoyQuote.scala
        val pattern = Lifts.liftTree(meta)
        val (thenp, elsep) = {
          if (holes.isEmpty) (q"true", q"false")
          else {
            val resultNames = holes.zipWithIndex.map({ case (_, i) => TermName(QuasiquotePrefix + "$result$" + i) })
            val resultPatterns = resultNames.map(name => pq"_root_.scala.Some($name)")
            val resultTerms = resultNames.map(name => q"$name")
            val thenp = q"""
              (..${holes.map(_.reifier)}) match {
                case (..$resultPatterns) => _root_.scala.Some((..$resultTerms))
                case _ => _root_.scala.None
              }
            """
            (thenp, q"_root_.scala.None")
          }
        }
        val matchp = pattern match {
          case Bind(_, Ident(termNames.WILDCARD)) => q"input match { case $pattern => $thenp }"
          case _ => q"input match { case $pattern => $thenp; case _ => $elsep }"
        }
        val internalResult = q"new { def unapply(input: _root_.scala.meta.Tree) = $matchp }.unapply($unapplySelector)"
        if (sys.props("quasiquote.debug") != null) {
          println(internalResult)
          // println(showRaw(internalResult))
        }
        internalResult
    }
  }
}

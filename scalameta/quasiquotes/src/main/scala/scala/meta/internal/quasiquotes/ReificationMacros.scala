package scala.meta
package internal
package quasiquotes

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.runtime.ScalaRunTime
import scala.language.implicitConversions
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.collection.{immutable, mutable}
import org.scalameta.data._
import org.scalameta.adt.{Liftables => AdtLiftables, Reflection => AdtReflection}
import scala.meta.internal.ast.{Liftables => AstLiftables, Reflection => AstReflection}
import org.scalameta._
import org.scalameta.invariants._
import scala.meta.dialects // no underscore import
import scala.meta.classifiers._
import scala.meta.parsers._
import scala.meta.tokenizers._
import scala.meta.prettyprinters._
import scala.meta.internal.dialects.InstantiateDialect
import scala.meta.internal.{semantic => s}
import scala.meta.internal.ast.Quasi
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.parsers.Messages
import scala.meta.internal.parsers.Absolutize._
import scala.meta.internal.tokens._
import scala.compat.Platform.EOL

// TODO: ideally, we would like to bootstrap these macros on top of scala.meta
// so that quasiquotes can be interpreted by any host, not just scalac
class ReificationMacros(val c: Context) extends AstReflection with AdtLiftables with AstLiftables with InstantiateDialect {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.internal._
  import decorators._
  import c.universe.{Tree => _, Symbol => _, Type => _, Position => _, _}
  import c.universe.{Tree => ReflectTree, Symbol => ReflectSymbol, Type => ReflectType, Position => ReflectPosition}
  import scala.meta.{Tree => MetaTree, Type => MetaType, Dialect => Dialect}
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
  val SeqModule = c.mirror.staticModule("scala.collection.Seq")
  val ScalaPackageObjectClass = c.mirror.staticModule("scala.package")
  val ScalaList = ScalaPackageObjectClass.info.decl(TermName("List"))
  val ScalaNil = ScalaPackageObjectClass.info.decl(TermName("Nil"))
  val ScalaSeq = ScalaPackageObjectClass.info.decl(TermName("Seq"))
  val ImmutableSeq = mirror.staticClass("scala.collection.immutable.Seq")
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
    val hygienicSkeleton = hygienifySkeleton(skeleton)
    reifySkeleton(hygienicSkeleton, mode)
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
        Hole(name, arg, reifier = EmptyTree) // TODO: make reifier immutable somehow
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
      val result = {
        if (reflectInput.file.file != null) Input.File(reflectInput.file.file)
        else Input.String(new String(reflectInput.content)) // NOTE: can happen in REPL or in custom Global
      }
      Input.Slice(result, start, end)
    }
    (metaInput, mode)
  }

  private implicit def metaPositionToReflectPosition(pos: MetaPosition): ReflectPosition = {
    // TODO: this is another instance of #383
    c.macroApplication.pos.focus.withPoint(pos.start.absolutize.offset)
  }

  private def instantiateDialect(dialectTree: ReflectTree, mode: Mode): Dialect = {
    val underlyingDialect = instantiateDialect(dialectTree)
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

  private def hygienifySkeleton(meta: MetaTree): MetaTree = {
    // TODO: Implement this (https://github.com/scalameta/scalameta/issues/156)
    // by setting Tree.env of appropriate trees (names, apply-like nodes) to appropriate values.
    // So far, we don't have to set anything to anything,
    // because Environment.None (the only possible value for environments at the moment) is the default.
    meta
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
          appliedType(ImmutableSeq, clazz.getComponentType.toTpe)
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
        else AppliedTypeTree(tq"$ImmutableSeq", List(tree.wrap(rank - 1)))
      }
    }
    implicit class XtensionQuasiHole(quasi: Quasi) {
      def hole: Hole = {
        val pos = quasi.pos.absolutize
        val maybeHole = mode.holes.find(h => pos.start.offset <= h.arg.pos.point && h.arg.pos.point <= pos.end.offset)
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
      def liftTrees(trees: Seq[MetaTree]): ReflectTree = {
        def loop(trees: List[MetaTree], acc: ReflectTree, prefix: List[MetaTree]): ReflectTree = trees match {
          // TODO: instead of checking against 1, we should also take pendingQuasis into account
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
                if (mode.isTerm) q"${liftTree(curr)} +: ${liftQuasi(quasi)}"
                else pq"${liftTree(curr)} +: ${liftQuasi(quasi)}"
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
            if (acc.isEmpty) q"_root_.scala.collection.immutable.Seq(..${prefix.map(liftTree)})"
            else acc
        }
        loop(trees.toList, EmptyTree, Nil)
      }
      def liftOptionTrees(maybeTrees: Option[Seq[MetaTree]]): ReflectTree = {
        maybeTrees match {
          case Some(Seq(quasi: Quasi)) if quasi.rank == 0 =>
            q"_root_.scala.Some(${liftTrees(Seq(quasi))})"
          case Some(Seq(quasi: Quasi)) if quasi.rank > 0 =>
            liftQuasi(quasi, optional = true)
          case Some(otherTrees) =>
            q"_root_.scala.Some(${liftTrees(otherTrees)})"
          case None =>
            q"_root_.scala.None"
        }
      }
      def liftTreess(treess: Seq[Seq[MetaTree]]): ReflectTree = {
        val tripleDotQuasis = treess.flatten.collect{ case quasi: Quasi if quasi.rank == 2 => quasi }
        if (tripleDotQuasis.length == 0) {
          Liftable.liftList[Seq[MetaTree]](Liftables.liftableSubTrees).apply(treess.toList)
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
            var inferredPt = quasi.pt.wrap(pendingQuasis.map(_.rank).sum).toTpe
            if (optional) inferredPt = appliedType(typeOf[Option[_]], inferredPt)
            val lifted = mode match {
              case Mode.Term(_, _) =>
                q"$InternalLift[$inferredPt](${quasi.hole.arg})"
              case Mode.Pattern(_, _, _) =>
                // TODO: Here, we would like to say q"$InternalUnlift[$inferredPt](${quasi.hole.arg})".
                // Unfortunately, the way how patterns work prevents us from having it this easy:
                // 1) unapplications can't have explicitly specified type arguments
                // 2) pattern language is very limited and can't express what we want to express in Unlift
                // Therefore, we're forced to take a two-step unquoting scheme: a) match everything in the corresponding hole,
                // b) call Unlift.unapply as a normal method in the right-hand side part of the pattern matching clause.
                val hole = quasi.hole
                val inferredPt = hole.arg match {
                  case pq"_: $pt" => pt
                  case pq"$_: $pt" => pt
                  case _ =>
                    var inferredPt = tq"_root_.scala.meta.Tree".wrap(pendingQuasis.map(_.rank).sum)
                    if (optional) inferredPt = tq"_root_.scala.Option[$inferredPt]"
                    inferredPt
                }
                hole.reifier = atPos(quasi.pos)(q"$InternalUnlift.unapply[$inferredPt](${hole.name})") // TODO: make `reifier`a immutable somehow
                pq"${hole.name}"
            }
            atPos(quasi.pos)(lifted)
          } else {
            quasi.tree match {
              case quasi: Quasi if quasi.rank == 0 =>
                // NOTE: Option[Seq[T]] exists only in one instance in our tree API: Template.stats.
                // Unfortunately, there is a semantic difference between an empty template and a template
                // without any stats, so we can't just replace it with Seq[T] and rely on tokens
                // (because tokens may be lost, e.g. a TASTY roundtrip).
                //
                // Strictly speaking, we shouldn't be allowing ..$stats in Template.stats,
                // because the pt is Option[Seq[T]], not a Seq[T], but given the fact that Template.stats
                // is very common, I'm willing to bend the rules here and flatten Option[Seq[T]],
                // capturing 0 trees if we have None and working as usual if we have Some.
                //
                // This is definitely loss of information, but if someone wants to discern Some and None,
                // they are welcome to write $stats, which will correctly capture an Option.
                // TODO: Well, they aren't welcome, because $stats will insert or capture just one stat.
                // This is the same problem as with `q"new $x"`, where x is ambiguous between a template and a ctorcall.
                if (optional) {
                  mode match {
                    case Mode.Term(_, _) =>
                      q"_root_.scala.Some(${liftQuasi(quasi)})"
                    case Mode.Pattern(_, holes, _) =>
                      val reified = liftQuasi(quasi)
                      val hole = holes.find(hole => reified.equalsStructure(pq"${hole.name}")).get
                      require(hole.reifier.nonEmpty)
                      val flattened = q"_root_.scala.meta.internal.quasiquotes.Flatten.unapply(${hole.name})"
                      val unlifted = (new Transformer {
                        override def transform(tree: ReflectTree): ReflectTree = tree match {
                          case Ident(name) if name == hole.name => Ident(TermName("tree"))
                          case tree => super.transform(tree)
                        }
                      }).transform(hole.reifier)
                      hole.reifier = atPos(quasi.pos)(q"$flattened.flatMap(tree => $unlifted)")
                      reified
                  }
                } else {
                  liftQuasi(quasi)
                }
              case _ =>
                c.abort(quasi.pos, "complex ellipses are not supported yet")
            }
          }
        } finally {
          pendingQuasis.pop()
        }
      }
    }
    object Liftables extends s.DenotationLiftables
                        with s.TypingLiftables
                        with s.EnvironmentLiftables {
      // NOTE: we could write just `implicitly[Liftable[MetaTree]].apply(meta)`
      // but that would bloat the code significantly with duplicated instances for denotations and sigmas
      override lazy val u: c.universe.type = c.universe
      implicit def liftableSubTree[T <: MetaTree]: Liftable[T] = Liftable((tree: T) => materializeAst[MetaTree].apply(tree))
      implicit def liftableSubTrees[T <:MetaTree]: Liftable[Seq[T]] = Liftable((trees: Seq[T]) => Lifts.liftTrees(trees))
      implicit def liftableSubTreess[T <: MetaTree]: Liftable[Seq[Seq[T]]] = Liftable((treess: Seq[Seq[T]]) => Lifts.liftTreess(treess))
      implicit def liftableOptionSubTree[T <: MetaTree]: Liftable[Option[T]] = Liftable((x: Option[T]) => Lifts.liftOptionTree(x))
      implicit def liftableOptionSubTrees[T <: MetaTree]: Liftable[Option[Seq[T]]] = Liftable((x: Option[Seq[T]]) => Lifts.liftOptionTrees(x))
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
        internalResult // TODO: q"InternalLift[${unapplySelector.tpe}]($internalResult)"
    }
  }
}

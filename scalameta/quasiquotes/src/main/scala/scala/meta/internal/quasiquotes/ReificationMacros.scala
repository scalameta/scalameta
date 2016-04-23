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
import scala.meta.parsers._
import scala.meta.tokenizers._
import scala.meta.prettyprinters._
import scala.meta.internal.dialects.InstantiateDialect
import scala.meta.internal.{semantic => s}
import scala.meta.internal.ast.Quasi
import scala.meta.internal.ast.Helpers._
import scala.compat.Platform.EOL

// TODO: ideally, we would like to bootstrap these macros on top of scala.meta
// so that quasiquotes can be interpreted by any host, not just scalac
private[meta] class ReificationMacros(val c: Context)
extends AstReflection with AdtLiftables with AstLiftables with InstantiateDialect {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.internal._
  import decorators._
  import c.universe.{Tree => _, Symbol => _, Type => _, Position => _, _}
  import c.universe.{Tree => ReflectTree, Symbol => ReflectSymbol, Type => ReflectType, Position => ReflectPosition, Bind => ReflectBind}
  import scala.meta.{Tree => MetaTree, Type => MetaType, Dialect => MetaDialect}
  import scala.meta.inputs.{Input => MetaInput, Content => MetaContent, Position => MetaPosition}
  import scala.meta.tokens.{Token => MetaToken, Tokens => MetaTokens}
  import scala.meta.Term.{Name => MetaTermName}
  type MetaParser = (MetaInput, MetaDialect) => MetaTree
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  // NOTE: only Mode.Pattern needs holes, and that's only because of Scala's limitations
  // read a comment in liftUnquote for more information on that
  case class Hole(name: TermName, arg: ReflectTree, var reifier: ReflectTree)
  sealed trait Mode { def isTerm = this == Mode.Term; def isPattern = this.isInstanceOf[Mode.Pattern] }
  object Mode {
    object Term extends Mode { def holes = Nil }
    case class Pattern(dummy: ReflectTree, holes: List[Hole]) extends Mode
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
  def expand(dialect: ReflectTree): ReflectTree = {
    val (skeleton, mode) = parseSkeleton(instantiateDialect(dialect), instantiateParser(c.macroApplication.symbol))
    val hygienicSkeleton = hygienifySkeleton(skeleton)
    reifySkeleton(hygienicSkeleton, mode)
  }

  private def instantiateParser(interpolator: ReflectSymbol): MetaParser = {
    val parserModule = interpolator.owner.owner.companion
    val parsersModule = Class.forName("scala.meta.quasiquotes.package", true, this.getClass.getClassLoader)
    val parserModuleGetter = parsersModule.getDeclaredMethod(parserModule.name.toString)
    val parserModuleInstance = parserModuleGetter.invoke(null)
    val parserMethod = parserModuleInstance.getClass.getDeclaredMethods().find(_.getName == "parse").head
    (input: MetaInput, dialect: MetaDialect) => {
      try parserMethod.invoke(parserModuleInstance, input, dialect).asInstanceOf[MetaTree]
      catch { case ex: java.lang.reflect.InvocationTargetException => throw ex.getTargetException }
    }
  }

  private def parseSkeleton(metaDialect: MetaDialect, metaParse: MetaParser): (MetaTree, Mode) = {
    val (parts, args, mode) = {
      try {
        c.macroApplication match {
          case q"$_($_.apply(..$parts)).$_.apply[..$_](..$args)($_)" =>
            require(parts.length == args.length + 1)
            (parts, args, Mode.Term)
          case q"$_($_.apply(..$parts)).$_.unapply[..$_](${dummy: Ident})($_)" =>
            require(dummy.name == TermName("<unapply-selector>"))
            val args = c.internal.subpatterns(dummy).get
            require(parts.length == args.length + 1)
            val holes = args.zipWithIndex.map({ case (arg, i) =>
              val name = TermName(QuasiquotePrefix + "$hole$" + i)
              Hole(name, arg, reifier = EmptyTree) // TODO: make reifier immutable somehow
            })
            (parts, holes.map(hole => pq"${hole.name}"), Mode.Pattern(dummy, holes))
        }
      } catch {
        case ex: Exception =>
          log(println(ex.toString))
          c.abort(c.macroApplication.pos, s"fatal error initializing quasiquote macro: ${showRaw(c.macroApplication)}")
      }
    }
    val parttokenss: List[MetaTokens] = parts.zip(args :+ EmptyTree).map{
      case (partlit @ q"${part: String}", arg) =>
        // Step 1: Compute start and end of the part.
        // Also provide facilities to map offsets between part and wholeFileSource.
        // The mapping is not Predef.identity because of $$ (explained below).
        val dollars = mutable.ListBuffer[Int]()
        val start = partlit.pos.start
        var remaining = part.length
        var end = start - 1
        while (remaining > 0) {
          end += 1
          if (wholeFileSource.content(end) == '$') {
            dollars += end
            end += 1
            require(wholeFileSource.content(end) == '$')
          }
          remaining -= 1
        }
        // NOTE: Here's an example of a translation for q"a+$$b+$$c".
        // This is how the [start..end] fragment of `wholeFileSource` is going to look like:
        //     2345678910
        //     a+$$b+$$c
        // This is what we'll see in `part`:
        //     0123456
        //     a+$b+$c
        // Let's translate a part offset 6 (i.e. the position of `c`) to a source offset.
        // `dollars` are going to be ListBuffer(4, 8).
        // First we add `start` to the part offset, obtaining 8.
        // Next we check whether 8 is greater than the 0th dollar, i.e. 4.
        // Yes, it is, so we increment it (to account for doubling of the dollar) and recur.
        // Now we check whether 9 is greater than the 1st dollar, i.e. 8.
        // Yes, it is, so we increment it (to account for doubling of the dollar) and recur.
        // Now we check whether 10 is greater than the 2nd dollar, but it doesn't exist, so we return.
        def partOffsetToSourceOffset(partOffset: Int): Int = {
          def loop(offset: Int, dollarIndex: Int): Int = {
            if (dollarIndex >= dollars.length || offset < dollars(dollarIndex)) offset
            else loop(offset + 1, dollarIndex + 1)
          }
          loop(partOffset + start, 0)
        }

        // Step 2: Tokenize the part translating tokenization exceptions.
        // Output tokens will be bound to a synthetic Input.String, which isn't yet correlated with `wholeFileSource`.
        def failUnclosed(what: String): Nothing = {
          // NOTE: arg.pos is not precise enough
          // val unquotePosition = arg.pos
          val unquotePosition = partlit.pos.focus.withPoint(end + 1)
          c.abort(unquotePosition, "can't unquote into " + what + "s")
        }
        val crudeTokens = {
          implicit val tokenizationDialect: MetaDialect = scala.meta.dialects.Quasiquote(metaDialect)
          try {
            val tokens = part.tokenize.get
            if (tokens.init.last.isInstanceOf[MetaToken.Comment] && arg.nonEmpty) {
              failUnclosed("single-line comment")
            }
            tokens
          } catch {
            case TokenizeException(partPos, message) =>
              if (message.startsWith("unclosed ") && arg.nonEmpty) {
                var what = message.stripPrefix("unclosed ")
                if (what == "comment") what = "multi-line comment"
                failUnclosed(what)
              } else {
                val sourceOffset = partOffsetToSourceOffset(partPos.start.offset)
                val sourcePosition = partlit.pos.focus.withPoint(sourceOffset)
                c.abort(sourcePosition, message)
              }
          }
        }

        // Step 3: Prettify the tokens by relating them to `wholeFileSource`.
        // To do that, we create InputSlice over the `wholeFileSource` and fixup positions to account for $$s.
        crudeTokens.map(crudeToken => {
          val delta = partOffsetToSourceOffset(crudeToken.start) - (start + crudeToken.start)
          crudeToken.adjust(content = sliceFileContent(start, end + 1), delta = delta)
        })
      case (part, arg) =>
        c.abort(part.pos, "quasiquotes can only be used with literal strings")
    }
    def merge(index: Int, parttokens: MetaTokens, arg: ReflectTree): MetaTokens = {
      implicit class RichMetaToken(token: MetaToken) { def absoluteStart = token.start + token.content.require[MetaInput.Slice].from }
      val part: MetaTokens = {
        val bof +: payload :+ eof = parttokens
        require(bof.isInstanceOf[MetaToken.BOF] && eof.isInstanceOf[MetaToken.EOF] && debug(parttokens))
        val prefix = if (index == 0) MetaTokens(bof) else MetaTokens()
        val suffix = if (index == parttokenss.length - 1) MetaTokens(eof) else MetaTokens()
        prefix ++ payload ++ suffix
      }
      val unquote: MetaTokens = {
        if (arg.isEmpty) {
          MetaTokens()
        } else {
          val unquoteStart = parttokens.last.absoluteStart
          val unquoteEnd = parttokenss(index + 1).head.absoluteStart - 1
          val unquoteContent = sliceFileContent(unquoteStart, unquoteEnd + 1)
          val unquoteDialect = scala.meta.dialects.Quasiquote(metaDialect)
          MetaTokens(MetaToken.Unquote(unquoteContent, unquoteDialect, 0, unquoteEnd - unquoteStart + 1, arg))
        }
      }
      part ++ unquote
    }
    val tokens: MetaTokens = MetaTokens(parttokenss.zip(args :+ EmptyTree).zipWithIndex.flatMap({ case ((ts, a), i) => merge(i, ts, a) }): _*)
    log(println(tokens))
    try {
      implicit val parsingDialect: MetaDialect = scala.meta.dialects.Quasiquote(metaDialect)
      log({ println(tokens); println(parsingDialect) })
      val syntax = metaParse(tokens, parsingDialect)
      log({ println(syntax.show[Syntax]); println(syntax.show[Structure]) })
      (syntax, mode)
    } catch {
      case ParseException(position, message) => c.abort(position, message)
    }
  }

  private lazy val wholeFileSource = c.macroApplication.pos.source
  private lazy val wholeFileContent = {
    if (wholeFileSource.file.file != null) MetaInput.File(wholeFileSource.file.file)
    else MetaInput.String(new String(wholeFileSource.content)) // NOTE: can happen in REPL or in custom Global
  }
  private def sliceFileContent(from: Int, until: Int) = MetaInput.Slice(wholeFileContent, from, until)
  implicit def metaPositionToReflectPosition(pos: MetaPosition): ReflectPosition = {
    val (content, contentStart) = pos.content match {
      case MetaInput.Slice(content, contentStart, _) => (content, contentStart)
      case content => (content, 0)
    }
    require(content == wholeFileContent && debug(pos, content, wholeFileContent))
    c.macroApplication.pos.focus.withPoint(contentStart + pos.point.offset)
  }

  private def hygienifySkeleton(meta: MetaTree): MetaTree = {
    // TODO: Implement this (https://github.com/scalameta/scalameta/issues/156)
    // by setting Tree.env of appropriate trees (names, apply-like nodes) to appropriate values.
    // So far, we don't have to set anything to anything,
    // because Environment.Zero (the only possible value for environments at the moment) is the default.
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
              else {
                val hint = {
                  "Note that you can extract a sequence into an unquote when pattern matching," + EOL+
                  "it just cannot follow another sequence either directly or indirectly."
                }
                val errorMessage = s"rank mismatch when unquoting;$EOL found   : ${"." * (quasi.rank + 1)}$EOL required: no dots$EOL$hint"
                c.abort(quasi.position, errorMessage)
              }
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
        // TODO: implement support for ... mixed with .., $ and normal code
        treess match {
          case Seq(Seq(quasi: Quasi)) if quasi.rank == 2 =>
            liftQuasi(quasi)
          case _ =>
            Liftable.liftList[Seq[MetaTree]](Liftables.liftableSubTrees).apply(treess.toList)
        }
      }
      def liftQuasi(quasi: Quasi, optional: Boolean = false): ReflectTree = {
        try {
          pendingQuasis.push(quasi)
          if (quasi.rank == 0) {
            val tree = quasi.tree.asInstanceOf[u.Tree]
            var inferredPt = quasi.pt.wrap(pendingQuasis.map(_.rank).sum).toTpe
            if (optional) inferredPt = appliedType(typeOf[Option[_]], inferredPt)
            val idealReifier = {
              if (mode.isTerm) q"$InternalLift[$inferredPt]($tree)"
              else q"$InternalUnlift[$inferredPt]($tree)"
            }
            val realWorldReifier = mode match {
              case Mode.Term =>
                idealReifier
              case Mode.Pattern(_, holes) =>
                // TODO: Unfortunately, the way how patterns work prevents us from going the ideal route:
                // 1) unapplications can't have explicitly specified type arguments
                // 2) pattern language is very limited and can't express what we want to express in Unlift
                // Therefore, we're forced to take a two-step unquoting scheme: a) match everything in the corresponding hole,
                // b) call Unlift.unapply as a normal method in the right-hand side part of the pattern matching clause.
                val hole = holes.find(hole => tree.equalsStructure(pq"${hole.name}")).getOrElse(sys.error(s"fatal error reifying pattern quasiquote: $quasi, $holes"))
                val inferredPt = hole.arg match {
                  case pq"_: $pt" => pt
                  case pq"$_: $pt" => pt
                  case _ =>
                    var inferredPt = tq"_root_.scala.meta.Tree".wrap(pendingQuasis.map(_.rank).sum)
                    if (optional) inferredPt = tq"_root_.scala.Option[$inferredPt]"
                    inferredPt
                }
                hole.reifier = atPos(quasi.position)(q"$InternalUnlift.unapply[$inferredPt](${hole.name})") // TODO: make `reifier`a immutable somehow
                pq"${hole.name}"
            }
            atPos(quasi.position)(realWorldReifier)
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
                    case Mode.Term =>
                      q"_root_.scala.Some(${liftQuasi(quasi)})"
                    case Mode.Pattern(_, holes) =>
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
                      hole.reifier = atPos(quasi.position)(q"$flattened.flatMap(tree => $unlifted)")
                      reified
                  }
                } else {
                  liftQuasi(quasi)
                }
              case _ =>
                c.abort(quasi.position, "complex ellipses are not supported yet")
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
      case Mode.Term =>
        val internalResult = Lifts.liftTree(meta)
        if (sys.props("quasiquote.debug") != null) {
          println(internalResult)
          // println(showRaw(internalResult))
        }
        q"$InternalUnlift[${c.macroApplication.tpe}]($internalResult)"
      case Mode.Pattern(dummy, holes) =>
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
        val internalResult = q"new { def unapply(input: _root_.scala.meta.Tree) = $matchp }.unapply($dummy)"
        if (sys.props("quasiquote.debug") != null) {
          println(internalResult)
          // println(showRaw(internalResult))
        }
        internalResult // TODO: q"InternalLift[${dummy.tpe}]($internalResult)"
    }
  }

  def log(op: => Unit): Unit = {
    if (sys.props("quasiquote.debug") != null) println(op)
  }
}

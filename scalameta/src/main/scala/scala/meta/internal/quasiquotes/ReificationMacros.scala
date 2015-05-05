package scala.meta
package internal
package quasiquotes

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.runtime.ScalaRunTime
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.collection.{immutable, mutable}
import org.scalameta.adt.{Liftables => AdtLiftables, Reflection => AdtReflection}
import org.scalameta.ast.{Liftables => AstLiftables, Reflection => AstReflection}
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.{Token => MetaToken, Tokens => MetaTokens}
import scala.meta.internal.hygiene.{Denotation => MetaDenotation, Sigma => MetaSigma, _}
import scala.meta.internal.hygiene.{Symbol => MetaSymbol, Prefix => MetaPrefix, Signature => MetaSignature, _}
import scala.meta.internal.parsers.Helpers._
import scala.meta.internal.tokenizers.{LegacyScanner, LegacyToken}
import scala.compat.Platform.EOL

// TODO: ideally, we would like to bootstrap these macros on top of scala.meta
// so that quasiquotes can be interpreted by any host, not just scalac
private[meta] class ReificationMacros(val c: Context) extends AstReflection with AdtLiftables with AstLiftables {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.internal._
  import decorators._
  import c.universe.{Tree => _, Symbol => _, Type => _, Position => _, _}
  import c.universe.{Tree => ReflectTree, Symbol => ReflectSymbol, Type => ReflectType, Position => ReflectPosition, Bind => ReflectBind}
  import scala.meta.{Tree => MetaTree, Type => MetaType, Input => MetaInput, Dialect => MetaDialect}
  import scala.meta.Term.{Name => MetaTermName}
  type MetaParser = (MetaInput, MetaDialect) => MetaTree
  import scala.{meta => api}
  import scala.meta.internal.{ast => impl}
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
    val maybeAttributedSkeleton = scala.util.Try(attributeSkeleton(skeleton)).getOrElse(skeleton)
    reifySkeleton(maybeAttributedSkeleton, mode)
  }

  private def instantiateDialect(dialect: ReflectTree): MetaDialect = {
    // We want to have a higher-order way to abstract over differences in dialects
    // and we're using implicits for that (implicits are values => values are higher-order => good).
    //
    // However, quasiquotes use macros, and macros are first-order, so we have a problem here.
    // Concretely, here we need to convert an implicit argument to a macro (the `dialect` tree)
    // into an instance of `Dialect` that we'll pass to the parser.
    //
    // TODO: For now I'll just prohibit quasiquotes for situations when `dialect` doesn't point to either Scala211 or Dotty.
    // A natural extension to this would be to allow any static value, not just predefined dialects.
    // Later on, we could further relax this restriction by doing parsing for a superset of all dialects and then
    // delaying validation of resulting ASTs until runtime.
    if (dialect.tpe.termSymbol == c.mirror.staticModule("_root_.scala.meta.dialects.Scala211")) _root_.scala.meta.dialects.Scala211
    else if (dialect.tpe.termSymbol == c.mirror.staticModule("_root_.scala.meta.dialects.Dotty")) _root_.scala.meta.dialects.Dotty
    else c.abort(c.enclosingPosition, dialect + " does not have precise enough type to be used in quasiquotes (to fix this, import something from scala.dialects, e.g. scala.meta.dialects.Scala211)")
  }

  private def instantiateParser(interpolator: ReflectSymbol): MetaParser = {
    val parserModule = interpolator.owner.owner.companion
    val metaPackageClass = Class.forName("scala.meta.package", true, classOf[scala.meta.Tree].getClassLoader)
    val parserModuleGetter = metaPackageClass.getDeclaredMethod(parserModule.name.toString)
    val parserModuleInstance = parserModuleGetter.invoke(null)
    val parserMethod = parserModuleInstance.getClass.getDeclaredMethods().find(_.getName == "parse").head
    (input: Input, dialect: Dialect) => {
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
          if (sys.props("quasiquote.debug") != null) println(ex.toString)
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
        val crudeTokens = {
          implicit val tokenizationDialect: MetaDialect = scala.meta.dialects.Quasiquote(metaDialect)
          try part.tokens
          catch {
            case TokenizeException(_, partOffset, message) =>
              if (message.startsWith("unclosed ") && arg.nonEmpty) {
                // NOTE: arg.pos is not precise enough
                // val unquotePosition = arg.pos
                val unquotePosition = partlit.pos.focus.withPoint(end + 1)
                c.abort(unquotePosition, "can't unquote into " + message.stripPrefix("unclosed ") + "s")
              } else {
                val sourceOffset = partOffsetToSourceOffset(partOffset)
                val sourcePosition = partlit.pos.focus.withPoint(sourceOffset)
                c.abort(sourcePosition, message)
              }
          }
        }

        // Step 3: Prettify the tokens by relating them to `wholeFileSource`.
        // To do that, we create Input.Slice over the `wholeFileSource` and fixup positions to account for $$s.
        crudeTokens.map(crudeToken => {
          val delta = partOffsetToSourceOffset(crudeToken.start) - (start + crudeToken.start)
          crudeToken.adjust(input = sliceFileInput(start, end), delta = delta)
        })
      case (part, arg) =>
        c.abort(part.pos, "quasiquotes can only be used with literal strings")
    }
    def merge(index: Int, parttokens: MetaTokens, arg: ReflectTree): MetaTokens = {
      implicit class RichToken(token: Token) { def absoluteStart = token.start + token.input.require[SliceInput].start }
      val part: Tokens = {
        val bof +: payload :+ eof = parttokens
        require(bof.isInstanceOf[Token.BOF] && eof.isInstanceOf[Token.EOF] && debug(parttokens))
        val prefix = if (index == 0) Tokens(bof) else Tokens()
        val suffix = if (index == parttokenss.length - 1) Tokens(eof) else Tokens()
        prefix ++ payload ++ suffix
      }
      val unquote: Tokens = {
        if (arg.isEmpty) {
          Tokens()
        } else {
          val unquoteStart = parttokens.last.absoluteStart
          val unquoteEnd = parttokenss(index + 1).head.absoluteStart - 1
          val unquoteInput = sliceFileInput(unquoteStart, unquoteEnd)
          Tokens(MetaToken.Unquote(unquoteInput, 0, unquoteEnd - unquoteStart, arg))
        }
      }
      part ++ unquote
    }
    val tokens: Tokens = parttokenss.zip(args :+ EmptyTree).zipWithIndex.flatMap({ case ((ts, a), i) => merge(i, ts, a) }).toTokens
    if (sys.props("quasiquote.debug") != null) println(tokens)
    try {
      implicit val parsingDialect: MetaDialect = scala.meta.dialects.Quasiquote(metaDialect)
      val input = Input.Virtual(tokens)
      if (sys.props("quasiquote.debug") != null) println(input.tokens)
      val syntax = metaParse(input, metaDialect)
      if (sys.props("quasiquote.debug") != null) { println(syntax.show[Code]); println(syntax.show[Raw]) }
      (syntax, mode)
    } catch {
      case ParseException(_, token, message) => c.abort(token.pos, message)
    }
  }

  private lazy val wholeFileSource = c.macroApplication.pos.source
  private lazy val wholeFileInput = {
    if (wholeFileSource.file.file != null) Input.File(wholeFileSource.file.file)
    else Input.String(new String(wholeFileSource.content)) // NOTE: can happen in REPL or in custom Global
  }
  private final case class SliceInput(input: Input.Real, start: Int, end: Int) extends Input.Real {
    require(0 <= start && start <= input.content.length)
    require(-1 <= end && end < input.content.length)
    lazy val content = input.content.slice(start, end + 1)
  }
  private def sliceFileInput(start: Int, end: Int) = SliceInput(wholeFileInput, start, end)
  implicit class XtensionTokenPos(token: MetaToken) {
    def pos: ReflectPosition = {
      val SliceInput(input, start, end) = token.input
      require(input == wholeFileInput && debug(token))
      val sourceOffset = start + token.start
      c.macroApplication.pos.focus.withPoint(sourceOffset)
    }
  }
  implicit class XtensionTreePos(tree: MetaTree) {
    def pos = tree.origin.tokens.headOption.map(_.pos).getOrElse(NoPosition)
  }

  // TODO: this is a very naive approach to hygiene, and it will be replaced as soon as possible
  private def attributeSkeleton(meta: MetaTree): MetaTree = {
    def denot(pre: ReflectType, sym: ReflectSymbol): Denotation = {
      def isGlobal(sym: ReflectSymbol): Boolean = {
        def definitelyLocal = sym == NoSymbol || sym.name.toString.startsWith("<local ") || (sym.owner.isMethod && !sym.isParameter)
        def isParentGlobal = sym.isPackage || sym.isPackageClass || isGlobal(sym.owner)
        !definitelyLocal && isParentGlobal
      }
      def signature(sym: ReflectSymbol): MetaSignature = {
        if (sym.isMethod && !sym.asMethod.isGetter) {
          val jvmSignature = {
            // NOTE: unfortunately, this simple-looking facility generates side effects that corrupt the state of the compiler
            // in particular, mixin composition stops working correctly, at least for `object Main extends App`
            // val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
            // g.exitingDelambdafy(new g.genASM.JPlainBuilder(null, false).descriptor(gsym))
            def jvmSignature(tpe: ReflectType): String = {
              val TypeRef(_, sym, args) = tpe
              require(args.nonEmpty ==> (sym == definitions.ArrayClass))
              if (sym == definitions.UnitClass || sym == c.mirror.staticClass("scala.runtime.BoxedUnit")) "V"
              else if (sym == definitions.BooleanClass) "Z"
              else if (sym == definitions.CharClass) "C"
              else if (sym == definitions.ByteClass) "B"
              else if (sym == definitions.ShortClass) "S"
              else if (sym == definitions.IntClass) "I"
              else if (sym == definitions.FloatClass) "F"
              else if (sym == definitions.LongClass) "J"
              else if (sym == definitions.DoubleClass) "D"
              else if (sym == definitions.ArrayClass) "[" + jvmSignature(args.head)
              else "L" + sym.fullName.replace(".", "/") + ";"
            }
            val MethodType(params, ret) = sym.info.erasure
            val jvmRet = if (!sym.isConstructor) ret else definitions.UnitClass.toType
            s"(" + params.map(param => jvmSignature(param.info)).mkString("") + ")" + jvmSignature(jvmRet)
          }
          MetaSignature.Method(jvmSignature)
        }
        else if (sym.isTerm) MetaSignature.Term
        else if (sym.isType) MetaSignature.Type
        else unreachable(debug(sym, sym.flags, sym.getClass, sym.owner))
      }
      def convertPrefix(pre: ReflectType): MetaPrefix = {
        def defaultPrefix(sym: ReflectSymbol): ReflectType = {
          if (sym.isType && sym.asType.isExistential && sym.asType.isParameter) NoPrefix
          else if (sym.isConstructor) defaultPrefix(sym.owner)
          else sym.owner.asInstanceOf[scala.reflect.internal.Symbols#Symbol].thisType.asInstanceOf[ReflectType]
        }
        def singletonType(pre: ReflectType, sym: ReflectSymbol): MetaType = {
          val name = {
            if (sym == c.mirror.RootClass || sym == c.mirror.RootPackage) "_root_"
            else if (sym == c.mirror.EmptyPackageClass || sym == c.mirror.EmptyPackage) "_empty_"
            else sym.name.toString
          }
          impl.Type.Singleton(impl.Term.Name(name, denot(pre, sym), Sigma.Naive))
        }
        val pre1 = pre.orElse(defaultPrefix(sym))
        pre1 match {
          case NoPrefix => MetaPrefix.Zero
          case ThisType(sym) => MetaPrefix.Type(singletonType(NoType, sym))
          case SingleType(pre, sym) => MetaPrefix.Type(singletonType(pre, sym))
          case TypeRef(pre, sym, Nil) if sym.isModule || sym.isModuleClass => MetaPrefix.Type(singletonType(pre, sym))
          case _ => sys.error(s"unsupported type ${pre1}, designation = ${pre1.getClass}, structure = ${showRaw(pre1, printIds = true, printTypes = true)}")
        }
      }
      def convertSymbol(sym: ReflectSymbol): MetaSymbol = {
        if (sym.isModuleClass) convertSymbol(sym.asClass.module)
        else if (sym == c.mirror.RootPackage) MetaSymbol.Root
        else if (sym == c.mirror.EmptyPackage) MetaSymbol.Empty
        else MetaSymbol.Global(convertSymbol(sym.owner), sym.name.decodedName.toString, signature(sym))
      }
      require(isGlobal(sym) && debug(pre, sym))
      Denotation.Precomputed(convertPrefix(pre), convertSymbol(sym))
    }
    def correlate(meta: MetaTree, reflect: ReflectTree): MetaTree = (meta, reflect) match {
      case (meta, reflect: TypeTree) =>
        correlate(meta, reflect.original)
      case (meta: impl.Term.Name, reflect: RefTree) =>
        impl.Term.Name(meta.value, denot(reflect.qualifier.tpe, reflect.symbol), Sigma.Naive)
      case (meta: impl.Type.Name, reflect: RefTree) =>
        impl.Type.Name(meta.value, denot(reflect.qualifier.tpe, reflect.symbol), Sigma.Naive)
      case (meta: impl.Ref, reflect: Ident) =>
        val fakePrefix = Ident(reflect.symbol.owner).setType(reflect.symbol.owner.asInstanceOf[scala.reflect.internal.Symbols#Symbol].tpe.asInstanceOf[ReflectType])
        correlate(meta, Select(fakePrefix, reflect.symbol.name).setSymbol(reflect.symbol).setType(reflect.tpe))
      case (meta: impl.Term.Select, reflect: RefTree) =>
        val qual = correlate(meta.qual, reflect.qualifier).require[impl.Term]
        val name = correlate(meta.name, reflect).require[impl.Term.Name]
        impl.Term.Select(qual, name)
      case (meta: impl.Type.Select, reflect: RefTree) =>
        val qual = correlate(meta.qual, reflect.qualifier).require[impl.Term.Ref]
        val name = correlate(meta.name, reflect).require[impl.Type.Name]
        impl.Type.Select(qual, name)
      case (meta: impl.Type.Singleton, reflect: SingletonTypeTree) =>
        val qual = correlate(meta.ref, reflect.ref).require[impl.Term.Ref]
        impl.Type.Singleton(qual)
      case (meta: impl.Type.Apply, reflect: AppliedTypeTree) =>
        val tpe = correlate(meta.tpe, reflect.tpt).require[impl.Type]
        val args = meta.args.zip(reflect.args).map{ case (meta, reflect) => correlate(meta, reflect).require[impl.Type] }
        impl.Type.Apply(tpe, args)
      case _ =>
        sys.error("correlation of " + meta.productPrefix + " and " + reflect.productPrefix + " is not supported yet")
    }
    val denotDebug = sys.props("denot.debug") != null
    val denotWarn = sys.props("denot.warn") != null
    if (denotDebug) { println("meta = " + meta); println(meta.show[Raw]) }
    try {
      def typecheckTerm(tree: ReflectTree) = {
        val result = c.typecheck(tree, mode = c.TERMmode, silent = true)
        if (result != EmptyTree) result
        else {
          import scala.reflect.internal.Mode
          import scala.reflect.internal.Mode._
          val TERMQUALmode = EXPRmode | QUALmode
          c.typecheck(tree, mode = TERMQUALmode.asInstanceOf[c.TypecheckMode], silent = false)
        }
      }
      def typecheckType(tree: ReflectTree) = c.typecheck(tree, mode = c.TYPEmode, silent = false)
      val (reflectParse, reflectTypecheck) = meta match {
        case _: scala.meta.Term => ((code: String) => c.parse(code), (tree: ReflectTree) => typecheckTerm(tree))
        case _: scala.meta.Type => ((code: String) => c.parse(s"type T = $code").asInstanceOf[TypeDef].rhs, (tree: ReflectTree) => typecheckType(tree))
        case _ => sys.error("attribution of " + meta.productPrefix + " is not supported yet")
      }
      val reflect = {
        // NOTE: please don't ask me about the hacks that you see below
        // even on three simple tests, scalac's typecheck has managed to screw me up multiple times
        // requiring crazy workarounds for really trivial situations
        def undealias(symbol: ReflectSymbol): ReflectSymbol = {
          if (symbol == ListModule && !meta.toString.contains(".List")) ScalaList
          else if (symbol == NilModule && !meta.toString.contains(".Nil")) ScalaNil
          else if (symbol == SeqModule && !meta.toString.contains(".Seq")) ScalaSeq
          else symbol
        }
        val untypedResult = reflectParse(meta.toString)
        var result = reflectTypecheck(untypedResult)
        if (result match { case _: SingletonTypeTree => true; case _ => false }) result = SingletonTypeTree(Ident(undealias(result.tpe.termSymbol)))
        if (result.symbol != undealias(result.symbol)) result = Ident(undealias(result.symbol))
        result
      }
      if (denotDebug) { println("reflect = " + reflect); println(showRaw(reflect, printIds = true)) }
      val meta1 = correlate(meta, reflect)
      if (denotDebug) { println("result = " + meta1.show[Semantics]) }
      meta1
    } catch {
      case ex: Throwable =>
        if (denotDebug) ex.printStackTrace()
        if (denotWarn) c.warning(c.enclosingPosition, "implementation restriction: failed to attribute the quasiquote, proceeding in unhygienic mode")
        throw ex
    }
  }

  private def reifySkeleton(meta: MetaTree, mode: Mode): ReflectTree = {
    val pendingQuasis = mutable.Stack[impl.Quasi]()
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
        else AppliedTypeTree(tq"$ImmutableSeq", List(tree))
      }
    }
    object Lifts {
      def liftTree(tree: api.Tree): u.Tree = {
        Liftables.liftableSubTree(tree)
      }
      def liftTrees(trees: Seq[api.Tree]): u.Tree = {
        def loop(trees: List[api.Tree], acc: u.Tree, prefix: List[api.Tree]): u.Tree = trees match {
          // TODO: instead of checking against 1, we should also take pendingQuasis into account
          case (quasi: impl.Quasi) +: rest if quasi.rank == 1 =>
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
                c.abort(quasi.pos, errorMessage)
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
      def liftTreess(treess: Seq[Seq[api.Tree]]): u.Tree = {
        // TODO: implement support for construction and deconstruction with ...
        Liftable.liftList[Seq[api.Tree]](Liftables.liftableSubTrees).apply(treess.toList)
      }
      def liftQuasi(quasi: impl.Quasi): u.Tree = {
        try {
          pendingQuasis.push(quasi)
          if (quasi.rank == 0) {
            val tree = quasi.tree.asInstanceOf[u.Tree]
            val pt = quasi.pt.wrap(pendingQuasis.map(_.rank).sum).toTpe
            val idealReifier = if (mode.isTerm) q"$InternalLift[$pt]($tree)" else q"$InternalUnlift[$pt]($tree)"
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
                val unrankedPt = hole.arg match { case pq"_: $pt" => pt; case pq"$_: $pt" => pt; case _ => tq"_root_.scala.meta.Tree" }
                val rankedPt = unrankedPt.wrap(pendingQuasis.map(_.rank).sum)
                hole.reifier = atPos(quasi.pos)(q"$InternalUnlift.unapply[$rankedPt](${hole.name})") // TODO: make reifier immutable somehow
                pq"${hole.name}"
            }
            atPos(quasi.pos)(realWorldReifier)
          } else {
            quasi.tree match {
              case quasi: impl.Quasi if quasi.rank == 0 => liftQuasi(quasi)
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
      lazy implicit val liftableDenotation: Liftable[MetaDenotation] = materializeAdt[MetaDenotation]
      lazy implicit val liftableSigma: Liftable[MetaSigma] = materializeAdt[MetaSigma]
      implicit def liftableSubTree[T <: api.Tree]: Liftable[T] = Liftable((tree: T) => materializeAst[api.Tree].apply(tree))
      implicit def liftableSubTrees[T <: api.Tree]: Liftable[Seq[T]] = Liftable((trees: Seq[T]) => Lifts.liftTrees(trees))
      implicit def liftableSubTreess[T <: api.Tree]: Liftable[Seq[Seq[T]]] = Liftable((treess: Seq[Seq[T]]) => Lifts.liftTreess(treess))
      lazy implicit val liftQuasi: Liftable[impl.Quasi] = Liftable((quasi: impl.Quasi) => Lifts.liftQuasi(quasi))
      lazy implicit val liftName: Liftable[impl.Name] = Liftable((name: impl.Name) => {
        val root = q"_root_": ReflectTree
        val fullProductPrefix = "scala.meta.internal.ast." + name.productPrefix
        val constructorDeconstructor = fullProductPrefix.split('.').foldLeft(root)((acc, part) => q"$acc.${TermName(part)}")
        var args = name.productIterator.toList.map { case s: String => q"$s"; case other => unreachable(debug(other, other.getClass)) }
        if (mode.isTerm) args ++= List(q"${name.denot}", q"${name.sigma}")
        else () // TODO: figure out how to use denotations in pattern matching
        q"$constructorDeconstructor(..$args)"
      })
      private def prohibitTermName(pat: impl.Pat): Unit = pat match {
        case q: impl.Quasi if q.tree.asInstanceOf[ReflectTree].tpe <:< typeOf[MetaTermName] =>
          val action = if (q.rank == 0) "unquote" else "splice"
          c.abort(q.pos, s"can't $action a name here, use a variable pattern instead")
        case _ =>
      }
      lazy implicit val liftDefnVal: Liftable[impl.Defn.Val] = Liftable((v: impl.Defn.Val) => {
        v.pats.foreach(prohibitTermName)
        q"_root_.scala.meta.internal.ast.Defn.Val(${v.mods}, ${v.pats}, ${v.decltpe}, ${v.rhs})"
      })
      lazy implicit val liftDefnVar: Liftable[impl.Defn.Var] = Liftable((v: impl.Defn.Var) => {
        v.pats.foreach(prohibitTermName)
        q"_root_.scala.meta.internal.ast.Defn.Var(${v.mods}, ${v.pats}, ${v.decltpe}, ${v.rhs})"
      })
      lazy implicit val liftPatTyped: Liftable[impl.Pat.Typed] = Liftable((p: impl.Pat.Typed) => {
        prohibitTermName(p.lhs)
        q"_root_.scala.meta.internal.ast.Pat.Typed(${p.lhs}, ${p.rhs})"
      })
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
        val internalResult = q"""
          new {
            def unapply(input: _root_.scala.meta.Tree) = {
              input match {
                case $pattern => $thenp
                case _ => $elsep
              }
            }
          }.unapply($dummy)
        """
        if (sys.props("quasiquote.debug") != null) {
          println(internalResult)
          // println(showRaw(internalResult))
        }
        internalResult // TODO: q"InternalLift[${dummy.tpe}]($internalResult)"
    }
  }
}

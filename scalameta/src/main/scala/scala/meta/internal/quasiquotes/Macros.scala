package scala.meta
package internal
package quasiquotes

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.collection.{immutable, mutable}
import org.scalameta.adt.{Liftables => AdtLiftables, AdtReflection}
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.{Token => MetaToken}
import scala.meta.internal.hygiene.{Denotation => MetaDenotation, Sigma => MetaSigma, _}
import scala.meta.internal.hygiene.{Symbol => MetaSymbol, Prefix => MetaPrefix, Signature => MetaSignature, _}
import scala.meta.internal.tokenizers.{LegacyScanner, LegacyToken}

// TODO: ideally, we would like to bootstrap these macros on top of scala.meta
// so that quasiquotes can be interpreted by any host, not just scalac
private[meta] class Macros(val c: Context) extends AdtReflection with AdtLiftables {
  val u: c.universe.type = c.universe
  val mirror: u.Mirror = c.mirror
  import c.internal._
  import decorators._
  import c.universe.{Tree => _, Symbol => _, Type => _, _}
  import c.universe.{Tree => ReflectTree, Symbol => ReflectSymbol, Type => ReflectType}
  import scala.meta.{Tree => MetaTree, Type => MetaType, Input => MetaInput, Dialect => MetaDialect}
  type MetaParser = (MetaInput, MetaDialect) => MetaTree
  import scala.meta.internal.{ast => impl}
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  import definitions._
  val SeqModule = c.mirror.staticModule("scala.collection.Seq")
  val ScalaPackageObjectClass = c.mirror.staticModule("scala.package")
  val ScalaList = ScalaPackageObjectClass.info.decl(TermName("List"))
  val ScalaNil = ScalaPackageObjectClass.info.decl(TermName("Nil"))
  val ScalaSeq = ScalaPackageObjectClass.info.decl(TermName("Seq"))

  def apply(args: ReflectTree*)(dialect: ReflectTree): ReflectTree = {
    val skeleton = parseSkeleton(c.macroApplication, instantiateDialect(dialect), instantiateParser(c.macroApplication.symbol))
    val maybeAttributedSkeleton = scala.util.Try(attributeSkeleton(skeleton)).getOrElse(skeleton)
    reifySkeleton(maybeAttributedSkeleton)
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
    else c.abort(c.enclosingPosition, "can't use the " + dialect + " dialect in quasiquotes")
  }

  private def instantiateParser(interpolator: ReflectSymbol): MetaParser = {
    val parserModule = interpolator.owner.owner.companion
    val metaPackageClass = Class.forName("scala.meta.package", true, classOf[scala.meta.Tree].getClassLoader)
    val parserModuleGetter = metaPackageClass.getDeclaredMethod(parserModule.name.toString)
    val parserModuleInstance = parserModuleGetter.invoke(null)
    val parserMethod = parserModuleInstance.getClass.getDeclaredMethods().find(_.getName == "parse").head
    (input: Input, dialect: Dialect) => parserMethod.invoke(parserModuleInstance, input, dialect).asInstanceOf[MetaTree]
  }

  private def parseSkeleton(macroApplication: ReflectTree, metaDialect: MetaDialect, metaParse: MetaParser): MetaTree = {
    implicit val implicitMetaDialect: MetaDialect = metaDialect
    macroApplication match {
      case q"$_($_.apply(..$parts)).$_.apply[..$_](..$args)($_)" if parts.length == args.length + 1 =>
        val parttokenss = parts.map{
          case partlit @ q"${part: String}" =>
            val source = macroApplication.pos.source
            val dollars = mutable.ListBuffer[Int]()
            val start = partlit.pos.start
            var remaining = part.length
            var end = start - 1
            while (remaining > 0) {
              end += 1
              if (source.content(end) == '$') {
                dollars += end
                end += 1
                require(source.content(end) == '$')
              }
              remaining -= 1
            }
            val tokens = part.tokens
            val preciseTokens = new immutable.VectorBuilder[MetaToken]()
            val wholeFile = {
              if (source.file.file != null) Input.File(source.file.file)
              else Input.Chars(source.content)
            }
            val slice = Input.Slice(wholeFile, start, end)
            var i = 0
            var delta = 0
            while (i < tokens.length) {
              val token = tokens(i)
              if (delta < dollars.length && token.start >= dollars(delta)) delta += 1
              preciseTokens += token.adjust(input = slice, delta = delta)
              i += 1
            }
            preciseTokens.result
          case part =>
            c.abort(part.pos, "quasiquotes can only be used with literal strings")
        }
        def merge(parttokens: Vector[MetaToken], arg: ReflectTree) = parttokens :+ MetaToken.Unquote(arg)
        val tokens = parttokenss.init.zip(args).flatMap((merge _).tupled) ++ parttokenss.last
        if (sys.props("quasiquote.debug") != null) println(tokens)
        metaParse(Input.Tokens(tokens.toVector), metaDialect)
      case _ =>
        c.abort(macroApplication.pos, s"fatal error initializing quasiquote macro: ${showRaw(macroApplication)}")
    }
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

  private def reifySkeleton(meta: MetaTree): ReflectTree = {
    // NOTE: we could write just `implicitly[Liftable[MetaTree]].apply(meta)`
    // but that would bloat the code significantly with duplicated instances for denotations and sigmas
    object LiftableInstances {
      lazy implicit val liftableDenotation: Liftable[MetaDenotation] = materializeAdt[MetaDenotation]
      lazy implicit val liftableSigma: Liftable[MetaSigma] = materializeAdt[MetaSigma]
      lazy implicit val liftableTree: Liftable[MetaTree] = materializeAdt[Tree]
      implicit def liftableSubTree[T <: MetaTree]: Liftable[T] = Liftable((tree: T) => liftableTree(tree))
    }
    LiftableInstances.liftableTree.apply(meta)
  }

  def unapply(scrutinee: c.Tree)(dialect: c.Tree): ReflectTree = {
    c.abort(c.enclosingPosition, "pattern matching for scala.meta quasiquotes hasn't been implemented yet")
  }
}
package scala.meta
package internal
package quasiquotes

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import org.scalameta.adt.{Liftables => AdtLiftables, AdtReflection}
import org.scalameta.ast.{Liftables => AstLiftables}
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.meta.internal.hygiene.{Symbol => MetaSymbol, Prefix => MetaPrefix, Signature => MetaSignature, _}
import scala.meta.ui._

// TODO: ideally, we would like to bootstrap these macros on top of scala.meta
// so that quasiquotes can be interpreted by any host, not just scalac
class Macros[C <: Context](val c: C) extends AdtReflection with AdtLiftables with AstLiftables {
  val u: c.universe.type = c.universe
  import c.internal._
  import decorators._
  import c.universe.{Tree => _, Symbol => _, Type => _, _}
  import c.universe.{Tree => ReflectTree, Symbol => ReflectSymbol, Type => ReflectType}
  import scala.meta.{Tree => MetaTree, Type => MetaType}
  import scala.meta.internal.{ast => impl}
  val TermQuote = "shadow scala.meta quasiquotes"
  case class Dummy(id: String, ndots: Int, arg: ReflectTree)

  import definitions._
  val SeqModule = c.mirror.staticModule("scala.collection.Seq")
  val ScalaPackageObjectClass = c.mirror.staticModule("scala.package")
  val ScalaList = ScalaPackageObjectClass.info.decl(TermName("List"))
  val ScalaNil = ScalaPackageObjectClass.info.decl(TermName("Nil"))
  val ScalaSeq = ScalaPackageObjectClass.info.decl(TermName("Seq"))

  def apply(macroApplication: ReflectTree, metaParse: String => MetaTree): ReflectTree = {
    val SyntacticFlavor = symbolOf[scala.meta.syntactic.quasiquotes.Enable.type]
    val SemanticFlavor = symbolOf[scala.meta.semantic.quasiquotes.Enable.type]
    val flavor = c.inferImplicitValue(typeOf[scala.meta.quasiquotes.Flavor]).tpe.typeSymbol
    flavor match {
      case SyntacticFlavor =>
        val (skeleton, dummies) = parseSkeleton(macroApplication, metaParse)
        reifySkeleton(skeleton, dummies)
      case SemanticFlavor =>
        // TODO: this is a very naive approach to hygiene, and it will be replaced as soon as possible
        val (skeleton, dummies) = parseSkeleton(macroApplication, metaParse)
        val maybeAttributedSkeleton = scala.util.Try(attributeSkeleton(skeleton)).getOrElse(skeleton)
        reifySkeleton(maybeAttributedSkeleton, dummies)
      case _ =>
        c.abort(c.enclosingPosition, "choose the flavor of quasiquotes by importing either scala.meta.syntactic.quasiquotes._ or scala.meta.semantic.quasiquotes._")
    }
  }

  private def parseSkeleton(macroApplication: ReflectTree, metaParse: String => MetaTree): (MetaTree, List[Dummy]) = {
    val q"$_($_.apply(..$partlits)).$_.apply[..$_](..$argtrees)($dialect)" = macroApplication
    val parts = partlits.map{ case q"${part: String}" => part }
    def ndots(s: String): Int = if (s.endsWith(".")) ndots(s.stripSuffix(".")) + 1 else 0
    val dummies = argtrees.zipWithIndex.map{ case (tree, i) => Dummy(c.freshName("dummy"), ndots(parts(i)), tree) }
    val snippet = (parts.init.zip(dummies).flatMap{ case (part, Dummy(id, ndots, _)) => List(if (ndots != 1) part.stripSuffix("." * ndots) else part, s"$id") } :+ parts.last).mkString("")
    (metaParse(snippet), dummies)
  }

  private def attributeSkeleton(meta: MetaTree): MetaTree = {
    def denot(pre: ReflectType, sym: ReflectSymbol): Denotation = {
      def isGlobal(sym: ReflectSymbol): Boolean = {
        def definitelyLocal = sym == NoSymbol || sym.name.toString.startsWith("<local ") || (sym.owner.isMethod && !sym.isParameter)
        def isParentGlobal = sym.isPackage || sym.isPackageClass || isGlobal(sym.owner)
        !definitelyLocal && isParentGlobal
      }
      def signature(sym: ReflectSymbol): MetaSignature = {
        if (sym.isMethod && !sym.asMethod.isGetter) {
          val g = c.universe.asInstanceOf[scala.tools.nsc.Global]
          val jvmSignature = g.exitingDelambdafy(new g.genASM.JPlainBuilder(null, false).descriptor(sym.asInstanceOf[g.Symbol]))
          MetaSignature.Method(jvmSignature)
        }
        else if (sym.isTerm) MetaSignature.Term
        else if (sym.isType) MetaSignature.Type
        else unreachable
      }
      def convertPrefix(pre: ReflectType): MetaPrefix = {
        def defaultPrefix(sym: ReflectSymbol): ReflectType = {
          if (sym.isType && sym.asType.isExistential && sym.asType.isParameter) NoPrefix
          else if (sym.isConstructor) defaultPrefix(sym.owner)
          else sym.owner.asInstanceOf[scala.reflect.internal.Symbols#Symbol].thisType.asInstanceOf[ReflectType]
        }
        def singletonType(pre: ReflectType, sym: ReflectSymbol): MetaType = {
          impl.Type.Singleton(impl.Term.Name(sym.name.toString, denot(pre, sym), Sigma.Naive))
        }
        val pre1 = pre.orElse(defaultPrefix(sym))
        pre1 match {
          case NoPrefix => MetaPrefix.Zero
          case ThisType(sym) => MetaPrefix.Type(singletonType(NoType, sym))
          case SingleType(pre, sym) => MetaPrefix.Type(singletonType(pre, sym))
          case TypeRef(pre, sym, Nil) if sym.isModule || sym.isModuleClass || sym.isPackage || sym.isPackageClass => MetaPrefix.Type(singletonType(pre, sym))
          case _ => sys.error(s"unsupported type ${pre1}, designation = ${pre1.getClass}, structure = ${showRaw(pre1, printIds = true, printTypes = true)}")
        }
      }
      def convertSymbol(sym: ReflectSymbol): MetaSymbol = {
        if (sym.isModuleClass || sym.isPackageClass) convertSymbol(sym.asClass.module)
        else if (sym == c.mirror.RootClass || sym == c.mirror.RootPackage) MetaSymbol.Root
        else MetaSymbol.Global(convertSymbol(sym.owner), sym.name.decodedName.toString, signature(sym))
      }
      require(pre != null && sym != NoSymbol && isGlobal(sym))
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
        val name = correlate(meta.selector, reflect).require[impl.Term.Name]
        impl.Term.Select(qual, name)
      case (meta: impl.Type.Select, reflect: RefTree) =>
        val qual = correlate(meta.qual, reflect.qualifier).require[impl.Term.Ref]
        val name = correlate(meta.selector, reflect).require[impl.Type.Name]
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
    if (denotDebug) { println("meta = " + meta); println(meta.show[Raw]) }
    try {
      val (reflectParse, reflectMode) = meta match {
        case _: scala.meta.Term => ((code: String) => c.parse(code), c.TERMmode)
        case _: scala.meta.Type => ((code: String) => c.parse(s"type T = $code").asInstanceOf[TypeDef].rhs, c.TYPEmode)
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
        var result = c.typecheck(reflectParse(meta.toString), mode = reflectMode, silent = false)
        if (result match { case _: SingletonTypeTree => true; case _ => false }) result = SingletonTypeTree(Ident(undealias(result.tpe.termSymbol)))
        if (result.symbol != undealias(result.symbol)) result = Ident(undealias(result.symbol))
        result
      }
      if (denotDebug) { println("reflect = " + reflect); println(showRaw(reflect, printIds = true)) }
      val meta1 = correlate(meta, reflect)
      if (denotDebug) { println("result = " + meta1.show[Semantics]) }
      meta1
    } catch {
      case ex: Throwable => if (denotDebug) ex.printStackTrace(); throw ex
    }
  }

  private def reifySkeleton(meta: MetaTree, dummies: List[Dummy]): ReflectTree = {
    implicitly[Liftable[MetaTree]].apply(meta)
  }
}
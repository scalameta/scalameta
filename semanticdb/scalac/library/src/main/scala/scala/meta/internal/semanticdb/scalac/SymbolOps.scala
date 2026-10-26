package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.scalacp._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d, Names => n}
import scala.meta.internal.{semanticdb => s}

import java.nio.CharBuffer

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.reflect.internal.util.{NoSourceFile => GNoSourceFile, SourceFile => GSourceFile}
import scala.reflect.internal.{Chars, Flags => gf}
import scala.tools.nsc.backend.jvm.GenBCode
import scala.util.control.NonFatal

trait SymbolOps {
  self: SemanticdbOps =>

  def clearSymbolCaches(): Unit = {
    symbolCache.clear()
    declarationCache.clear()
    sourceIndexes.clear()
    runSources.clear()
    runSourcesRun = null
    namespaces.clear()
    clearDocumentCaches()
  }

  def clearDocumentCaches(): Unit = {
    localCache.clear()
    freeExistentials.clear()
  }

  private lazy val symbolCache = new mutable.HashMap[g.Symbol, String]
  // the locals that we looked up since a document started or ended: either changes a namespace
  private lazy val localCache = new mutable.HashMap[g.Symbol, String]

  private lazy val sourceIndexes = new mutable.HashMap[GSourceFile, SourceIndex]
  private lazy val namespaces = new mutable.HashMap[GSourceFile, Namespace]

  // the units of `runSourcesRun` by source, when it had `runSourcesSize` units
  private lazy val runSources = new java.util.IdentityHashMap[GSourceFile, g.CompilationUnit]
  private var runSourcesRun: g.Run = _
  private var runSourcesSize = 0

  private var documentSource: GSourceFile = GNoSourceFile
  private var documentBody: g.Tree = _
  private var documentNamespace: Namespace = _

  implicit class XtensionGSymbolMSymbol(sym: g.Symbol) {
    private[semanticdb] def toSemanticUncached: String = {
      if (sym == null || sym == g.NoSymbol) return Symbols.None
      if (sym.isOverloaded) return Symbols.Multi(sym.alternatives.map(_.toSemantic))
      if (sym.isModuleClass) return sym.asClass.module.toSemantic
      if (sym.isTypeSkolem) return sym.deSkolemize.toSemantic
      if (sym.isSemanticdbLocal) return freshSymbol(sym)

      val owner = sym.owner.toSemantic
      val desc =
        if (sym.isValMethod) d.Term(sym.symbolName)
        else if (sym.isMethod || sym.isUsefulField) d.Method(sym.symbolName, sym.disambiguator)
        else if (sym.isTypeParameter && sym.name == g.tpnme.WILDCARD) {
          // 2.12 names each anonymous type param `_`: number them within the owner
          val anonymous = sym.owner.typeParams.filter(_.name == g.tpnme.WILDCARD)
          val index = anonymous.indexOf(sym)
          d.TypeParameter(if (index < 0) sym.symbolName else "_$$" + (index + 1))
        } else if (sym.isTypeParameter) d.TypeParameter(sym.symbolName)
        else if (sym.isValueParameter) d.Parameter(sym.symbolName)
        else if (sym.isType || sym.isJavaClass) d.Type(sym.symbolName)
        else if (sym.hasPackageFlag) d.Package(sym.symbolName)
        else d.Term(sym.symbolName)
      Symbols.Global(owner, desc)
    }

    def toSemantic: String = {
      def defaultValue = {
        val ssym = // see https://github.com/scalameta/scalameta/issues/1194
          try toSemanticUncached
          catch { case e if isInteractiveCompiler && NonFatal(e) => Symbols.None }
        if (ssym.isLocal) localCache(sym) = ssym else symbolCache(sym) = ssym
        ssym
      }
      symbolCache.getOrElse(sym, localCache.getOrElse(sym, defaultValue))
    }
  }

  implicit class XtensionGSymbolMSpec(sym: g.Symbol) {
    def isSemanticdbGlobal: Boolean = !isSemanticdbLocal
    def isSemanticdbLocal: Boolean = {
      def definitelyGlobal = sym.hasPackageFlag
      def definitelyLocal = sym == g.NoSymbol || sym.owner.isTerm && !sym.isParameter ||
        (sym.owner.isAliasType || sym.owner.isAbstractType) && !sym.isParameter ||
        sym.isSelfParameter || sym.isLocalDummy || sym.isRefinementClass || sym.isAnonymousClass ||
        sym.isAnonymousFunction || sym.hasFlag(gf.EXISTENTIAL)
      def ownerLocal = sym.owner.isSemanticdbLocal
      !definitelyGlobal && (definitelyLocal || ownerLocal)
    }
    def isSemanticdbMulti: Boolean = sym.isOverloaded
    def symbolName: String =
      if (sym.name == g.nme.ROOTPKG) n.RootPackage.value
      else if (sym.name == g.nme.EMPTY_PACKAGE_NAME) n.EmptyPackage.value
      else if (sym.isConstructor) n.Constructor.value
      else NameTransformer.decode(sym.name.toString.stripSuffix(g.nme.LOCAL_SUFFIX_STRING))
    def disambiguator: String = {
      val peers = sym.owner.semanticdbDecls.gsyms
      val overloads = peers
        .filter(peer => peer.isMethod && peer.symbolName == sym.symbolName && !peer.isValMethod)
      val suffix =
        if (overloads.lengthCompare(1) == 0) ""
        else {
          val index = overloads.indexOf(sym)
          if (index <= 0) "" else "+" + index
        }
      "(" + suffix + ")"
    }
    def semanticdbDecls: SemanticdbDecls =
      if (sym.hasPackageFlag) SemanticdbDecls(Nil)
      else if (sym.isModule)
        if (sym.isJavaDefined) sym.companionClass.semanticdbDecls
        else sym.moduleClass.semanticdbDecls
      else if (sym.isModuleClass && sym.isJavaDefined) sym.companionClass.semanticdbDecls
      else {
        @tailrec
        def loop(info: g.Type): SemanticdbDecls = info match {
          case info: g.PolyType => loop(info.resultType)
          case info: g.ClassInfoType =>
            /* Reorder companion objects to their source position (#1544): the namer enters an
             * eagerly-created companion right after its class, so an explicit `object` declared
             * later keeps that early slot. Only these modules move; other members keep scalac's
             * scope order.
             */
            def isReorderable(gsym: g.Symbol): Boolean = gsym.isModule && !gsym.isSynthetic &&
              !gsym.isJavaDefined && gsym.pos.isDefined
            val gbuf = List.newBuilder[g.Symbol]
            val gmod = List.newBuilder[g.Symbol]
            info.decls.sorted.foreach(x =>
              if (x.isUsefulSymbolInformation) if (isReorderable(x)) gmod += x else gbuf += x,
            )
            if (sym.isJavaDefined) sym.companionModule.info.decls.filter(_.isUseful).foreach(gbuf.+=)
            val others = gbuf.result()
            val modules = gmod.result()
            if (modules.isEmpty) SemanticdbDecls(others)
            else {
              // merge the modules into the other members by source offset
              val rest = others.iterator.buffered
              val result = List.newBuilder[g.Symbol]
              modules.sortBy(_.pos.start).foreach { module =>
                def precedes(other: g.Symbol) = !other.pos.isDefined ||
                  other.pos.start <= module.pos.start
                while (rest.hasNext && precedes(rest.head)) result += rest.next()
                result += module
              }
              result ++= rest
              SemanticdbDecls(result.result())
            }
          case _ => SemanticdbDecls(Nil)
        }
        loop(sym.info)
      }
  }

  implicit class XtensionGScopeMSpec(scope: g.Scope) {
    def semanticdbDecls: SemanticdbDecls = SemanticdbDecls(scope.sorted.filter(_.isUseful))
  }

  implicit class XtensionGSymbolsMSpec(syms: List[g.Symbol]) {
    def sscope(linkMode: LinkMode): s.Scope = linkMode match {
      case SymlinkChildren => s.Scope(symlinks = syms.map(_.ssym))
      case HardlinkChildren => s.Scope(hardlinks = syms.map(_.toSymbolInformation(HardlinkChildren)))
    }
  }

  case class SemanticdbDecls(gsyms: List[g.Symbol]) {
    def sscope(linkMode: LinkMode): s.Scope = linkMode match {
      case SymlinkChildren =>
        val sbuf = List.newBuilder[String]
        gsyms.foreach { gsym =>
          val ssym = gsym.ssym
          sbuf += ssym
          if (gsym.isUsefulField && gsym.isMutable)
            if (ssym.isGlobal) {
              val setterSymbolName = s"${ssym.desc.name}_="
              val setterSym = Symbols.Global(ssym.owner, d.Method(setterSymbolName, "()"))
              sbuf += setterSym
            } else {
              val setterSym = ssym + "+1"
              sbuf += setterSym
            }
        }
        s.Scope(symlinks = sbuf.result())
      case HardlinkChildren =>
        val sbuf = List.newBuilder[s.SymbolInformation]
        gsyms.foreach { gsym =>
          val sinfo = gsym.toSymbolInformation(HardlinkChildren)
          sbuf += sinfo
          if (gsym.isUsefulField && gsym.isMutable) Synthetics.setterInfos(sinfo, HardlinkChildren)
            .foreach(sbuf.+=)
        }
        s.Scope(hardlinks = sbuf.result())
    }
  }

  implicit class XtensionGSymbol(sym: g.Symbol) {

    def ssym: String = sym.toSemantic
    def self: g.Type = sym.thisSym.info match {
      case g.RefinedType(List(_, self), _) if sym.thisSym != sym => self
      case _ => g.NoType
    }
    def isSelfParameter: Boolean = sym != g.NoSymbol && sym.owner.thisSym == sym
    private def isJavaNonPackage: Boolean = sym.getFlag(gf.PACKAGE | gf.JAVA) == gf.JAVA
    def isJavaClass: Boolean = isJavaNonPackage && (sym.isClass || sym.isModule)
    def isSyntheticConstructor: Boolean = {
      val owner = sym.owner
      sym.isConstructor &&
      (owner.isModuleClass || owner.hasAllFlags(gf.JAVA | gf.INTERFACE) || owner.hasJavaEnumFlag) ||
      sym.name == g.TermName(GenBCode.CLASS_CONSTRUCTOR_NAME) || owner.isClassfileAnnotation
    }
    def isLocalChild: Boolean = sym.name == g.tpnme.LOCAL_CHILD
    def isSyntheticValueClassCompanion: Boolean =
      if (sym.isModule) sym.moduleClass.isSyntheticValueClassCompanion
      else sym.isModuleClass && sym.isSynthetic && sym.semanticdbDecls.gsyms.isEmpty
    def isDefMethod: Boolean = sym.isMethod && !sym.isAccessor
    def isValMethod: Boolean = sym.kind.isMethod && {
      sym.isAccessor && sym.isStable || isUsefulField && !sym.isMutable
    }
    def isScalacField: Boolean = {
      val isFieldForPrivateThis = sym.isPrivateThis && sym.isTerm && !sym.isMethod && !sym.isModule
      val isFieldForOther = sym.name.endsWith(g.nme.LOCAL_SUFFIX_STRING)
      val isJavaDefined = sym.isJavaDefined || sym.hasJavaEnumFlag
      (isFieldForPrivateThis || isFieldForOther) && !isJavaDefined
    }
    def isUselessField: Boolean = isScalacField && sym.getterIn(sym.owner) != g.NoSymbol
    def isUsefulField: Boolean = isScalacField && !isUselessField
    def isSyntheticCaseAccessor: Boolean = sym.isCaseAccessor && sym.decodedName.contains("$")
    def isSyntheticJavaModule: Boolean = isJavaNonPackage && sym.isModule
    def isAnonymousClassConstructor: Boolean = sym.isConstructor && sym.owner.isAnonymousClass
    def isSyntheticAbstractType: Boolean = sym.isSynthetic && sym.isAbstractType // these are hardlinked to TypeOps
    // scalac names a parameter `_` like `x$1`, and a user can write such a name too
    def hasFreshName: Boolean = {
      val name = sym.decodedName
      if (!name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX)) return false
      val pos = sym.pos
      if (!pos.isDefined) return true

      !isWrittenAt(pos, name)
    }
    def isEtaExpandedParameter: Boolean =
      // the parameter of `m _` or of `_ + 1`; we save the latter's information with its occurrence
      sym.isParameter && hasFreshName && sym.owner.isAnonymousFunction
    def isAnonymousSelfParameter: Boolean = isSelfParameter && {
      sym.name == g.nme.this_ || // hardlinked in ClassSignature.self
      hasFreshName // wildcards can't be referenced: class A { _: B => }
    }
    def isUseless: Boolean = sym == g.NoSymbol || isSyntheticConstructor ||
      sym.isStaticConstructor || isLocalChild || isSyntheticValueClassCompanion || isUselessField ||
      isSyntheticCaseAccessor || sym.isRefinementClass || isSyntheticJavaModule
    def isUseful: Boolean = !isUseless
    def isUselessOccurrence: Boolean = sym.isAnonymousClass || isUseless && !isSyntheticJavaModule // references to static Java inner classes should have occurrences
    def isSyntheticLocalParam: Boolean = sym.hasAllFlags(gf.SYNTHETIC | gf.PARAM) &&
      sym.isSemanticdbLocal
    // a parameter that scalac made up, or that the user wrote as `_`, has no occurrence
    def isHiddenParameter: Boolean = isEtaExpandedParameter || isSyntheticLocalParam
    // scalac binds the pattern of a for value definition to a value like `x$1`
    def isFreshValue: Boolean = !sym.isParameter && !sym.isSynthetic && !sym.isArtifact &&
      hasFreshName
    def isUselessSymbolInformation: Boolean = isUseless || isAnonymousClassConstructor ||
      isSyntheticAbstractType || isAnonymousSelfParameter || patternValue.exists || isFreshValue
    /* scalac desugars `val p = e` into a match of `e` against `p`, and puts the binders of
     * that match at transparent positions: the value that such a binder defines */
    def patternValue: g.Symbol = {
      import g._
      if (sym.pos.isTransparent) {
        val owner = sym.owner
        if (owner.isTerm) {
          val parent = owner.owner
          if (owner.name.dropLocal == sym.name)
            return if (parent.isClass) owner.getterIn(parent).orElse(owner) else owner
          if (owner.isArtifact && parent.isClass)
            return parent.info.decl(sym.name).filter(_.paramss.isEmpty)
        }
      }
      NoSymbol
    }

    def isUsefulSymbolInformation: Boolean = !isUselessSymbolInformation
    def isClassfileAnnotation: Boolean = sym.isClass && sym.hasFlag(gf.JAVA_ANNOTATION)
    def isDefaultParameter: Boolean = sym.hasFlag(gf.DEFAULTPARAM) && sym.hasFlag(gf.PARAM)
    def isDefaultMethod: Boolean = sym.isJavaDefined && sym.owner.isInterface && !sym.isDeferred &&
      !sym.isStatic

  }

  // the source does not have the whole name written at `at`, maybe in backquotes
  private def notWrittenAt(text: Array[Char], at: Int, name: String): Boolean = {
    val quoted = at < text.length && text(at) == '`'
    val start = if (quoted) at + 1 else at
    val end = start + name.length
    val notEndsThere =
      if (end == text.length) quoted
      else end > text.length || (if (quoted) text(end) != '`' else Chars.isIdentifierPart(text(end)))
    notEndsThere || !name.contentEquals(CharBuffer.wrap(text, start, name.length))
  }

  // the source has `name` written at the start or at the point of `pos`
  private def isWrittenAt(pos: g.Position, name: String): Boolean = {
    val text = pos.source.content
    // a parameter starts at its name, and a 2.12 value has its name at its point
    !notWrittenAt(text, pos.start, name) || !notWrittenAt(text, pos.point, name)
  }

  private def freshSymbol(sym: g.Symbol): String = {
    val gval = sym.patternValue
    if (gval.exists) return gval.toSemantic
    localSymbol(declarationOf(sym))
  }

  /* A local stands for a declaration. A document numbers the locals that it meets in a namespace
   * of its own. Outside a document, a local takes its number in the namespace of its source. */
  private final class Namespace {
    val locals = new mutable.HashMap[g.Symbol, String]
    // the declaration of each local, by its number
    val declarations = new mutable.ArrayBuffer[g.Symbol]
    def foreach(f: (String, g.Symbol) => Unit): Unit = {
      var i = 0
      while (i < declarations.length) {
        val decl = declarations(i)
        f(locals(decl), decl)
        i += 1
      }

    }
  }

  def startDocument(unit: g.CompilationUnit): Unit = {
    clearDocumentCaches()
    documentSource = unit.source
    documentBody = unit.body
    documentNamespace = new Namespace
    namespaces(documentSource) = documentNamespace
  }
  def endDocument(): Unit = {
    documentSource = GNoSourceFile
    documentBody = null
    documentNamespace = null
    localCache.clear()
  }

  private def localSymbol(decl: g.Symbol): String = {
    val namespace =
      if (null ne documentNamespace) documentNamespace
      else {
        val source = enclosingSource(decl)
        if (!source.hasInput) return Symbols.None
        namespaces.getOrElseUpdate(source, new Namespace)
      }
    namespace.locals.getOrElseUpdate(
      decl, {
        val declarations = namespace.declarations
        try Symbols.Local(declarations.length)
        finally declarations += decl
      },
    )
  }

  // the declaration of a local of the document, or `sym`
  private[scalac] def localDeclaration(local: String, sym: g.Symbol): g.Symbol = {
    if (null ne documentNamespace) {
      val number = localNumber(local)
      if (number >= 0) {
        val declarations = documentNamespace.declarations
        if (number < declarations.length) return declarations(number)
      }
    }
    sym
  }

  // the number of a local such as `local12`, or -1
  private def localNumber(local: String): Int = {
    val start = Symbols.LocalPrefix.length
    // a namespace has fewer than 10^9 locals
    if (local.length <= start || local.length > start + 9) return -1
    var number = 0
    var i = start
    while (i < local.length) {
      val digit = local.charAt(i) - '0'
      if (digit < 0 || digit > 9) return -1
      number = number * 10 + digit
      i += 1
    }
    number
  }

  // calls `f` with each local of the document and its declaration, also with the locals `f` adds
  private[scalac] def foreachDocumentLocal(f: (String, g.Symbol) => Unit): Unit =
    if (null ne documentNamespace) documentNamespace.foreach(f)

  @tailrec
  private[semanticdb] final def enclosingSource(sym: g.Symbol): GSourceFile =
    if (sym == g.NoSymbol) GNoSourceFile
    else sym.pos.source match {
      case GNoSourceFile => enclosingSource(sym.owner)
      case src => src
    }

  // the declaration of each local symbol that we looked up in this run
  private lazy val declarationCache = new mutable.HashMap[g.Symbol, g.Symbol]
  private def declarationOf(sym: g.Symbol): g.Symbol = declarationCache.getOrElse(
    sym, {
      // a symbol whose declaration depends on itself is its own declaration
      declarationCache(sym) = sym
      val decl =
        try findDeclaration(sym)
        catch { case NonFatal(_) => sym }
      declarationCache(sym) = decl
      decl
    },
  )
  private def ownerDeclaration(sym: g.Symbol): g.Symbol = {
    val owner = sym.owner
    if (owner.isSemanticdbLocal) declarationOf(owner) else owner
  }

  private def findDeclaration(sym: g.Symbol): g.Symbol = {
    val pos = sym.pos
    val index = if (pos.isDefined) sourceIndex(pos.source) else null
    if (index ne null) {
      val key = DeclarationKey(sym)
      val decls = index.declarations.getOrElse(key, null)
      if (decls eq null) {
        // an existential that no tree declares, such as `?0`
        if (sym.hasFlag(gf.EXISTENTIAL)) return index.existentials.getOrElseUpdate(key, sym)
      } else {
        val decl = declarationAt(sym, decls)
        if (decl ne g.NoSymbol) return decl
      }
    }
    // a parameter of a copy
    if (sym.isParameter) {
      val owner = sym.owner
      if (owner.isSemanticdbLocal) {
        val odecl = declarationOf(owner)
        if (odecl ne owner) {
          val params = if (sym.isType) odecl.typeParams.iterator else odecl.paramss.iterator.flatten
          params.find(_.name == sym.name) match {
            case Some(param) => return declarationOf(param)
            case _ =>
          }
        }
      }
    }
    sym
  }

  // the declaration of `sym` at its key, or NoSymbol
  private def declarationAt(sym: g.Symbol, decls: Declarations): g.Symbol = {
    val rep = decls.representative
    val symbols = decls.symbols
    if (symbols.contains(sym)) return if (rep ne null) rep else sym

    // a copy has the position and the name of its original
    lazy val owner = ownerDeclaration(sym)
    def isOriginal(decl: g.Symbol) = (decl.owner eq sym.owner) || (ownerDeclaration(decl) eq owner)
    // a type operation copies a member of a refinement, or an existential, to another owner
    def isTypeCopy = sym.owner.isRefinementClass || sym.hasFlag(gf.EXISTENTIAL)
    if (rep ne null) return if (symbols.exists(isOriginal) || isTypeCopy) rep else g.NoSymbol

    val originals = symbols.iterator.filter(isOriginal)
    if (originals.hasNext) {
      val original = originals.next()
      return if (originals.hasNext) g.NoSymbol else original
    }

    if (isTypeCopy && symbols.lengthCompare(1) == 0) symbols.head else g.NoSymbol
  }

  private case class DeclarationKey(point: Int, name: g.Name)
  private object DeclarationKey {
    def apply(sym: g.Symbol): DeclarationKey = DeclarationKey(sym.pos.point, sym.name)
  }

  /* The declarations of one key in a unit. A desugaring copies a declaration with its position,
   * and if its name is written at that position, the copies join the representative. */
  private final class Declarations {
    var symbols: List[g.Symbol] = Nil
    var representative: g.Symbol = _
  }

  // the local declarations in the trees of a unit that a document visits
  private final class SourceIndex(source: GSourceFile, val body: g.Tree) {
    val declarations = new mutable.HashMap[DeclarationKey, Declarations]
    val existentials = new mutable.HashMap[DeclarationKey, g.Symbol]

    /* the representative is the last declaration outside synthetic definitions, else the first;
     * a binder that stands for a value is not a declaration */
    private def add(sym: g.Symbol, inSynthetic: Boolean): Unit = {
      val pos = sym.pos
      if (pos.isDefined && (pos.source eq source) && sym.isSemanticdbLocal &&
        !sym.patternValue.exists) {
        val decls = declarations.getOrElseUpdate(DeclarationKey(sym), new Declarations)
        if (!decls.symbols.contains(sym)) {
          decls.symbols = sym :: decls.symbols
          if ((!inSynthetic || (decls.representative eq null)) && isNameWritten(sym, pos))
            decls.representative = sym
        }
      }
    }

    new DocumentTraverser(source) {
      private var syntheticDef: g.Tree = null
      override protected def leave(tree: g.Tree): Unit =
        if (tree eq syntheticDef) syntheticDef = null
      override protected def visit(tree: g.Tree, hasOriginal: Boolean): Unit = tree match {
        case t: g.DefTree if (t.symbol ne null) && t.symbol.exists =>
          val sym = t.symbol
          // the tree of a method has the skolems of the type parameters of its signature
          add(sym.deSkolemize, syntheticDef ne null)
          traverseAnnotations(t)
          if ((syntheticDef eq null) && t.isInstanceOf[g.MemberDef] && sym.isSynthetic)
            syntheticDef = t
        case _ =>
      }
    }.traverse(body)
  }

  private def isNameWritten(sym: g.Symbol, pos: g.Position): Boolean = {
    val text = pos.source.content
    // an existential value `v` has the name `v.type`
    val name = sym.decodedName.stripSuffix(g.nme.LOCAL_SUFFIX_STRING).stripSuffix(".type")
    // scalac gives a placeholder `_` a fresh name
    val isFresh = name.startsWith("_$") || name.startsWith(g.nme.FRESH_TERM_NAME_PREFIX)
    def isPlaceholderAt(at: Int) = isFresh && at < text.length && text(at) == '_'
    isWrittenAt(pos, name) || isPlaceholderAt(pos.start) || isPlaceholderAt(pos.point)
  }

  // the index of a unit of the run, or null; Metals can type a unit again into a new body
  private def sourceIndex(source: GSourceFile): SourceIndex = {
    val body =
      if (source eq documentSource) documentBody
      else {
        val unit = runSource(source)
        if (null eq unit) null else unit.body
      }
    if (null eq body) return null
    sourceIndexes.get(source) match {
      case Some(index) if index.body eq body => index
      case _ =>
        val index = new SourceIndex(source, body)
        sourceIndexes(source) = index
        index
    }
  }

  // the unit of the run, or null; `compileLate` can add units to the run
  private def runSource(source: GSourceFile): g.CompilationUnit = {
    val unit = runSources.get(source)
    val run = g.currentRun
    if ((null ne unit) || (run eq runSourcesRun) && run.size == runSourcesSize) return unit
    runSources.clear()
    run.units.foreach(unit => runSources.put(unit.source, unit))
    runSourcesRun = run
    runSourcesSize = run.size
    runSources.get(source)
  }

}

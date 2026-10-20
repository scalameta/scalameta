package scala.meta.internal.scalacp

import scala.meta.internal.classpath.MissingSymbolException
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d, Names => n}
import scala.meta.internal.semanticdb.SymbolInformation.{Kind => k}
import scala.meta.internal.{semanticdb => s}

import java.util.HashSet

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.reflect.internal.{Flags => gf}
import scala.tools.nsc.backend.jvm.GenBCode
import scala.tools.scalap.scalax.rules.scalasig._

trait SymbolOps {
  self: Scalacp =>

  import SymbolOps._

  lazy val symbolCache = new mutable.HashMap[Symbol, String]
  private lazy val classfileOrderCache = new mutable.HashMap[Symbol, List[Symbol]]
  implicit class XtensionSymbolSSymbol(sym: Symbol) {
    def toSemantic: String = {
      def uncached(sym: Symbol): String =
        if (sym.isSemanticdbGlobal) Symbols.Global(sym.owner, sym.descriptor) else freshSymbol()
      symbolCache.getOrElseUpdate(sym, uncached(sym))
    }
  }

  implicit class XtensionSymbolSSpec(sym: Symbol) {
    def isSemanticdbGlobal: Boolean = !isSemanticdbLocal
    def isSemanticdbLocal: Boolean = {
      val owner = sym.parent.getOrElse(NoSymbol)
      def definitelyGlobal = sym.isPackage
      def definitelyLocal = sym == NoSymbol || owner.isInstanceOf[MethodSymbol] && !sym.isParam ||
        (owner.isAlias || owner.isType && owner.isDeferred) && !sym.isParam ||
        // NOTE: Scalap doesn't expose locals.
        // sym.isSelfParameter ||
        // sym.isLocalDummy ||
        sym.isRefinementClass || sym.isAnonymousClass || sym.isAnonymousFunction ||
        sym.isExistentialType
      def ownerLocal = sym.parent.map(_.isSemanticdbLocal).getOrElse(false)
      !definitelyGlobal && (definitelyLocal || ownerLocal)
    }
    def owner: String =
      if (sym.isRootPackage) Symbols.None
      else if (sym.isEmptyPackage) Symbols.RootPackage
      else if (sym.isToplevelPackage) Symbols.RootPackage
      else sym.parent match {
        case Some(NoSymbol) => ""
        case Some(parent) => parent.ssym
        case None => sys.error(s"unsupported symbol $sym")
      }
    def symbolName: String =
      if (sym.isRootPackage) n.RootPackage.value
      else if (sym.isEmptyPackage) n.EmptyPackage.value
      else if (sym.isConstructor) n.Constructor.value
      else if (sym.isTypeParam) sym.decodedName
      else {
        val name = sym.name.stripSuffix(NameTransformer.LOCAL_SUFFIX_STRING)
        NameTransformer.decode(sym match {
          case sym: SymbolInfoSymbol if name.contains("$$") && isExpandedName(sym) =>
            unexpandedName(name, sym)
          case _ => name
        })
      }
    def descriptor: Descriptor = sym match {
      case sym: SymbolInfoSymbol => sym.kind match {
          case k.LOCAL | k.OBJECT | k.PACKAGE_OBJECT => d.Term(symbolName)
          case k.METHOD if sym.isValMethod => d.Term(symbolName)
          case k.METHOD | k.CONSTRUCTOR | k.MACRO =>
            val overloads = {
              val peers = sym.parent.get.semanticdbDecls.syms
              peers.filter {
                case peer: MethodSymbol => peer.symbolName == sym.symbolName && !peer.isValMethod
                case _ => false
              }
            }
            val disambiguator =
              if (overloads.lengthCompare(1) == 0) "()"
              else {
                val index = overloads.indexOf(sym)
                if (index <= 0) "()" else s"(+$index)"
              }
            d.Method(symbolName, disambiguator)
          case k.TYPE | k.CLASS | k.TRAIT => d.Type(symbolName)
          case k.PACKAGE => d.Package(symbolName)
          case k.PARAMETER => d.Parameter(symbolName)
          case k.TYPE_PARAMETER if sym.name == "_" =>
            // 2.12 names each anonymous type param `_`: number them within the owner
            val tparams = sym.parent.get match {
              case owner: SymbolInfoSymbol => owner.infoType match {
                  case PolyType(_, tparams) => tparams
                  case _ => Nil
                }
              case _ => Nil
            }
            val anonymous = tparams.filter(_.name == "_")
            val index = anonymous.indexOf(sym)
            d.TypeParameter(if (index < 0) symbolName else "_$$" + (index + 1))
          case k.TYPE_PARAMETER => d.TypeParameter(symbolName)
          case skind => sys.error(s"unsupported kind $skind for symbol $sym")
        }
      case sym: ExternalSymbol => symbolIndex.lookup(sym) match {
          case PackageLookup => d.Package(symbolName)
          case JavaLookup => d.Type(symbolName)
          case ScalaLookup if sym.entry.entryType == 9 => d.Type(symbolName)
          case ScalaLookup if sym.entry.entryType == 10 => d.Term(symbolName)
          case ScalaLookup => sys.error(s"unsupported symbol $sym")
          case MissingLookup => throw MissingSymbolException(sym.path)
        }
      case NoSymbol => d.None
    }
    def semanticdbDecls: SemanticdbDecls = {
      // ScalaSig has no source positions, so declarations keep symbol-table order: a companion
      // eagerly created after its class stays there even if declared later (#1544); metac reorders.
      val decls = sym.children
        .filter(decl => decl.isUseful && !decl.isTypeParam && !decl.isExistentialType)
      sym match {
        case cls: ClassSymbol if decls.exists(_.isMovedPrivate) =>
          SemanticdbDecls(
            classfileOrderCache
              .getOrElseUpdate(cls, inClassfileOrder(decls, symbolIndex.methodNames(cls))),
          )
        case _ => SemanticdbDecls(decls)
      }
    }
    // the condition under which scalac's superaccessors phase expands the name of a member
    def isMovedPrivate: Boolean = sym match {
      case sym: SymbolInfoSymbol if !sym.isProtected && !sym.isConstructor =>
        sym.symbolInfo.privateWithin match {
          case Some(owner: ClassSymbol) => !owner.isModule
          case _ => false
        }
      case _ => false
    }
    def isMethodDecl: Boolean = sym.isInstanceOf[MethodSymbol] && sym.isMethod && !sym.isConstructor
  }

  /* The superaccessors phase expands the name of a member that is private to an enclosing class
   * and re-enters it at the end of its owner's scope, so the pickle lists it last (#1590). The
   * classfile keeps the methods in source order, so move each such method back before the first
   * other method that follows it there. Objects, classes and types have no method in that order,
   * so they stay last. */
  private def inClassfileOrder(decls: Seq[Symbol], methods: IndexedSeq[String]): List[Symbol] = {
    /* A private method that the companion calls takes the same expanded name in the classfile,
     * so count the classfile methods of each moved name less the moved methods of that name, and
     * keep the pickle order of a name whose count is not 0. */
    val surplus = mutable.HashMap.empty[String, Int]
    decls.foreach(decl =>
      if (decl.isMethodDecl && decl.isMovedPrivate)
        surplus(decl.name) = surplus.getOrElse(decl.name, 0) - 1,
    )
    methods.foreach(name => surplus.get(name).foreach(n => surplus(name) = n + 1))
    val slots = new Array[Symbol](methods.length)
    // the first classfile method from `from` with this name whose slot is free, or -1
    def slotOf(name: String, from: Int): Int = {
      var i = from
      while (i < methods.length && ((slots(i) ne null) || methods(i) != name)) i += 1
      if (i == methods.length) -1 else i
    }
    val isMoved = new Array[Boolean](decls.length)
    var declsIdx = 0
    var declsIter = decls.iterator
    while (declsIter.hasNext) {
      val decl = declsIter.next()
      if (decl.isMethodDecl && decl.isMovedPrivate && surplus(decl.name) == 0) {
        val i = slotOf(decl.name, 0)
        if (i >= 0) {
          slots(i) = decl
          isMoved(declsIdx) = true
        }
      }
      declsIdx += 1
    }
    val buf = List.newBuilder[Symbol]
    // add the moved members of the slots in [from, end), and return the first slot not released
    def releaseSlots(from: Int, end: Int): Int = {
      var slotIdx = from
      while (slotIdx < end) {
        val moved = slots(slotIdx)
        if (moved ne null) buf += moved
        slotIdx += 1
      }
      slotIdx
    }
    var released = 0
    var anchorFrom = 0
    declsIdx = 0
    declsIter = decls.iterator
    while (declsIter.hasNext) {
      val decl = declsIter.next()
      if (!isMoved(declsIdx)) {
        if (decl.isMethodDecl && !decl.isMovedPrivate) {
          val i = slotOf(decl.name, anchorFrom)
          if (i >= 0) {
            released = releaseSlots(released, i)
            anchorFrom = i + 1
          }
        }
        buf += decl
      }
      declsIdx += 1
    }
    releaseSlots(released, slots.length)
    buf.result()
  }

  implicit class XtensionSymbolsSSpec(syms: Seq[Symbol]) {
    def semanticdbDecls: SemanticdbDecls = SemanticdbDecls(syms.filter(_.isUseful))

    def sscope(linkMode: LinkMode): s.Scope = linkMode match {
      case SymlinkChildren => s.Scope(symlinks = syms.map(_.ssym))
      case HardlinkChildren =>
        syms.map(registerHardlink)
        val hardlinks = syms.map {
          case sym: SymbolInfoSymbol => sym.toSymbolInformation(HardlinkChildren)
          case sym => sys.error(s"unsupported symbol $sym")
        }
        s.Scope(hardlinks = hardlinks)
    }
  }

  case class SemanticdbDecls(syms: Seq[Symbol]) {
    def sscope(linkMode: LinkMode): s.Scope = linkMode match {
      case SymlinkChildren =>
        val sbuf = List.newBuilder[String]
        syms.foreach { sym =>
          val ssym = sym.ssym
          sbuf += ssym
          if (sym.isUsefulField && sym.isMutable) {
            val setterSymbolName = s"${ssym.desc.name}_="
            val setterSym = Symbols.Global(ssym.owner, d.Method(setterSymbolName, "()"))
            sbuf += setterSym
          }
        }
        s.Scope(sbuf.result())
      case HardlinkChildren =>
        val sbuf = List.newBuilder[s.SymbolInformation]
        syms.foreach { sym =>
          registerHardlink(sym)
          val sinfo = sym match {
            case sym: SymbolInfoSymbol => sym.toSymbolInformation(HardlinkChildren)
            case sym => sys.error(s"unsupported symbol $sym")
          }
          sbuf += sinfo
          if (sym.isUsefulField && sym.isMutable) Synthetics.setterInfos(sinfo, HardlinkChildren)
            .foreach(sbuf.+=)
        }
        s.Scope(hardlinks = sbuf.result())
    }
  }

  implicit class XtensionSymbol(sym: Symbol) {

    def ssym: String = sym.toSemantic
    def self: Type = sym match {
      case sym: ClassSymbol => sym.selfType.map {
          case RefinedType(_, List(_, self)) => self
          case _ => NoType
        }.getOrElse(NoType)
      case _ => NoType
    }
    def isRootPackage: Boolean = sym.path == "<root>"
    def isEmptyPackage: Boolean = sym.path == "<empty>"
    def isToplevelPackage: Boolean = sym.parent.isEmpty
    def isModuleClass: Boolean = sym.isInstanceOf[ClassSymbol] && sym.isModule
    def moduleClass: Symbol = sym match {
      case sym: SymbolInfoSymbol if sym.isModule =>
        sym.infoType match {
          case TypeRefType(_, moduleClass, _) => moduleClass
          case _ => NoSymbol
        }
      case _ => NoSymbol
    }
    def isClass: Boolean = sym.isInstanceOf[ClassSymbol] && !sym.isModule
    def isObject: Boolean = sym.isInstanceOf[ObjectSymbol]
    def isType: Boolean = sym.isInstanceOf[TypeSymbol]
    def isAlias: Boolean = sym.isInstanceOf[AliasSymbol]
    def isMacro: Boolean = sym.isMethod && sym.hasFlag(gf.MACRO)
    def isConstructor: Boolean = sym.isMethod &&
      (sym.name == GenBCode.INSTANCE_CONSTRUCTOR_NAME || sym.name == "$init$")
    def isPackageObject: Boolean = sym.name == "package"
    def isTypeParam = sym.isType && sym.isParam
    def isExistentialType: Boolean = sym match {
      case sym: TypeSymbol => sym.hasLongFlag(gf.EXISTENTIAL)
      case _ => false
    }
    def isAnonymousClass = decodedName.contains("$anon")
    def isAnonymousFunction = decodedName.contains("$anonfun")
    def isSyntheticConstructor = sym match {
      case sym: SymbolInfoSymbol =>
        val owner = sym.symbolInfo.owner
        sym.isConstructor && owner.isModuleClass
      case _ => false
    }
    def isLocalChild: Boolean = sym.name == "<local child>"
    def isLocalDummy: Boolean = sym.parent.exists(owner => sym.name == s"<local ${owner.name}>")
    def isExtensionMethod: Boolean = decodedName.contains("$extension")
    def isSyntheticValueClassCompanion: Boolean = sym match {
      case sym: SymbolInfoSymbol =>
        if (sym.isModuleClass) sym.infoType match {
          case ClassInfoType(_, List(TypeRefType(_, anyRef, _))) => sym.isSynthetic &&
            sym.semanticdbDecls.syms.isEmpty
          case _ => false
        }
        else if (sym.isModule) sym.moduleClass.isSyntheticValueClassCompanion
        else false
      case _ => false
    }
    def isValMethod: Boolean = sym match {
      case sym: SymbolInfoSymbol => sym.kind.isMethod && {
          sym.isAccessor && sym.isStable || isUsefulField && !sym.isMutable
        }
      case _ => false
    }
    def isScalacField: Boolean = {
      val isField = sym.isInstanceOf[MethodSymbol] && !sym.isMethod && !sym.isParam
      val isJavaDefined = sym.isJava
      isField && !isJavaDefined
    }
    def isUselessField: Boolean = {
      val peers = sym.parent.map(_.children.toList).getOrElse(Nil)
      val getter = peers.find(m =>
        m.isAccessor && m.name == sym.name.stripSuffix(NameTransformer.LOCAL_SUFFIX_STRING),
      )
      sym.isScalacField && getter.nonEmpty
    }
    def isUsefulField: Boolean = sym.isScalacField && !sym.isUselessField
    def isSyntheticCaseAccessor: Boolean = sym.isCaseAccessor && decodedName.contains("$")
    def isRefinementClass: Boolean = sym.name == "<refinement>"
    def isUseless: Boolean = sym == NoSymbol || sym.isAnonymousClass ||
      sym.isSyntheticConstructor || sym.isModuleClass || sym.isLocalChild || sym.isLocalDummy ||
      sym.isExtensionMethod || sym.isSyntheticValueClassCompanion || sym.isUselessField ||
      sym.isSyntheticCaseAccessor || sym.isRefinementClass
    def isUseful: Boolean = !sym.isUseless
    def isDefaultParameter: Boolean = sym.hasFlag(gf.DEFAULTPARAM) && sym.isParam

    def decodedName: String = NameTransformer.decode(sym.name)

  }

  /* scalac prefixed the name with the full name of an enclosing class and `$$`: the class that a
   * qualified private member is private to, or the owner of a private member */
  @tailrec
  private def unexpandedName(name: String, sym: Symbol): String = enclosingClass(sym) match {
    case Some(cls: ClassSymbol) =>
      val stripped = name.stripPrefix(fullName(cls) + "$$")
      if (stripped ne name) stripped else unexpandedName(name, cls)
    case _ => name
  }

  // scalac's `fullName('$')`
  private def fullName(sym: Symbol): String = enclosingClass(sym) match {
    case Some(owner) if !owner.isRootPackage && !owner.isEmptyPackage =>
      fullName(owner) + NameTransformer.NAME_JOIN_STRING + sym.name
    case _ => sym.name
  }

  @tailrec // scalac's `effectiveOwner.enclClass`
  private def enclosingClass(sym: Symbol): Option[Symbol] = sym.parent match {
    case Some(owner) if owner.isPackageObject || owner.isInstanceOf[MethodSymbol] =>
      enclosingClass(owner)
    case owner => owner
  }

  private var nextId = 0
  private def freshSymbol(): String =
    try Symbols.Local(nextId)
    finally nextId += 1

  lazy val hardlinks = new HashSet[String]
  private def registerHardlink(sym: Symbol): Unit = hardlinks.add(sym.ssym)

}

object SymbolOps {

  /* scalap reads the flags as an Int and drops the bits from 32 up, such as EXISTENTIAL (bit 35),
   * so read them again: name_Ref owner_Ref flags_LongNat, after a Nat if the tag is over 64 */
  private def pickledFlags(sym: SymbolInfoSymbol): Long = {
    val entry = sym.entry.scalaSig.getEntry(sym.index)
    val bytes = entry.byteCode.bytes
    var pos = entry.byteCode.pos
    var left = if (entry.entryType > 64) 4 else 3
    var nat = 0L
    while (left > 0) {
      nat = 0L
      var b = 0x80
      while ((b & 0x80) != 0) {
        b = bytes(pos)
        pos += 1
        nat = nat << 7 | b & 0x7f
      }
      left -= 1
    }
    nat
  }

  private implicit class XtensionSymbolInfoSymbol(val sym: SymbolInfoSymbol) extends AnyVal {
    def hasLongFlag(flag: Long): Boolean = (pickledFlags(sym) & flag) != 0
  }

  private def isExpandedName(sym: SymbolInfoSymbol): Boolean = sym.hasLongFlag(gf.EXPANDEDNAME)

}

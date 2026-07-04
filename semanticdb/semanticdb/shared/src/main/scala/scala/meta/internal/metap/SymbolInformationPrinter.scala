package scala.meta.internal.metap

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.Kind._
import scala.meta.internal.semanticdb.SymbolInformation._
import scala.meta.internal.semanticdb._

import scala.collection.mutable
import scala.math.Ordering

object SymbolInformationPrinter {

  // One row per modifier, in print order (shared by both styles).
  //   prop   the SemanticDB property bit this row renders.
  //   word   how the property is printed. This is the rule for every style; `defn` and `defnOK`
  //          are the only exceptions to it.
  //   defn   an occasional Definition-only deviation from `word`: null (default) keeps `word`,
  //          so most rows omit this arg; "" drops the modifier, which Definition does not model
  //          (synthetic, primary, default, given, inline, open, transparent, infix, opaque); any
  //          other string replaces it (variance prints +/- rather than covariant/contravariant).
  //   defnOK an extra Definition gate keyed on kind, not property, for the two modifiers Scala
  //          keeps only in some positions: `abstract` only on classes, `final` only off objects
  //          (Scala implies it there). Declaration ignores it.
  private final case class Modifier(
      prop: Property,
      word: String,
      defn: String = null,
      defnOK: SymbolInformation => Boolean = _ => true,
  )

  private val modifiers: List[Modifier] = List(
    Modifier(Property.SYNTHETIC, "synthetic ", ""),
    Modifier(Property.ABSTRACT, "abstract ", defnOK = _.isClass),
    Modifier(Property.FINAL, "final ", defnOK = !_.isObject),
    Modifier(Property.SEALED, "sealed "),
    Modifier(Property.IMPLICIT, "implicit "),
    Modifier(Property.LAZY, "lazy "),
    Modifier(Property.CASE, "case "),
    Modifier(Property.COVARIANT, "covariant ", "+"),
    Modifier(Property.CONTRAVARIANT, "contravariant ", "-"),
    Modifier(Property.VAL, "val "),
    Modifier(Property.VAR, "var "),
    Modifier(Property.STATIC, "static "),
    Modifier(Property.PRIMARY, "primary ", ""),
    Modifier(Property.ENUM, "enum "),
    Modifier(Property.DEFAULT, "default ", ""),
    Modifier(Property.GIVEN, "given ", ""),
    Modifier(Property.INLINE, "inline ", ""),
    Modifier(Property.OPEN, "open ", ""),
    Modifier(Property.TRANSPARENT, "transparent ", ""),
    Modifier(Property.INFIX, "infix ", ""),
    Modifier(Property.OPAQUE, "opaque ", ""),
  )
}

trait SymbolInformationPrinter extends BasePrinter {

  def pprint(info: SymbolInformation): Unit = {
    out.print(info.symbol)
    out.print(" => ")

    val infoNotes = new InfoNotes
    val infoPrinter = new InfoPrinter(infoNotes)
    infoPrinter.pprint(info)
    out.println()

    if (settings.format.isDetailed) {
      val printed = mutable.Set[String]()
      infoNotes.visited.tail.foreach(info =>
        if (!printed(info.symbol)) {
          printed += info.symbol
          out.print("  ")
          out.print(info.displayName)
          out.print(" => ")
          out.println(info.symbol)
        },
      )
    }
  }

  class InfoPrinter(notes: InfoNotes) {
    def pprint(info: SymbolInformation): Unit = {
      notes.visit(info)
      printInfo(info, Declaration)
      info.overriddenSymbols match {
        case Nil => ()
        case all => out.print(" <: " + all.mkString(", "))
      }
    }

    private def pprint(ann: AnnotationTree): Unit = {
      out.print("@")
      ann.tpe match {
        case NoType => out.print("<?>")
        case tpe => pprint(tpe)
      }
    }

    private def pprint(acc: Access): Unit = acc match {
      case PrivateAccess() => out.print("private ")
      case PrivateThisAccess() => out.print("private[this] ")
      case PrivateWithinAccess(sym) =>
        out.print("private[")
        pprint(sym, Reference)
        out.print("] ")
      case ProtectedAccess() => out.print("protected ")
      case ProtectedThisAccess() => out.print("protected[this] ")
      case ProtectedWithinAccess(sym) =>
        out.print("protected[")
        pprint(sym, Reference)
        out.print("] ")
      case NoAccess | _: PublicAccess => out.print("")
    }

    def pprint(sig: Signature): Unit = sig match {
      case ClassSignature(tparams, parents, self, decls) =>
        rep("[", tparams.infos, ", ", "]")(pprintDefn)
        rep(" extends ", parents, " with ")(pprint)
        if (self.nonEmpty || decls.infos.nonEmpty) out.print(" { ")
        if (self.nonEmpty) {
          out.print("self: ")
          pprint(self)
          out.print(" => ")
        }
        if (decls.infos.nonEmpty) out.print(s"+${decls.infos.length} decls")
        if (self.nonEmpty || decls.infos.nonEmpty) out.print(" }")
      case sig: MethodSignature =>
        rep("[", sig.typeParameters.infos, ", ", "]")(pprintDefn)
        rep("(", sig.parameterLists, ")(", ")")(params => rep(params.infos, ", ")(pprintDefn))
        rep(" throws ", sig.throws, ", ", "")(pprint)
        opt(": ", sig.returnType)(pprint)
      case TypeSignature(tparams, lo, hi) =>
        rep("[", tparams.infos, ", ", "]")(pprintDefn)
        if (lo != hi) {
          lo match {
            case TypeRef(NoType, "scala/Nothing#", Nil) => ()
            case lo => opt(" >: ", lo)(pprint)
          }
          hi match {
            case TypeRef(NoType, "scala/Any#", Nil) => ()
            case TypeRef(NoType, "java/lang/Object#", Nil) => ()
            case hi => opt(" <: ", hi)(pprint)
          }
        } else {
          val alias = lo
          opt(" = ", alias)(pprint)
        }
      case ValueSignature(tpe) => pprint(tpe)
      case NoSignature => out.print("<?>")
    }

    def pprint(tpe: Type): Unit = {
      def prefix(tpe: Type): Unit = tpe match {
        case TypeRef(pre, sym, args) =>
          pre match {
            case _: SingleType | _: ThisType | _: SuperType =>
              prefix(pre)
              out.print(".")
            case NoType => ()
            case _ =>
              prefix(pre)
              out.print("#")
          }
          pprintRef(sym)
          rep("[", args, ", ", "]")(normal)
        case SingleType(pre, sym) =>
          opt(pre, ".")(prefix)
          pprintRef(sym)
        case ThisType(sym) =>
          opt(sym, ".")(pprintRef)
          out.print("this")
        case SuperType(pre, sym) =>
          opt(pre, ".")(prefix)
          out.print("super")
          opt("[", sym, "]")(pprintRef)
        case ConstantType(const) => pprint(const)
        case IntersectionType(types) => rep(types, " & ")(normal)
        case UnionType(types) => rep(types, " | ")(normal)
        case WithType(types) => rep(types, " with ")(normal)
        case StructuralType(utpe, decls) =>
          decls.infos.foreach(notes.discover)
          opt(utpe)(normal)
          if (decls.infos.nonEmpty) rep(" { ", decls.infos, "; ", " }")(pprintDefn)
          else out.print(" {}")
        case AnnotatedType(anns, utpe) =>
          opt(utpe)(normal)
          out.print(" ")
          rep(anns, " ", "")(pprint)
        case ExistentialType(utpe, decls) =>
          decls.infos.foreach(notes.discover)
          opt(utpe)(normal)
          rep(" forSome { ", decls.infos, "; ", " }")(pprintDefn)
        case UniversalType(tparams, utpe) =>
          tparams.infos.foreach(notes.discover)
          rep("[", tparams.infos, ", ", "] => ")(pprintDefn)
          opt(utpe)(normal)
        case ByNameType(utpe) =>
          out.print("=> ")
          opt(utpe)(normal)
        case RepeatedType(utpe) =>
          opt(utpe)(normal)
          out.print("*")
        case MatchType(scrutinee, cases) =>
          opt(scrutinee)(normal)
          rep(" match { ", cases, ", ", " }") { kase =>
            opt(kase.key)(normal)
            out.print(" => ")
            opt(kase.body)(normal)
          }
        case LambdaType(parameters, returnType) =>
          parameters.infos.foreach(notes.discover)
          rep("[", parameters.infos, ", ", "] =>> ")(pprintDefn)
          opt(returnType)(normal)
        case NoType => out.print("<?>")
      }
      def normal(tpe: Type): Unit = tpe match {
        case _: SingleType | _: ThisType | _: SuperType =>
          prefix(tpe)
          out.print(".type")
        case _ => prefix(tpe)
      }
      normal(tpe)
    }

    private def pprintRef(sym: String): Unit = pprint(sym, Reference)

    private def pprintDefn(info: SymbolInformation): Unit = {
      notes.discover(info)
      pprint(info.symbol, Definition)
    }

    protected sealed trait SymbolStyle
    // InfoStyle renders the full symbol information; Reference renders just the display name.
    // Keeping Reference outside InfoStyle means printInfo cannot be reached with a style that
    // would silently print a name and signature without modifiers or kind.
    protected sealed trait InfoStyle extends SymbolStyle
    protected case object Declaration extends InfoStyle
    protected case object Reference extends SymbolStyle
    protected case object Definition extends InfoStyle

    protected def pprint(sym: String, style: SymbolStyle): Unit = {
      val info = notes.visit(sym)
      style match {
        case Reference => pprint(info.displayName)
        case style: InfoStyle => printInfo(info, style)
      }
    }

    private def printInfo(info: SymbolInformation, style: InfoStyle): Unit = {
      rep(info.annotations, " ", " ")(pprint)
      pprint(info.access)
      printModifiers(info, style)
      printKind(info, style)
      pprint(info.displayName)
      printSignature(info)
    }

    // See SymbolInformationPrinter.modifiers for the per-style words. Declaration prints the
    // SemanticDB word for every set modifier; Definition prints its Scala-like word, skipping
    // the modifiers it does not model and honouring each modifier's positional guard.
    private def printModifiers(info: SymbolInformation, style: InfoStyle): Unit = {
      val props = info.properties
      SymbolInformationPrinter.modifiers.foreach(m =>
        if ((props & m.prop.value) != 0) {
          val word = style match {
            case Declaration => m.word
            case Definition if m.defnOK(info) => if (m.defn ne null) m.defn else m.word
            case _ => ""
          }
          if (word.nonEmpty) out.print(word)
        },
      )
    }

    // Declaration prints SemanticDB kind names (method, ctor, param, ...); Definition prints
    // the closest Scala keyword: `def` for both methods and constructors, and no keyword at
    // all for locals, fields, params and type params. Only those deltas are style-specific;
    // every other kind renders identically, so it is spelled out once below.
    private def printKind(info: SymbolInformation, style: InfoStyle): Unit = {
      val word = info.kind match {
        case METHOD | CONSTRUCTOR if style == Definition => "def "
        case LOCAL | FIELD | PARAMETER | SELF_PARAMETER | TYPE_PARAMETER if style == Definition =>
          ""
        case CONSTRUCTOR => "ctor "
        case PARAMETER => "param "
        case SELF_PARAMETER => "selfparam "
        case TYPE_PARAMETER => "typeparam "
        case PACKAGE_OBJECT => "package object "
        case UNKNOWN_KIND | _: Kind.Unrecognized => "unknown "
        case _ => s"${info.kind.name.toLowerCase} "
      }
      if (word.nonEmpty) out.print(word)
    }

    private def printSignature(info: SymbolInformation): Unit = info.signature match {
      case NoSignature if info.isSelfParameter => ()
      case _ => opt(info.prefixBeforeTpe, info.signature)(pprint)
    }

    private def pprint(name: String): Unit = if (name.nonEmpty) out.print(name) else out.print("<?>")

    def pprint(const: Constant): Unit = const match {
      case NoConstant => out.print("<?>")
      case _: UnitConstant => out.print("()")
      case x: BooleanConstant => out.print(x.value)
      case x: ByteConstant => out.print(x.value.toByte)
      case x: ShortConstant => out.print(x.value.toShort)
      case x: CharConstant => out.print("'" + x.value.toChar + "'")
      case x: IntConstant => out.print(x.value)
      case x: LongConstant => out.print(s"${x.value}L")
      case x: FloatConstant => out.print(s"${x.value}f")
      case x: DoubleConstant => out.print(x.value)
      case x: StringConstant => out.print("\"" + x.value + "\"")
      case _: NullConstant => out.print("null")
    }

    private implicit class InfoOps(info: SymbolInformation) {
      def prefixBeforeTpe: String = info.kind match {
        case LOCAL | FIELD | PARAMETER | SELF_PARAMETER | UNKNOWN_KIND | _: Kind.Unrecognized =>
          ": "
        case METHOD | CONSTRUCTOR | MACRO | TYPE | TYPE_PARAMETER | OBJECT | PACKAGE |
            PACKAGE_OBJECT | CLASS | TRAIT | INTERFACE => ""
        case _ => ""
      }
    }
  }

  class InfoNotes {
    private val buf = mutable.ListBuffer[SymbolInformation]()
    private val noteSymtab = mutable.Map[String, SymbolInformation]()

    def discover(info: SymbolInformation): Unit =
      if (symtab.info(info.symbol).isEmpty && info.kind != UNKNOWN_KIND)
        noteSymtab(info.symbol) = info

    def visit(sym: String): SymbolInformation = {
      val symtabInfo = noteSymtab.get(sym).orElse(symtab.info(sym))
      val info = symtabInfo.getOrElse {
        val displayName = if (sym.isGlobal) sym.desc.value else sym
        SymbolInformation(
          symbol = sym,
          displayName = displayName,
          annotations = Nil,
          overriddenSymbols = Nil,
        )
      }
      visit(info)
    }

    def visit(info: SymbolInformation): SymbolInformation = {
      buf.append(info)
      info
    }

    def visited: List[SymbolInformation] = buf.toList
  }

  implicit def infoOrder: Ordering[SymbolInformation] = Ordering
    .by[SymbolInformation, String](_.symbol)(new IdentifierOrdering)
}

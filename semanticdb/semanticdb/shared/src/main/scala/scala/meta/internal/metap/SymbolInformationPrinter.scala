package scala.meta.internal.metap

import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.Kind._
import scala.meta.internal.semanticdb.SymbolInformation._
import scala.meta.internal.semanticdb._

import scala.collection.mutable

object SymbolInformationPrinter {

  // One row per modifier, in print order (shared by both styles).
  //   prop   the SemanticDB property bit this row renders.
  //   word   how the property is printed. This is the rule for every style; `defn` and `defnOK`
  //          are the only exceptions to it.
  //   defn   an occasional Definition-only deviation from `word`: null (default) keeps `word`,
  //          so most rows omit this arg; "" drops the modifier, which Definition does not model
  //          (synthetic, primary, default, given, inline, open, transparent, infix, opaque,
  //          extension); any other string replaces it (variance prints +/- rather than
  //          covariant/contravariant).
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
    Modifier(Property.EXTENSION, "extension ", ""),
    Modifier(Property.NATIVE, "native "),
    Modifier(Property.SYNCHRONIZED, "synchronized "),
    Modifier(Property.TRANSIENT, "transient "),
    Modifier(Property.VOLATILE, "volatile "),
  )
}

trait SymbolInformationPrinter extends BasePrinter {

  def pprint(info: SymbolInformation): Unit = {
    str(info.symbol)
    str(" => ")

    val infoNotes = new InfoNotes
    val infoPrinter = new InfoPrinter(infoNotes)
    infoPrinter.pprint(info)
    out.println()

    if (settings.format.isDetailed) {
      val printed = mutable.Set[String]()
      infoNotes.visited.tail.foreach(info =>
        if (!printed(info.symbol)) {
          printed += info.symbol
          str("  ")
          str(info.displayName)
          str(" => ")
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
        case Nil =>
        case all => str(all.mkString(" <: ", ", ", ""))
      }
    }

    private def pprint(ann: AnnotationTree): Unit = {
      str("@")
      pprint(ann.tpe)
    }

    private def pprint(acc: Access): Unit = acc match {
      case _: PrivateAccess => str("private ")
      case _: PrivateThisAccess => str("private[this] ")
      case acc: PrivateWithinAccess =>
        str("private[")
        pprint(acc.symbol, Reference)
        str("] ")
      case _: ProtectedAccess => str("protected ")
      case _: ProtectedThisAccess => str("protected[this] ")
      case acc: ProtectedWithinAccess =>
        str("protected[")
        pprint(acc.symbol, Reference)
        str("] ")
      case NoAccess | _: PublicAccess => str("")
    }

    def pprint(sig: Signature): Unit = sig match {
      case sig: ClassSignature =>
        rep("[", sig.typeParameters.infos, ", ", "]")(pprintDefn)
        rep(" extends ", sig.parents, " with ")(pprint)
        val inBraces = sig.self.nonEmpty || sig.declarations.infos.nonEmpty
        if (inBraces) str(" { ")
        if (sig.self.nonEmpty) {
          str("self: ")
          pprint(sig.self)
          str(" => ")
        }
        if (sig.declarations.infos.nonEmpty) str(s"+${sig.declarations.infos.length} decls")
        if (inBraces) str(" }")
      case sig: MethodSignature =>
        rep("[", sig.typeParameters.infos, ", ", "]")(pprintDefn)
        rep("(", sig.parameterLists, ")(", ")")(params => rep(params.infos, ", ")(pprintDefn))
        rep(" throws ", sig.throws, ", ", "")(pprint)
        opt(": ", sig.returnType)(pprint)
      case sig: TypeSignature =>
        rep("[", sig.typeParameters.infos, ", ", "]")(pprintDefn)
        if (sig.lowerBound == sig.upperBound) opt(" = ", sig.lowerBound)(pprint)
        else {
          sig.lowerBound match {
            case TypeRef(NoType, "scala/Nothing#", Nil) => ()
            case lo => opt(" >: ", lo)(pprint)
          }
          sig.upperBound match {
            case TypeRef(NoType, "scala/Any#", Nil) => ()
            case TypeRef(NoType, "java/lang/Object#", Nil) => ()
            case hi => opt(" <: ", hi)(pprint)
          }
        }
      case sig: ValueSignature => pprint(sig.tpe)
      case NoSignature => str("<?>")
    }

    def pprint(tpe: Type): Unit = pprintTpe(tpe, outer = true)

    private def pprintInnerTpe(tpe: Type): Unit = pprintTpe(tpe, outer = false)

    private def pprintTpe(tpe: Type, outer: Boolean): Unit = tpe match {
      case tpe: TypeRef =>
        tpe.prefix match {
          case pre @ (_: SingleType | _: ThisType | _: SuperType) =>
            pprintInnerTpe(pre)
            str(".")
          case NoType => ()
          case pre =>
            pprintInnerTpe(pre)
            str("#")
        }
        pprintRef(tpe.symbol)
        rep("[", tpe.typeArguments, ", ", "]")(pprint)
      case tpe: SingleType =>
        opt(tpe.prefix, ".")(pprintInnerTpe)
        pprintRef(tpe.symbol)
        if (outer) str(".type")
      case tpe: ThisType =>
        opt(tpe.symbol, ".")(pprintRef)
        str("this")
        if (outer) str(".type")
      case tpe: SuperType =>
        opt(tpe.prefix, ".")(pprintInnerTpe)
        str("super")
        opt("[", tpe.symbol, "]")(pprintRef)
        if (outer) str(".type")
      case tpe: ConstantType => pprint(tpe.constant)
      case tpe: IntersectionType => rep(tpe.types, " & ")(pprint)
      case tpe: UnionType => rep(tpe.types, " | ")(pprint)
      case tpe: WithType => rep(tpe.types, " with ")(pprint)
      case tpe: StructuralType =>
        notes.discover(tpe.declarations.infos)
        opt(tpe.tpe)(pprint)
        if (tpe.declarations.infos.isEmpty) str(" {}")
        else rep(" { ", tpe.declarations.infos, "; ", " }")(pprintDefn)
      case tpe: AnnotatedType =>
        opt(tpe.tpe)(pprint)
        str(" ")
        rep(tpe.annotations, " ")(pprint)
      case tpe: ExistentialType =>
        notes.discover(tpe.declarations.infos)
        opt(tpe.tpe)(pprint)
        rep(" forSome { ", tpe.declarations.infos, "; ", " }")(pprintDefn)
      case tpe: UniversalType =>
        notes.discover(tpe.typeParameters.infos)
        rep("[", tpe.typeParameters.infos, ", ", "] => ")(pprintDefn)
        opt(tpe.tpe)(pprint)
      case tpe: ByNameType =>
        str("=> ")
        opt(tpe.tpe)(pprint)
      case tpe: RepeatedType =>
        opt(tpe.tpe)(pprint)
        str("*")
      case tpe: MatchType =>
        opt(tpe.scrutinee)(pprint)
        rep(" match { ", tpe.cases, ", ", " }") { kase =>
          opt(kase.key)(pprint)
          str(" => ")
          opt(kase.body)(pprint)
        }
      case tpe: LambdaType =>
        notes.discover(tpe.parameters.infos)
        rep("[", tpe.parameters.infos, ", ", "] =>> ")(pprintDefn)
        opt(tpe.returnType)(pprint)
      case NoType => str("<?>")
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
        case Reference => pprintName(info.displayName)
        case style: InfoStyle => printInfo(info, style)
      }
    }

    private def printInfo(info: SymbolInformation, style: InfoStyle): Unit = {
      rep(info.annotations, " ", " ")(pprint)
      pprint(info.access)
      printModifiers(info, style)
      printKind(info, style)
      pprintName(info.displayName)
      printSignature(info)
    }

    // See SymbolInformationPrinter.modifiers for the per-style words. Declaration prints the
    // SemanticDB word for every set modifier; Definition prints its Scala-like word, skipping
    // the modifiers it does not model and honouring each modifier's positional guard.
    private def printModifiers(info: SymbolInformation, style: InfoStyle): Unit = {
      val props = info.properties
      SymbolInformationPrinter.modifiers.foreach(m =>
        if ((props & m.prop.value) != 0) str(style match {
          case Declaration => m.word
          case Definition if m.defnOK(info) => if (m.defn ne null) m.defn else m.word
          case _ => ""
        }),
      )
    }

    // Declaration prints SemanticDB kind names (method, ctor, param, ...); Definition prints
    // the closest Scala keyword: `def` for both methods and constructors, and no keyword at
    // all for locals, fields, params and type params. Only those deltas are style-specific;
    // every other kind renders identically, so it is spelled out once below.
    private def printKind(info: SymbolInformation, style: InfoStyle): Unit = str {
      info.kind match {
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
    }

    private def printSignature(info: SymbolInformation): Unit = info.signature match {
      case NoSignature if info.isSelfParameter => ()
      case _ => opt(info.prefixBeforeTpe, info.signature)(pprint)
    }

    private def pprintName(name: String): Unit = str(if (name.nonEmpty) name else "<?>")

    def pprint(const: Constant): Unit = const match {
      case NoConstant => str("<?>")
      case _: UnitConstant => str("()")
      case x: BooleanConstant => out.print(x.value)
      case x: ByteConstant => out.print(x.value.toByte)
      case x: ShortConstant => out.print(x.value.toShort)
      case x: CharConstant => str("'" + x.value.toChar + "'")
      case x: IntConstant => out.print(x.value)
      case x: LongConstant => str(s"${x.value}L")
      case x: FloatConstant => str(s"${x.value}f")
      case x: DoubleConstant => out.print(x.value)
      case x: StringConstant => str("\"" + x.value + "\"")
      case _: NullConstant => str("null")
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

    def discover(infos: Iterable[SymbolInformation]): Unit = infos.foreach(discover)

    def visit(sym: String): SymbolInformation = {
      val symtabInfo = noteSymtab.get(sym).orElse(symtab.info(sym))
      visit(symtabInfo.getOrElse(SymbolInformation(
        symbol = sym,
        displayName = if (sym.isGlobal) sym.desc.value else sym,
        annotations = Nil,
        overriddenSymbols = Nil,
      )))
    }

    def visit(info: SymbolInformation): SymbolInformation = {
      buf.append(info)
      info
    }

    def visited: List[SymbolInformation] = buf.toList
  }
}

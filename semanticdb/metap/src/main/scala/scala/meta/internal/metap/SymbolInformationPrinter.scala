package scala.meta.internal.metap

import java.io.ByteArrayOutputStream
import java.io.PrintStream
import scala.collection.mutable
import scala.math.Ordering
import scala.meta.cli.Reporter
import scala.meta.internal.semanticdb._
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation._
import scala.meta.internal.semanticdb.SymbolInformation.Kind._
import scala.meta.metap.Format
import scala.meta.metap.Settings

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
      infoNotes.visited.tail.foreach { info =>
        if (!printed(info.symbol)) {
          printed += info.symbol
          out.print("  ")
          out.print(info.displayName)
          out.print(" => ")
          out.println(info.symbol)
        }
      }
    }
  }

  class InfoPrinter(notes: InfoNotes) {
    def pprint(info: SymbolInformation): Unit = {
      notes.visit(info)
      rep(info.annotations, " ", " ")(pprint)
      pprint(info.access)
      if (info.isAbstract) out.print("abstract ")
      if (info.isFinal) out.print("final ")
      if (info.isSealed) out.print("sealed ")
      if (info.isImplicit) out.print("implicit ")
      if (info.isLazy) out.print("lazy ")
      if (info.isCase) out.print("case ")
      if (info.isCovariant) out.print("covariant ")
      if (info.isContravariant) out.print("contravariant ")
      if (info.isVal) out.print("val ")
      if (info.isVar) out.print("var ")
      if (info.isStatic) out.print("static ")
      if (info.isPrimary) out.print("primary ")
      if (info.isEnum) out.print("enum ")
      if (info.isDefault) out.print("default ")
      info.kind match {
        case LOCAL => out.print("local ")
        case FIELD => out.print("field ")
        case METHOD => out.print("method ")
        case CONSTRUCTOR => out.print("ctor ")
        case MACRO => out.print("macro ")
        case TYPE => out.print("type ")
        case PARAMETER => out.print("param ")
        case SELF_PARAMETER => out.print("selfparam ")
        case TYPE_PARAMETER => out.print("typeparam ")
        case OBJECT => out.print("object ")
        case PACKAGE => out.print("package ")
        case PACKAGE_OBJECT => out.print("package object ")
        case CLASS => out.print("class ")
        case TRAIT => out.print("trait ")
        case INTERFACE => out.print("interface ")
        case UNKNOWN_KIND | Kind.Unrecognized(_) => out.print("unknown ")
      }
      pprint(info.displayName)
      info.signature match {
        case NoSignature if info.isSelfParameter => ()
        case _ => opt(info.prefixBeforeTpe, info.signature)(pprint)
      }
    }

    private def pprint(ann: Annotation): Unit = {
      out.print("@")
      ann.tpe match {
        case NoType =>
          out.print("<?>")
        case tpe =>
          pprint(tpe)
      }
    }

    private def pprint(acc: Access): Unit = {
      acc match {
        case PrivateAccess() =>
          out.print("private ")
        case PrivateThisAccess() =>
          out.print("private[this] ")
        case PrivateWithinAccess(sym) =>
          out.print("private[")
          pprint(sym, Reference)
          out.print("] ")
        case ProtectedAccess() =>
          out.print("protected ")
        case ProtectedThisAccess() =>
          out.print("protected[this] ")
        case ProtectedWithinAccess(sym) =>
          out.print("protected[")
          pprint(sym, Reference)
          out.print("] ")
        case NoAccess | PublicAccess() =>
          out.print("")
      }
    }

    def pprint(sig: Signature): Unit = {
      sig match {
        case ClassSignature(tparams, parents, self, decls) =>
          rep("[", tparams.infos, ", ", "]")(pprintDefn)
          rep(" extends ", parents, " with ")(pprint)
          if (self.nonEmpty || decls.infos.nonEmpty) {
            out.print(" { ")
          }
          if (self.nonEmpty) {
            out.print("self: ")
            pprint(self)
            out.print(" => ")
          }
          if (decls.infos.nonEmpty) {
            out.print(s"+${decls.infos.length} decls")
          }
          if (self.nonEmpty || decls.infos.nonEmpty) {
            out.print(" }")
          }
        case MethodSignature(tparams, paramss, res) =>
          rep("[", tparams.infos, ", ", "]")(pprintDefn)
          rep("(", paramss, ")(", ")")(params => rep(params.infos, ", ")(pprintDefn))
          opt(": ", res)(pprint)
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
        case ValueSignature(tpe) =>
          pprint(tpe)
        case NoSignature =>
          out.print("<?>")
      }
    }

    def pprint(tpe: Type): Unit = {
      def prefix(tpe: Type): Unit = {
        tpe match {
          case TypeRef(pre, sym, args) =>
            pre match {
              case _: SingleType | _: ThisType | _: SuperType =>
                prefix(pre)
                out.print(".")
              case NoType =>
                ()
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
          case ConstantType(const) =>
            pprint(const)
          case IntersectionType(types) =>
            rep(types, " & ")(normal)
          case UnionType(types) =>
            rep(types, " | ")(normal)
          case WithType(types) =>
            rep(types, " with ")(normal)
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
          case NoType =>
            out.print("<?>")
        }
      }
      def normal(tpe: Type): Unit = {
        tpe match {
          case _: SingleType | _: ThisType | _: SuperType =>
            prefix(tpe)
            out.print(".type")
          case _ =>
            prefix(tpe)
        }
      }
      normal(tpe)
    }

    private def pprintRef(sym: String): Unit = {
      pprint(sym, Reference)
    }

    private def pprintDefn(info: SymbolInformation): Unit = {
      notes.discover(info)
      pprint(info.symbol, Definition)
    }

    protected sealed trait SymbolStyle
    protected case object Reference extends SymbolStyle
    protected case object Definition extends SymbolStyle

    protected def pprint(sym: String, style: SymbolStyle): Unit = {
      val info = notes.visit(sym)
      style match {
        case Reference =>
          pprint(info.displayName)
        case Definition =>
          // NOTE: I am aware of some degree of duplication with pprint(info).
          // However, deduplicating these two methods leads to very involved code,
          // since there are subtle differences in behavior.
          rep(info.annotations, " ", " ")(pprint)
          pprint(info.access)
          if (info.isAbstract && info.isClass) out.print("abstract ")
          if (info.isFinal && !info.isObject) out.print("final ")
          if (info.isSealed) out.print("sealed ")
          if (info.isImplicit) out.print("implicit ")
          if (info.isLazy) out.print("lazy ")
          if (info.isCase) out.print("case ")
          if (info.isCovariant) out.print("+")
          if (info.isContravariant) out.print("-")
          if (info.isVal) out.print("val ")
          if (info.isVar) out.print("var ")
          if (info.isStatic) out.print("static ")
          if (info.isPrimary) out.print("")
          if (info.isEnum) out.print("enum ")
          if (info.isPrimary) out.print("")
          info.kind match {
            case LOCAL => out.print("")
            case FIELD => out.print("")
            case METHOD => out.print("def ")
            case CONSTRUCTOR => out.print("def ")
            case MACRO => out.print("macro ")
            case TYPE => out.print("type ")
            case PARAMETER => out.print("")
            case SELF_PARAMETER => out.print("")
            case TYPE_PARAMETER => out.print("")
            case OBJECT => out.print("object ")
            case PACKAGE => out.print("package ")
            case PACKAGE_OBJECT => out.print("package object ")
            case CLASS => out.print("class ")
            case TRAIT => out.print("trait ")
            case INTERFACE => out.print("interface ")
            case UNKNOWN_KIND | Kind.Unrecognized(_) => out.print("unknown ")
          }
          pprint(info.displayName)
          info.signature match {
            case NoSignature if info.isSelfParameter => ()
            case _ => opt(info.prefixBeforeTpe, info.signature)(pprint)
          }
      }
    }

    private def pprint(name: String): Unit = {
      if (name.nonEmpty) out.print(name)
      else out.print("<?>")
    }

    def pprint(const: Constant): Unit = {
      const match {
        case NoConstant =>
          out.print("<?>")
        case UnitConstant() =>
          out.print("()")
        case BooleanConstant(true) =>
          out.print(true)
        case BooleanConstant(false) =>
          out.print(false)
        case ByteConstant(value) =>
          out.print(value.toByte)
        case ShortConstant(value) =>
          out.print(value.toShort)
        case CharConstant(value) =>
          out.print("'" + value.toChar + "'")
        case IntConstant(value) =>
          out.print(value)
        case LongConstant(value) =>
          out.print(value + "L")
        case FloatConstant(value) =>
          out.print(value + "f")
        case DoubleConstant(value) =>
          out.print(value)
        case StringConstant(value) =>
          out.print("\"" + value + "\"")
        case NullConstant() =>
          out.print("null")
      }
    }

    private implicit class InfoOps(info: SymbolInformation) {
      def prefixBeforeTpe: String = {
        info.kind match {
          case LOCAL | FIELD | PARAMETER | SELF_PARAMETER | UNKNOWN_KIND | Kind.Unrecognized(_) =>
            ": "
          case METHOD | CONSTRUCTOR | MACRO | TYPE | TYPE_PARAMETER | OBJECT | PACKAGE |
              PACKAGE_OBJECT | CLASS | TRAIT | INTERFACE =>
            ""
        }
      }
    }
  }

  class InfoNotes {
    private val buf = mutable.ListBuffer[SymbolInformation]()
    private val noteSymtab = mutable.Map[String, SymbolInformation]()

    def discover(info: SymbolInformation): Unit = {
      if (symtab.info(info.symbol).isEmpty && info.kind != UNKNOWN_KIND) {
        noteSymtab(info.symbol) = info
      }
    }

    def visit(sym: String): SymbolInformation = {
      val symtabInfo = noteSymtab.get(sym).orElse(symtab.info(sym))
      val info = symtabInfo.getOrElse {
        val displayName = if (sym.isGlobal) sym.desc.value else sym
        SymbolInformation(symbol = sym, displayName = displayName)
      }
      visit(info)
    }

    def visit(info: SymbolInformation): SymbolInformation = {
      buf.append(info)
      info
    }

    def visited: List[SymbolInformation] = {
      buf.toList
    }
  }

  implicit def infoOrder: Ordering[SymbolInformation] = {
    Ordering.by(_.symbol)
  }
}

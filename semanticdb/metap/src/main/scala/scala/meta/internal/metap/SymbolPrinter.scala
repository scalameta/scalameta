package scala.meta.internal.metap

import scala.collection.mutable
import scala.math.Ordering
import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.Accessibility.Tag._
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.SingletonType.Tag._
import scala.meta.internal.semanticdb3.SymbolInformation._
import scala.meta.internal.semanticdb3.SymbolInformation.Kind._
import scala.meta.internal.semanticdb3.SymbolInformation.Property._
import scala.meta.internal.semanticdb3.Type.Tag._

trait SymbolPrinter extends BasePrinter {
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
          out.print(info.name)
          out.print(" => ")
          out.println(info.symbol)
        }
      }
    }
  }

  private class InfoPrinter(notes: InfoNotes) {
    def pprint(info: SymbolInformation): Unit = {
      notes.visit(info)
      rep(info.annotations, " ", " ")(pprint)
      opt(info.accessibility)(pprint)
      if (info.has(ABSTRACT)) out.print("abstract ")
      if (info.has(FINAL)) out.print("final ")
      if (info.has(SEALED)) out.print("sealed ")
      if (info.has(IMPLICIT)) out.print("implicit ")
      if (info.has(LAZY)) out.print("lazy ")
      if (info.has(CASE)) out.print("case ")
      if (info.has(COVARIANT)) out.print("covariant ")
      if (info.has(CONTRAVARIANT)) out.print("contravariant ")
      if (info.has(VAL)) out.print("val ")
      if (info.has(VAR)) out.print("var ")
      if (info.has(STATIC)) out.print("static ")
      if (info.has(PRIMARY)) out.print("primary ")
      if (info.has(ENUM)) out.print("enum ")
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
      pprint(info.name)
      opt(info.prefixBeforeTpe, info.tpe)(pprint)
    }

    private def pprint(ann: Annotation): Unit = {
      out.print("@")
      ann.tpe match {
        case Some(tpe) =>
          pprint(tpe)
        case None =>
          out.print("<?>")
          Nil
      }
    }

    private def pprint(acc: Accessibility): Unit = {
      acc.tag match {
        case PUBLIC =>
          out.print("")
        case PRIVATE =>
          out.print("private ")
        case PRIVATE_THIS =>
          out.print("private[this] ")
        case PRIVATE_WITHIN =>
          out.print("private[")
          pprint(acc.symbol, Reference)
          out.print("] ")
        case PROTECTED =>
          out.print("protected ")
        case PROTECTED_THIS =>
          out.print("protected[this] ")
        case PROTECTED_WITHIN =>
          out.print("protected[")
          pprint(acc.symbol, Reference)
          out.print("] ")
        case UNKNOWN_ACCESSIBILITY | Accessibility.Tag.Unrecognized(_) =>
          out.print("<?>")
      }
    }

    def pprint(tpe: Type): Unit = {
      def ref(sym: String): Unit = {
        pprint(sym, Reference)
      }
      def defn(info: SymbolInformation): Unit = {
        notes.discover(info)
        pprint(info.symbol, Definition)
      }
      def prefix(tpe: Type): Unit = {
        tpe.tag match {
          case TYPE_REF =>
            val Some(TypeRef(pre, sym, args)) = tpe.typeRef
            pre match {
              case Some(pre) if pre.tag.isSingletonType =>
                prefix(pre)
                out.print(".")
              case Some(pre) =>
                prefix(pre)
                out.print("#")
              case _ =>
                ()
            }
            ref(sym)
            rep("[", args, ", ", "]")(normal)
          case SINGLETON_TYPE =>
            val Some(SingletonType(tag, pre, sym, x, s)) = tpe.singletonType
            tag match {
              case SYMBOL =>
                opt(pre, ".")(prefix)
                ref(sym)
              case THIS =>
                opt(sym, ".")(ref)
                out.print("this")
              case SUPER =>
                opt(pre, ".")(prefix)
                out.print("super")
                opt("[", sym, "]")(ref)
              case UNIT =>
                out.print("()")
              case BOOLEAN =>
                if (x == 0) out.print("false")
                else if (x == 1) out.print("true")
                else out.print("<?>")
              case BYTE | SHORT =>
                out.print(x)
              case CHAR =>
                out.print("'" + x.toChar + "'")
              case INT =>
                out.print(x)
              case LONG =>
                out.print(x + "L")
              case FLOAT =>
                out.print(java.lang.Float.intBitsToFloat(x.toInt) + "f")
              case DOUBLE =>
                out.print(java.lang.Double.longBitsToDouble(x))
              case STRING =>
                out.print("\"" + s + "\"")
              case NULL =>
                out.print("null")
              case UNKNOWN_SINGLETON | SingletonType.Tag.Unrecognized(_) =>
                out.print("<?>")
            }
          case INTERSECTION_TYPE =>
            val Some(IntersectionType(types)) = tpe.intersectionType
            rep(types, " & ")(normal)
          case UNION_TYPE =>
            val Some(UnionType(types)) = tpe.unionType
            rep(types, " | ")(normal)
          case WITH_TYPE =>
            val Some(WithType(types)) = tpe.withType
            rep(types, " with ")(normal)
          case STRUCTURAL_TYPE =>
            val Some(StructuralType(utpe, decls)) = tpe.structuralType
            utpe.foreach(normal)
            if (decls.nonEmpty) rep(" { ", decls, "; ", " }")(defn)
            else out.print(" {}")
          case ANNOTATED_TYPE =>
            val Some(AnnotatedType(anns, utpe)) = tpe.annotatedType
            utpe.foreach(normal)
            out.print(" ")
            rep(anns, " ", "")(pprint)
          case EXISTENTIAL_TYPE =>
            val Some(ExistentialType(utpe, decls)) = tpe.existentialType
            utpe.foreach(normal)
            rep(" forSome { ", decls, "; ", " }")(defn)
          case UNIVERSAL_TYPE =>
            val Some(UniversalType(tparams, utpe)) = tpe.universalType
            rep("[", tparams, ", ", "] => ")(defn)
            utpe.foreach(normal)
          case CLASS_INFO_TYPE =>
            val Some(ClassInfoType(tparams, parents, decls)) = tpe.classInfoType
            rep("[", tparams, ", ", "]")(defn)
            rep(" extends ", parents, " with ")(normal)
            if (decls.nonEmpty) out.print(s" { +${decls.length} decls }")
          case METHOD_TYPE =>
            val Some(MethodType(tparams, paramss, res)) = tpe.methodType
            rep("[", tparams, ", ", "] => ")(defn)
            rep("(", paramss, ")(", ")")(params => rep(params.symbols, ", ")(defn))
            opt(": ", res)(normal)
          case BY_NAME_TYPE =>
            val Some(ByNameType(utpe)) = tpe.byNameType
            out.print("=> ")
            utpe.foreach(normal)
          case REPEATED_TYPE =>
            val Some(RepeatedType(utpe)) = tpe.repeatedType
            utpe.foreach(normal)
            out.print("*")
          case TYPE_TYPE =>
            val Some(TypeType(tparams, lo, hi)) = tpe.typeType
            rep("[", tparams, ", ", "] => ")(defn)
            if (lo != hi) {
              opt(" >: ", lo)(normal)
              opt(" <: ", hi)(normal)
            } else {
              val alias = lo
              opt(" = ", alias)(normal)
            }
          case UNKNOWN_TYPE | Type.Tag.Unrecognized(_) =>
            out.print("<?>")
        }
      }
      def normal(tpe: Type): Unit = {
        tpe.tag match {
          case SINGLETON_TYPE =>
            val Some(SingletonType(tag, _, _, _, _)) = tpe.singletonType
            tag match {
              case SYMBOL | THIS | SUPER =>
                prefix(tpe)
                out.print(".type")
              case _ =>
                prefix(tpe)
            }
          case _ =>
            prefix(tpe)
        }
      }
      normal(tpe)
    }

    private sealed trait SymbolStyle
    private case object Reference extends SymbolStyle
    private case object Definition extends SymbolStyle

    private def pprint(sym: String, style: SymbolStyle): Unit = {
      val info = notes.visit(sym)
      style match {
        case Reference =>
          pprint(info.name)
        case Definition =>
          // NOTE: I am aware of some degree of duplication with pprint(info).
          // However, deduplicating these two methods leads to very involved code,
          // since there are subtle differences in behavior.
          rep(info.annotations, " ", " ")(pprint)
          opt(info.accessibility)(pprint)
          if (info.has(ABSTRACT)) out.print("abstract ")
          if (info.has(FINAL)) out.print("final ")
          if (info.has(SEALED)) out.print("sealed ")
          if (info.has(IMPLICIT)) out.print("implicit ")
          if (info.has(LAZY)) out.print("lazy ")
          if (info.has(CASE)) out.print("case ")
          if (info.has(COVARIANT)) out.print("+")
          if (info.has(CONTRAVARIANT)) out.print("-")
          if (info.has(VAL)) out.print("val ")
          if (info.has(VAR)) out.print("var ")
          if (info.has(STATIC)) out.print("static ")
          if (info.has(PRIMARY)) out.print("")
          if (info.has(ENUM)) out.print("enum ")
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
          pprint(info.name)
          opt(info.prefixBeforeTpe, info.tpe)(pprint)
      }
    }

    private def pprint(name: String): Unit = {
      if (name.nonEmpty) out.print(name)
      else out.print("<?>")
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

  private lazy val docSymtab: Map[String, SymbolInformation] = {
    doc.symbols.map(info => (info.symbol, info)).toMap
  }

  private class InfoNotes {
    private val buf = mutable.ListBuffer[SymbolInformation]()
    private val noteSymtab = mutable.Map[String, SymbolInformation]()

    def discover(info: SymbolInformation): Unit = {
      if (!docSymtab.contains(info.symbol) && info.kind != UNKNOWN_KIND) {
        noteSymtab(info.symbol) = info
      }
    }

    def visit(sym: String): SymbolInformation = {
      val symtabInfo = noteSymtab.get(sym).orElse(docSymtab.get(sym))
      val info = symtabInfo.getOrElse {
        val name = if (sym.isGlobal) sym.desc.name else sym
        SymbolInformation(symbol = sym, name = name)
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

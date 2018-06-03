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
import scala.meta.internal.semanticdb3.SymbolOccurrence._
import scala.meta.internal.semanticdb3.SymbolOccurrence.Role._
import scala.meta.internal.semanticdb3.Type.Tag._

trait SymbolPrinter extends BasePrinter {
  private lazy val symtab: Map[String, SymbolInformation] = {
    doc.symbols.map(info => (info.symbol, info)).toMap
  }

  def pprint(info: SymbolInformation): Unit = {
    out.print(info.symbol)
    out.print(" => ")

    val infoNotes = new InfoNotes
    val infoPrinter = new InfoPrinter(infoNotes)
    infoPrinter.pprint(info)
    out.println()

    if (settings.format.isDetailed) {
      if (info.isTemplate || info.isMember) {
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
  }

  private class InfoPrinter(notes: InfoNotes) {
    def pprint(info: SymbolInformation): Unit = {
      notes.visit(info)
      rep(info.annotations, " ", " ")(pprint)
      opt(info.accessibility)(pprint)
      def has(prop: Property) = (info.properties & prop.value) != 0
      if (has(ABSTRACT)) out.print("abstract ")
      if (has(FINAL)) out.print("final ")
      if (has(SEALED)) out.print("sealed ")
      if (has(IMPLICIT)) out.print("implicit ")
      if (has(LAZY)) out.print("lazy ")
      if (has(CASE)) out.print("case ")
      if (has(COVARIANT)) out.print("covariant ")
      if (has(CONTRAVARIANT)) out.print("contravariant ")
      if (has(VAL)) out.print("val ")
      if (has(VAR)) out.print("var ")
      if (has(STATIC)) out.print("static ")
      if (has(PRIMARY)) out.print("primary ")
      if (has(ENUM)) out.print("enum ")
      info.kind match {
        case FIELD => out.print("field ")
        case LOCAL => out.print("local ")
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
        case UNKNOWN_KIND | Kind.Unrecognized(_) => ()
      }
      pprint(info.name)
      if (info.isTemplate) {
        opt("", info.tpe)(pprint)
      } else if (info.isMember) {
        opt(": ", info.tpe)(pprint)
      } else {
        ()
      }
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
          pprint(acc.symbol, REFERENCE)
          out.print("] ")
        case PROTECTED =>
          out.print("protected ")
        case PROTECTED_THIS =>
          out.print("protected[this] ")
        case PROTECTED_WITHIN =>
          out.print("protected[")
          pprint(acc.symbol, REFERENCE)
          out.print("] ")
        case UNKNOWN_ACCESSIBILITY | Accessibility.Tag.Unrecognized(_) =>
          out.print("<?>")
      }
    }

    def pprint(tpe: Type): Unit = {
      def ref(sym: String): Unit = {
        pprint(sym, REFERENCE)
      }
      def defn(sym: String): Unit = {
        pprint(sym, DEFINITION)
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
            val Some(ExistentialType(tparams, utpe)) = tpe.existentialType
            utpe.foreach(normal)
            rep(" forSome { ", tparams, "; ", " }")(defn)
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
            opt(": ", res, "")(normal)
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
              opt(">: ", lo, "")(normal)
              lo.foreach(_ => out.print(" "))
              opt("<: ", hi, "")(normal)
            } else {
              val alias = lo
              opt("", alias, "")(normal)
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

    private def pprint(sym: String, role: Role): Unit = {
      val info = notes.visit(sym)
      if (role == REFERENCE) {
        pprint(info.name)
      } else if (role == DEFINITION) {
        // NOTE: This mode is only used to print symbols that are part
        // of complex types, so we don't need to fully support all symbols here.
        rep(info.annotations, " ", " ")(pprint)
        opt(info.accessibility)(pprint)
        if ((info.properties & COVARIANT.value) != 0) out.print("+")
        if ((info.properties & CONTRAVARIANT.value) != 0) out.print("-")
        if ((info.properties & VAL.value) != 0) out.print("val ")
        if ((info.properties & VAR.value) != 0) out.print("var ")
        info.kind match {
          case METHOD =>
            out.print("method ")
            out.print(info.name)
          case TYPE =>
            out.print("type ")
            out.print(info.name)
            out.print(" ")
          case PARAMETER =>
            out.print(info.name)
            out.print(": ")
          case TYPE_PARAMETER =>
            out.print(info.name)
            out.print(" ")
          case _ =>
            out.print("<?>")
            return
        }
        info.tpe match {
          case Some(tpe) => pprint(tpe)
          case None => out.print("<?>")
        }
      } else {
        ()
      }
    }

    private def pprint(name: String): Unit = {
      if (name.nonEmpty) out.print(name)
      else out.print("<?>")
    }
  }

  private class InfoNotes {
    private val buf = mutable.ListBuffer[SymbolInformation]()

    def visit(sym: String): SymbolInformation = {
      val info = symtab.getOrElse(sym, {
        val name = if (sym.isGlobal) sym.desc.name else sym
        SymbolInformation(symbol = sym, name = name)
      })
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

  private implicit class InfoOps(info: SymbolInformation) {
    def isPackage: Boolean = {
      info.kind == PACKAGE
    }
    def isTemplate: Boolean = {
      info.kind == OBJECT ||
      info.kind == PACKAGE_OBJECT ||
      info.kind == CLASS ||
      info.kind == TRAIT ||
      info.kind == INTERFACE
    }
    def isMember: Boolean = {
      info.kind == LOCAL ||
      info.kind == FIELD ||
      info.kind == METHOD ||
      info.kind == CONSTRUCTOR ||
      info.kind == MACRO ||
      info.kind == TYPE ||
      info.kind == PARAMETER ||
      info.kind == SELF_PARAMETER ||
      info.kind == TYPE_PARAMETER
    }
    def isUnknown: Boolean = {
      !isPackage && !isTemplate && !isMember
    }
  }

  implicit def infoOrder: Ordering[SymbolInformation] = {
    Ordering.by(_.symbol)
  }
}

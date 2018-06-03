package scala.meta.internal.metap

import java.io._
import java.util.HashMap
import scala.collection.mutable
import scala.math.Ordering
import scala.meta.internal.semanticdb3._
import scala.meta.internal.semanticdb3.Accessibility.Tag._
import scala.meta.internal.semanticdb3.Diagnostic._
import scala.meta.internal.semanticdb3.Diagnostic.Severity._
import scala.meta.internal.semanticdb3.Language._
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.SingletonType.Tag._
import scala.meta.internal.semanticdb3.SymbolInformation._
import scala.meta.internal.semanticdb3.SymbolInformation.Kind._
import scala.meta.internal.semanticdb3.SymbolInformation.Property._
import scala.meta.internal.semanticdb3.SymbolOccurrence._
import scala.meta.internal.semanticdb3.SymbolOccurrence.Role._
import scala.meta.internal.semanticdb3.Type.Tag._

class Printer(out: PrintStream, doc: TextDocument) {
  def print(): Unit = {
    out.println(doc.uri)
    out.println(s"-" * doc.uri.length)
    out.println("")

    out.println(s"Summary:")
    out.println(s"Schema => SemanticDB v${doc.schema.value}")
    out.println(s"Uri => ${doc.uri}")
    out.println(s"Text => ${if (doc.text.nonEmpty) "non-empty" else "empty"}")
    out.print("Language => ")
    pprint(doc.language)
    out.println()
    if (doc.symbols.nonEmpty) out.println(s"Symbols => ${doc.symbols.length} entries")
    if (doc.occurrences.nonEmpty) out.println(s"Occurrences => ${doc.occurrences.length} entries")
    if (doc.diagnostics.nonEmpty) out.println(s"Diagnostics => ${doc.diagnostics.length} entries")
    if (doc.synthetics.nonEmpty) out.println(s"Synthetics => ${doc.synthetics.length} entries")

    if (doc.symbols.nonEmpty) {
      out.println("")
      out.println("Symbols:")
      doc.symbols.sorted.foreach(pprint)
    }

    if (doc.occurrences.nonEmpty) {
      out.println("")
      out.println("Occurrences:")
      doc.occurrences.sorted.foreach(pprint)
    }

    if (doc.diagnostics.nonEmpty) {
      out.println("")
      out.println("Diagnostics:")
      doc.diagnostics.sorted.foreach(pprint)
    }

    if (doc.synthetics.nonEmpty) {
      out.println("")
      out.println("Synthetics:")
      doc.synthetics.sorted.foreach(pprint)
    }
  }

  private val lineToOffsetCache = new HashMap[TextDocument, Array[Int]]
  implicit class DocumentOps(doc: TextDocument) {
    def substring(range: Option[Range]): Option[String] = {
      range.flatMap { range =>
        if (doc.text.nonEmpty) {
          var lineToOffset = lineToOffsetCache.get(doc)
          if (lineToOffset == null) {
            val chars = doc.text.toArray
            val buf = new mutable.ArrayBuffer[Int]
            buf += 0
            var i = 0
            while (i < chars.length) {
              if (chars(i) == '\n') buf += (i + 1)
              i += 1
            }
            if (buf.last != chars.length) buf += chars.length
            lineToOffset = buf.toArray
            lineToOffsetCache.put(doc, lineToOffset)
          }
          val startOffset = lineToOffset(range.startLine) + range.startCharacter
          val endOffset = lineToOffset(range.endLine) + range.endCharacter
          Some(doc.text.substring(startOffset, endOffset))
        } else {
          None
        }
      }
    }
  }

  private lazy val symtab: Map[String, SymbolInformation] = {
    doc.symbols.map(info => (info.symbol, info)).toMap
  }

  private def pprint(language: Language): Unit = {
    language match {
      case SCALA => out.print("Scala")
      case JAVA => out.print("Java")
      case _ => out.print("Unknown")
    }
  }

  private def pprint(range: Range): Unit = {
    out.print("[")
    out.print(range.startLine)
    out.print(":")
    out.print(range.startCharacter)
    out.print("..")
    out.print(range.endLine)
    out.print(":")
    out.print(range.endCharacter)
    out.print(")")
  }

  private def pprint(name: String): Unit = {
    if (name.nonEmpty) out.print(name)
    else out.print("<?>")
  }

  private def pprint(sym: String, role: Role): List[String] = {
    val buf = List.newBuilder[String]
    buf += sym
    role match {
      case REFERENCE =>
        symtab.get(sym) match {
          case Some(info) => pprint(info.name)
          case None if sym.isGlobal => pprint(sym.desc.name)
          case _ => pprint(sym)
        }
      case DEFINITION =>
        symtab.get(sym) match {
          case Some(info) =>
            // NOTE: This mode is only used to print symbols that are part
            // of complex types, so we don't need to fully support all symbols here.
            rep(info.annotations, " ", " ") { ann =>
              val syms = pprint(ann)
              syms.foreach(buf.+=)
            }
            opt(info.accessibility) { acc =>
              if (acc.symbol.nonEmpty) buf += acc.symbol
              pprint(acc)
            }
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
                return buf.result
            }
            info.tpe match {
              case Some(tpe) =>
                val syms = pprint(tpe)
                syms.foreach(buf.+=)
              case None =>
                out.print("<?>")
            }
          case None =>
            if (sym.isGlobal) pprint(sym.desc.name) else pprint(sym)
            out.print(": <?>")
        }
      case UNKNOWN_ROLE | Role.Unrecognized(_) =>
        ()
    }
    buf.result
  }

  private def pprint(tpe: Type): List[String] = {
    val buf = List.newBuilder[String]
    def ref(sym: String): Unit = {
      val syms = pprint(sym, REFERENCE)
      syms.foreach(buf.+=)
    }
    def defn(sym: String): Unit = {
      val syms = pprint(sym, DEFINITION)
      syms.foreach(buf.+=)
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
          rep(anns, " ", "") { ann =>
            val todo = pprint(ann)
            todo.foreach(buf.+=)
          }
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
          rep("[", tparams, ", ", "] => ")(defn)
          rep(parents, " with ")(normal)
          rep(" { ", decls, "; ", " }")(defn)
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
    buf.result
  }

  private def pprint(info: SymbolInformation): Unit = {
    pprint(info.symbol)
    out.print(" => ")
    rep(info.annotations, " ", " ")(ann => pprint(ann))
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
    info.kind match {
      case LOCAL | FIELD | METHOD | CONSTRUCTOR | MACRO | TYPE | PARAMETER | SELF_PARAMETER |
          TYPE_PARAMETER =>
        info.tpe match {
          case Some(tpe) =>
            out.print(": ")
            val syms = pprint(tpe)
            val visited = mutable.Set[String]()
            out.println("")
            syms.foreach { sym =>
              if (!visited(sym)) {
                visited += sym
                out.print("  ")
                pprint(sym, REFERENCE)
                out.print(" => ")
                out.println(sym)
              }
            }
          case None =>
            out.println("<?>")
        }
      case OBJECT | PACKAGE_OBJECT | CLASS | TRAIT | INTERFACE =>
        info.tpe match {
          case Some(tpe: Type) =>
            tpe.classInfoType match {
              case Some(ClassInfoType(tparams, parents, decls)) =>
                if (tparams.nonEmpty) rep("[", tparams, ", ", "]")(pprint(_, DEFINITION))
                if (decls.nonEmpty) out.println(s".{+${decls.length} decls}")
                else out.println("")
                parents.foreach { tpe =>
                  out.print("  extends ")
                  pprint(tpe)
                  out.println("")
                }
              case _ =>
                out.println("")
            }
          case None =>
            out.println("<?>")
        }
      case PACKAGE =>
        out.println("")
      case UNKNOWN_KIND | Kind.Unrecognized(_) =>
        out.println("")
    }
  }

  private def pprint(ann: Annotation): List[String] = {
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

  private def pprint(occ: SymbolOccurrence): Unit = {
    opt(occ.range)(pprint)
    opt(": ", doc.substring(occ.range))(out.print)
    pprint(occ.role)
    out.println(occ.symbol)
  }

  private def pprint(role: Role): Unit = {
    role match {
      case REFERENCE => out.print(" => ")
      case DEFINITION => out.print(" <= ")
      case UNKNOWN_ROLE | Role.Unrecognized(_) => out.print(" <?> ")
    }
  }

  private def pprint(diag: Diagnostic): Unit = {
    opt(diag.range, " ")(pprint)
    pprint(diag.severity)
    out.println(diag.message)
  }

  private def pprint(sev: Severity): Unit = {
    sev match {
      case ERROR => out.print("[error] ")
      case WARNING => out.print("[warning] ")
      case INFORMATION => out.print("[info] ")
      case HINT => out.print("[hint] ")
      case UNKNOWN_SEVERITY | Severity.Unrecognized(_) => out.print("[<?>] ")
    }
  }

  private def pprint(synth: Synthetic): Unit = {
    opt(synth.range)(pprint)
    opt(": ", doc.substring(synth.range))(out.print)
    out.print(" => ")
    synth.text match {
      case Some(text) =>
        out.println(text.text)
        rep("  ", text.occurrences.sorted, "  ") { occ =>
          opt(occ.range)(pprint)
          opt(": ", text.substring(occ.range))(out.print)
          pprint(occ.role)
          out.println(occ.symbol)
        }
      case _ =>
        out.println("<?>")
    }
  }

  private implicit def rangeOrder: Ordering[Range] =
    Ordering.by(r => (r.startLine, r.startCharacter, r.endLine, r.endCharacter))
  private implicit def infoOrder: Ordering[SymbolInformation] =
    Ordering.by(_.symbol)
  private implicit def occOrder: Ordering[SymbolOccurrence] =
    Ordering.by(o => (o.range, o.symbol, o.role.value))
  private implicit def diagOrder: Ordering[Diagnostic] =
    Ordering.by(d => (d.range, d.severity.value, d.message))
  private implicit def synthOrder: Ordering[Synthetic] =
    Ordering.by(s => (s.range, s.text.map(_.text)))

  private def rep[T](pre: String, xs: Seq[T], sep: String, suf: String)(f: T => Unit): Unit = {
    if (xs.nonEmpty) {
      out.print(pre)
      rep(xs, sep)(f)
      out.print(suf)
    }
  }

  private def rep[T](pre: String, xs: Seq[T], sep: String)(f: T => Unit): Unit = {
    rep(pre, xs, sep, "")(f)
  }

  private def rep[T](xs: Seq[T], sep: String, suf: String)(f: T => Unit): Unit = {
    rep("", xs, sep, suf)(f)
  }

  private def rep[T](xs: Seq[T], sep: String)(f: T => Unit): Unit = {
    xs.zipWithIndex.foreach {
      case (x, i) =>
        if (i != 0) out.print(sep)
        f(x)
    }
  }

  private def opt[T](pre: String, xs: Option[T], suf: String)(f: T => Unit): Unit = {
    xs.foreach { x =>
      out.print(pre)
      f(x)
      out.print(suf)
    }
  }

  private def opt[T](pre: String, xs: Option[T])(f: T => Unit): Unit = {
    opt(pre, xs, "")(f)
  }

  private def opt[T](xs: Option[T], suf: String)(f: T => Unit): Unit = {
    opt("", xs, suf)(f)
  }

  private def opt[T](xs: Option[T])(f: T => Unit): Unit = {
    opt("", xs, "")(f)
  }

  private def opt(pre: String, s: String, suf: String)(f: String => Unit): Unit = {
    if (s.nonEmpty) {
      out.print(pre)
      f(s)
      out.print(suf)
    }
  }

  private def opt(s: String, suf: String)(f: String => Unit): Unit = {
    opt("", s, suf)(f)
  }

  private def opt(s: String)(f: String => Unit): Unit = {
    opt("", s, "")(f)
  }
}

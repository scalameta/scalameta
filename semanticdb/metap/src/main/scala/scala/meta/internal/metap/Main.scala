package scala.meta.internal.metap

import java.io.InputStream
import java.nio.file._
import java.util.WeakHashMap
import java.util.jar.JarEntry
import java.util.jar.JarFile
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering
import scala.util.control.NonFatal
import scala.meta.internal.semanticdb3._
import scala.meta.metap._
import Diagnostic._
import Severity._
import SymbolInformation._
import Kind._
import Property._
import SymbolOccurrence._
import Role._
import Type.Tag._
import SingletonType.Tag._
import Accessibility.Tag._
import Language._
import java.io.ByteArrayOutputStream
import java.io.PrintStream

class Main(settings: Settings, reporter: Reporter) {
  import reporter._

  def process(): Boolean = {

    var success = true
    var first = true
    def processSemanticdb(path: Path, stream: InputStream): Unit = {
      if (first) {
        first = false
      } else {
        out.println("")
      }
      try {
        val documents = TextDocuments.parseFrom(stream)
        if (settings.format.isProto) {
          out.println(documents.toProtoString)
        } else {
          documents.documents.foreach(pprint)
        }
      } catch {
        case NonFatal(ex) =>
          out.println(s"error: can't decompile $path")
          ex.printStackTrace(out)
          success = false
      } finally {
        stream.close()
      }
    }

    settings.paths.foreach { path =>
      if (Files.isDirectory(path)) {
        val root = path.resolve("META-INF").resolve("semanticdb")
        if (Files.isDirectory(root)) {
          import scala.collection.JavaConverters._
          Files
            .walk(root)
            .iterator()
            .asScala
            .filter(_.getFileName.toString.endsWith(".semanticdb"))
            .toArray
            // nio.file.Path.compareTo is file system specific,
            // and the behavior is different on windows vs. unix
            .sortBy(_.toString.toLowerCase)
            .foreach { file =>
              processSemanticdb(file, Files.newInputStream(file))
            }
        } else {
          ()
        }
      } else if (Files.isRegularFile(path)) {
        if (path.getFileName.toString.endsWith(".jar")) {
          // Can't use nio.Files.walk because nio.FileSystems is not supported on Scala Native.
          val jarfile = new JarFile(path.toFile)
          val buf = ArrayBuffer.empty[JarEntry]
          val entries = jarfile.entries()
          while (entries.hasMoreElements) {
            val entry = entries.nextElement()
            if (entry.getName.endsWith(".semanticdb")) {
              buf += entry
            }
          }
          buf.sortBy(_.getName).foreach { entry =>
            processSemanticdb(Paths.get(entry.getName), jarfile.getInputStream(entry))
          }
        } else {
          processSemanticdb(path, Files.newInputStream(path))
        }
      } else {
        ()
      }
    }

    success
  }

  def pprint(doc: TextDocument): Unit = {
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
      doc.symbols.sorted.foreach(pprint(_, doc))
    }

    if (doc.occurrences.nonEmpty) {
      out.println("")
      out.println("Occurrences:")
      doc.occurrences.sorted.foreach(pprint(_, doc))
    }

    if (doc.diagnostics.nonEmpty) {
      out.println("")
      out.println("Diagnostics:")
      doc.diagnostics.sorted.foreach(pprint(_, doc))
    }

    if (doc.synthetics.nonEmpty) {
      out.println("")
      out.println("Synthetics:")
      doc.synthetics.sorted.foreach(pprint(_, doc))
    }
  }

  private def pprint(language: Language): Unit = {
    language match {
      case SCALA => out.print("Scala")
      case JAVA => out.print("Java")
      case _ => out.print("Unknown")
    }
  }

  private val offsetCache = new WeakHashMap[TextDocument, Array[Int]]
  private def offset(doc: TextDocument, line: Int): Int = {
    var lineIndices = offsetCache.get(doc)
    if (lineIndices == null) {
      val chars = doc.text.toArray
      val buf = new mutable.ArrayBuffer[Int]
      buf += 0
      var i = 0
      while (i < chars.length) {
        if (chars(i) == '\n') buf += (i + 1)
        i += 1
      }
      if (buf.last != chars.length) buf += chars.length // sentinel value used for binary search
      lineIndices = buf.toArray
      offsetCache.put(doc, lineIndices)
    }
    lineIndices(line)
  }

  private def pprint(range: Option[Range], doc: Option[TextDocument]): Unit = {
    range.foreach { range =>
      out.print("[")
      out.print(range.startLine)
      out.print(":")
      out.print(range.startCharacter)
      out.print("..")
      out.print(range.endLine)
      out.print(":")
      out.print(range.endCharacter)
      out.print(")")
      doc match {
        case Some(doc) if doc.text.nonEmpty =>
          val startOffset = offset(doc, range.startLine) + range.startCharacter
          val endOffset = offset(doc, range.endLine) + range.endCharacter
          val text = doc.text.substring(startOffset, endOffset)
          out.print(s": $text")
        case _ =>
          ()
      }
    }
  }

  private def pprint(name: String, doc: TextDocument): Unit = {
    if (name.nonEmpty) out.print(name)
    else out.print("<?>")
  }

  private val symCache = new WeakHashMap[TextDocument, immutable.Map[String, SymbolInformation]]
  private def pprint(sym: String, role: Role, doc: TextDocument): List[String] = {
    val buf = List.newBuilder[String]
    buf += sym
    var infos = symCache.get(doc)
    if (infos == null) {
      infos = doc.symbols.map(info => (info.symbol, info)).toMap
      symCache.put(doc, infos)
    }
    infos.get(sym) match {
      case Some(info) =>
        role match {
          case REFERENCE =>
            pprint(info.name, doc)
          case DEFINITION =>
            // NOTE: This mode is only used to print symbols that are part
            // of complex types, so we don't need to fully support all symbols here.
            rep(info.annotations, " ", " ") { ann =>
              val syms = pprint(ann, doc)
              syms.foreach(buf.+=)
            }
            opt(info.accessibility) { acc =>
              if (acc.symbol.nonEmpty) buf += acc.symbol
              pprint(acc, doc)
            }
            if ((info.properties & COVARIANT.value) != 0) out.print("+")
            if ((info.properties & CONTRAVARIANT.value) != 0) out.print("-")
            if ((info.properties & VAL.value) != 0) out.print("val ")
            if ((info.properties & VAR.value) != 0) out.print("var ")
            info.kind match {
              case LOCAL =>
                out.print("local ")
                out.print(info.name)
              case FIELD =>
                out.print("field ")
                out.print(info.name)
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
                val syms = pprint(tpe, doc)
                syms.foreach(buf.+=)
              case None =>
                out.print("<?>")
            }
          case UNKNOWN_ROLE | Role.Unrecognized(_) =>
            ()
        }
      case None =>
        // TODO: It would be nice to have a symbol parser in semanticdb3.
        sym.split("[\\.|#]").toList match {
          case _ :+ last =>
            val approxName = {
              val last1 = last.stripPrefix("(").stripPrefix("[")
              val last2 = last1.stripSuffix(")").stripSuffix("]").stripSuffix("#")
              last2.stripPrefix("`").stripSuffix("`")
            }
            pprint(approxName, doc)
          case _ =>
            out.print("<?>")
        }
        role match {
          case REFERENCE => ()
          case DEFINITION => out.print(": <?>")
          case UNKNOWN_ROLE | Role.Unrecognized(_) => ()
        }
    }
    buf.result
  }

  private def pprint(tpe: Type, doc: TextDocument): List[String] = {
    val buf = List.newBuilder[String]
    def ref(sym: String): Unit = {
      val syms = pprint(sym, REFERENCE, doc)
      syms.foreach(buf.+=)
    }
    def defn(sym: String): Unit = {
      val syms = pprint(sym, DEFINITION, doc)
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
            val todo = pprint(ann, doc)
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
          out.print(": ")
          res.foreach(normal)
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
          opt(">: ", lo, "")(normal)
          lo.foreach(_ => out.print(" "))
          opt("<: ", hi, "")(normal)
        case APPLIED_METHOD_TYPE =>
          val Some(AppliedMethodType(tparams, res)) = tpe.appliedMethodType
          rep(tparams, "", " => ")(params => rep("(", params.parameters, ", ", ")")(normal))
          res.foreach(normal)
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

  private def pprint(info: SymbolInformation, doc: TextDocument): Unit = {
    pprint(info.symbol, doc)
    out.print(" => ")
    rep(info.annotations, " ", " ")(pprint(_, doc))
    opt(info.accessibility)(pprint(_, doc))
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
    pprint(info.name, doc)
    info.kind match {
      case LOCAL | FIELD | METHOD | CONSTRUCTOR | MACRO | TYPE | PARAMETER | SELF_PARAMETER |
          TYPE_PARAMETER =>
        info.tpe match {
          case Some(tpe) =>
            out.print(": ")
            val syms = pprint(tpe, doc)
            val visited = mutable.Set[String]()
            out.println("")
            syms.foreach { sym =>
              if (!visited(sym)) {
                visited += sym
                out.print("  ")
                pprint(sym, REFERENCE, doc)
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
                if (tparams.nonEmpty) rep("[", tparams, ", ", "]")(pprint(_, DEFINITION, doc))
                if (decls.nonEmpty) out.println(s".{+${decls.length} decls}")
                else out.println("")
                parents.foreach { tpe =>
                  out.print("  extends ")
                  pprint(tpe, doc)
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

  private def pprint(ann: Annotation, doc: TextDocument): List[String] = {
    out.print("@")
    ann.tpe match {
      case Some(tpe) =>
        pprint(tpe, doc)
      case None =>
        out.print("<?>")
        Nil
    }
  }

  private def pprint(acc: Accessibility, doc: TextDocument): Unit = {
    acc.tag match {
      case PUBLIC =>
        out.print("")
      case PRIVATE =>
        out.print("private ")
      case PRIVATE_THIS =>
        out.print("private[this] ")
      case PRIVATE_WITHIN =>
        out.print("private[")
        pprint(acc.symbol, REFERENCE, doc)
        out.print("] ")
      case PROTECTED =>
        out.print("protected ")
      case PROTECTED_THIS =>
        out.print("protected[this] ")
      case PROTECTED_WITHIN =>
        out.print("protected[")
        pprint(acc.symbol, REFERENCE, doc)
        out.print("] ")
      case UNKNOWN_ACCESSIBILITY | Accessibility.Tag.Unrecognized(_) =>
        out.print("<?>")
    }
  }

  private def pprint(occ: SymbolOccurrence, doc: TextDocument): Unit = {
    pprint(occ.range, Some(doc))
    occ.role match {
      case REFERENCE => out.print(" => ")
      case DEFINITION => out.print(" <= ")
      case UNKNOWN_ROLE | Role.Unrecognized(_) => out.print(" <?> ")
    }
    out.print(occ.symbol)
    occ.tpe match {
      case Some(tpe) =>
        out.print(" (")
        pprint(tpe, doc)
        out.print(")")
      case _ =>
    }
    out.println("")
  }

  private def pprint(diag: Diagnostic, doc: TextDocument): Unit = {
    pprint(diag.range, None)
    diag.severity match {
      case ERROR => out.print("[error] ")
      case WARNING => out.print("[warning] ")
      case INFORMATION => out.print("[info] ")
      case HINT => out.print("[hint] ")
      case UNKNOWN_SEVERITY | Severity.Unrecognized(_) => out.print("[<?>] ")
    }
    out.println(diag.message)
  }

  private def pprint(synth: Synthetic, doc: TextDocument): Unit = {
    pprint(synth.range, Some(doc))
    out.print(" => ")
    synth.text match {
      case Some(text) =>
        out.println(text.text)
        val occs = text.occurrences.sorted
        rep("  ", occs, "  ")(pprint(_, text))
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

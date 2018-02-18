package scala.meta.internal.metap

import java.nio.file._
import java.util.WeakHashMap
import scala.collection.{immutable, mutable}
import scala.compat.Platform.EOL
import scala.math.Ordering
import scala.util.control.NonFatal
import scala.meta.internal.semanticdb3._
import Diagnostic._, Severity._
import SymbolInformation._, Kind._, Property._
import SymbolOccurrence._, Role._
import Type.Tag._, SingletonType.Tag._, Accessibility.Tag._

object Main {
  def process(args: Array[String]): Int = {
    var failed = false
    args.zipWithIndex.foreach {
      case (arg, i) =>
        try {
          val stream = Files.newInputStream(Paths.get(arg))
          try {
            if (i != 0) println("")
            val documents = TextDocuments.parseFrom(stream)
            documents.documents.foreach(pprint)
          } finally {
            stream.close()
          }
        } catch {
          case NonFatal(ex) =>
            println(s"error: can't decompile $arg")
            ex.printStackTrace()
            failed = true
        }
    }
    if (failed) 1 else 0
  }

  private def pprint(doc: TextDocument): Unit = {
    println(doc.uri)
    println(s"-" * doc.uri.length)
    println("")

    println(s"Summary:")
    println(s"Schema => SemanticDB v${doc.schema.value}")
    println(s"Uri => ${doc.uri}")
    println(s"Text => ${if (doc.text.nonEmpty) "non-empty" else "empty"}")
    if (doc.language.nonEmpty) println(s"Language => ${doc.language.get.name}")
    if (doc.symbols.nonEmpty) println(s"Symbols => ${doc.symbols.length} entries")
    if (doc.occurrences.nonEmpty) println(s"Occurrences => ${doc.occurrences.length} entries")
    if (doc.diagnostics.nonEmpty) println(s"Diagnostics => ${doc.diagnostics.length} entries")
    if (doc.synthetics.nonEmpty) println(s"Synthetics => ${doc.synthetics.length} entries")

    if (doc.symbols.nonEmpty) {
      println("")
      println("Symbols:")
      doc.symbols.sorted.foreach(pprint(_, doc))
    }

    if (doc.occurrences.nonEmpty) {
      println("")
      println("Occurrences:")
      doc.occurrences.sorted.foreach(pprint(_, doc))
    }

    if (doc.diagnostics.nonEmpty) {
      println("")
      println("Diagnostics:")
      doc.diagnostics.sorted.foreach(pprint(_, doc))
    }

    if (doc.synthetics.nonEmpty) {
      println("")
      println("Synthetics:")
      doc.synthetics.sorted.foreach(pprint(_, doc))
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
      print("[")
      print(range.startLine)
      print(":")
      print(range.startCharacter)
      print("..")
      print(range.endLine)
      print(":")
      print(range.endCharacter)
      print(")")
      doc match {
        case Some(doc) if doc.text.nonEmpty =>
          val startOffset = offset(doc, range.startLine) + range.startCharacter
          val endOffset = offset(doc, range.endLine) + range.endCharacter
          val text = doc.text.substring(startOffset, endOffset)
          print(s": $text")
        case _ =>
          ()
      }
    }
  }

  private def pprint(name: String, doc: TextDocument): Unit = {
    if (name.nonEmpty) print(name)
    else print("<?>")
  }

  private val symCache = new WeakHashMap[TextDocument, immutable.Map[String, SymbolInformation]]
  private def pprint(sym: String, role: Role, doc: TextDocument): Unit = {
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
            rep(info.annotations, " ", " ")(pprint(_, doc))
            opt(info.accessibility)(pprint(_, doc))
            if ((info.properties & COVARIANT.value) != 0) print("+")
            if ((info.properties & CONTRAVARIANT.value) != 0) print("-")
            info.kind match {
              case GETTER =>
                print("getter ")
                print(info.name)
              case SETTER =>
                print("setter ")
                print(info.name)
              case TYPE =>
                print("type ")
                print(info.name)
                print(" ")
              case PARAMETER =>
                print(info.name)
                print(": ")
              case TYPE_PARAMETER =>
                print(info.name)
                print(" ")
              case _ =>
                print("<?>")
                return
            }
            info.tpe match {
              case Some(tpe) => pprint(tpe, doc)
              case None => print("<?>")
            }
          case UNKNOWN_ROLE | Role.Unrecognized(_) =>
            ()
        }
      case None =>
        // TODO: It would be nice to have a symbol parser in semanticdb3.
        sym.split("\\.").toList match {
          case _ :+ last =>
            val approxName = {
              val last1 = last.stripPrefix("(").stripPrefix("[")
              val last2 = last1.stripSuffix(")").stripSuffix("]").stripSuffix("#")
              last2.stripPrefix("`").stripSuffix("`")
            }
            pprint(approxName, doc)
          case _ =>
            print("<?>")
        }
        role match {
          case REFERENCE => ()
          case DEFINITION => print(": <?>")
          case UNKNOWN_ROLE | Role.Unrecognized(_) => ()
        }
    }
  }

  private def pprint(tpe: Type, doc: TextDocument): List[String] = {
    val buf = List.newBuilder[String]
    def ref(sym: String): Unit = {
      buf += sym
      pprint(sym, REFERENCE, doc)
    }
    def defn(sym: String): Unit = {
      buf += sym
      pprint(sym, DEFINITION, doc)
    }
    def prefix(tpe: Type): Unit = {
      tpe.tag match {
        case TYPE_REF =>
          val Some(TypeRef(pre, sym, args)) = tpe.typeRef
          pre match {
            case Some(pre) if pre.tag.isSingletonType =>
              prefix(pre)
              print(".")
            case Some(pre) =>
              prefix(pre)
              print("#")
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
              print("this")
            case SUPER =>
              opt(pre, ".")(prefix)
              print("super")
              opt("[", sym, "]")(ref)
            case UNIT =>
              print("()")
            case BOOLEAN =>
              if (x == 0) print("false")
              else if (x == 1) print("true")
              else print("<?>")
            case BYTE | SHORT =>
              print(x)
            case CHAR =>
              print("'" + x.toChar + "'")
            case INT =>
              print(x)
            case LONG =>
              print(x + "L")
            case FLOAT =>
              print(java.lang.Float.intBitsToFloat(x.toInt) + "f")
            case DOUBLE =>
              print(java.lang.Double.longBitsToDouble(x))
            case STRING =>
              print("\"" + s + "\"")
            case NULL =>
              print("null")
            case UNKNOWN_SINGLETON | SingletonType.Tag.Unrecognized(_) =>
              print("<?>")
          }
        case STRUCTURAL_TYPE =>
          val Some(StructuralType(tparams, parents, decls)) = tpe.structuralType
          rep("[", tparams, ", ", "] => ")(defn)
          rep(parents, " with ")(normal)
          if (decls.nonEmpty || parents.length == 1) {
            print(" { ")
            rep(decls, "; ")(defn)
            print(" }")
          }
        case ANNOTATED_TYPE =>
          val Some(AnnotatedType(anns, utpe)) = tpe.annotatedType
          utpe.foreach(normal)
          print(" ")
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
          print(": ")
          res.foreach(normal)
        case BY_NAME_TYPE =>
          val Some(ByNameType(utpe)) = tpe.byNameType
          print("=> ")
          utpe.foreach(normal)
        case REPEATED_TYPE =>
          val Some(RepeatedType(utpe)) = tpe.repeatedType
          utpe.foreach(normal)
          print("*")
        case TYPE_TYPE =>
          val Some(TypeType(tparams, lo, hi)) = tpe.typeType
          rep("[", tparams, ", ", "] => ")(defn)
          opt(">: ", lo, "")(normal)
          lo.foreach(_ => print(" "))
          opt("<: ", hi, "")(normal)
        case UNKNOWN_TYPE | Type.Tag.Unrecognized(_) =>
          print("<?>")
      }
    }
    def normal(tpe: Type): Unit = {
      tpe.tag match {
        case SINGLETON_TYPE =>
          val Some(SingletonType(tag, _, _, _, _)) = tpe.singletonType
          tag match {
            case SYMBOL | THIS | SUPER =>
              prefix(tpe)
              print(".type")
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
    print(" => ")
    rep(info.annotations, " ", " ")(pprint(_, doc))
    opt(info.accessibility)(pprint(_, doc))
    def has(prop: Property) = (info.properties & prop.value) != 0
    if (has(ABSTRACT)) print("abstract ")
    if (has(FINAL)) print("final ")
    if (has(SEALED)) print("sealed ")
    if (has(IMPLICIT)) print("implicit ")
    if (has(LAZY)) print("lazy ")
    if (has(CASE)) print("case ")
    if (has(COVARIANT)) print("covariant ")
    if (has(CONTRAVARIANT)) print("contravariant ")
    if (has(VALPARAM)) print("valparam ")
    if (has(VARPARAM)) print("varparam ")
    info.kind match {
      case VAL => print("val ")
      case VAR => print("var ")
      case DEF => print("def ")
      case GETTER => print("getter ")
      case SETTER => print("setter ")
      case PRIMARY_CONSTRUCTOR => print("primaryctor ")
      case SECONDARY_CONSTRUCTOR => print("secondaryctor ")
      case MACRO => print("macro ")
      case TYPE => print("type ")
      case PARAMETER => print("param ")
      case SELF_PARAMETER => print("selfparam ")
      case TYPE_PARAMETER => print("typeparam ")
      case OBJECT => print("object ")
      case PACKAGE => print("package ")
      case PACKAGE_OBJECT => print("package object ")
      case CLASS => print("class ")
      case TRAIT => print("trait ")
      case UNKNOWN_KIND | Kind.Unrecognized(_) => ()
    }
    pprint(info.name, doc)
    info.kind match {
      case VAL | VAR | DEF | GETTER | SETTER | PRIMARY_CONSTRUCTOR | SECONDARY_CONSTRUCTOR | MACRO |
          TYPE | PARAMETER | SELF_PARAMETER | TYPE_PARAMETER =>
        info.tpe match {
          case Some(tpe) =>
            print(": ")
            val syms = pprint(tpe, doc)
            val visited = mutable.Set[String]()
            println("")
            syms.foreach { sym =>
              if (!visited(sym)) {
                visited += sym
                print("  ")
                pprint(sym, REFERENCE, doc)
                print(" => ")
                println(sym)
              }
            }
          case None =>
            info.signature match {
              case Some(sig) =>
                print(s": ${sig.text}")
                println("")
                val occs = sig.occurrences.sorted
                rep("  ", occs, "  ")(pprint(_, sig))
              case _ =>
                println("")
            }
        }
        info.overrides.foreach(sym => println(s"  overrides $sym"))
      case OBJECT | PACKAGE | PACKAGE_OBJECT | CLASS | TRAIT =>
        info.tpe match {
          case Some(tpe: Type) =>
            tpe.classInfoType match {
              case Some(ClassInfoType(_, parents, decls)) =>
                if (decls.nonEmpty) println(s".{+${decls.length} decls}")
                else println("")
                parents.foreach { tpe =>
                  print("  extends ")
                  pprint(tpe, doc)
                  println("")
                }
              case _ =>
                println("")
            }
          case None =>
            if (info.members.nonEmpty) println(s".{+${info.members.length} members}")
            else println("")
            info.overrides.sorted.foreach(sym => println(s"  extends $sym"))
        }
      case UNKNOWN_KIND | Kind.Unrecognized(_) =>
        println("")
    }
  }

  private def pprint(ann: Annotation, doc: TextDocument): List[String] = {
    print("@")
    ann.tpe match {
      case Some(tpe) =>
        pprint(tpe, doc)
      case None =>
        print("<?>")
        Nil
    }
  }

  private def pprint(acc: Accessibility, doc: TextDocument): Unit = {
    acc.tag match {
      case PUBLIC =>
        print("")
      case PRIVATE =>
        print("private ")
      case PRIVATE_THIS =>
        print("private[this] ")
      case PRIVATE_WITHIN =>
        print("private[")
        pprint(acc.symbol, REFERENCE, doc)
        print("] ")
      case PROTECTED =>
        print("protected ")
      case PROTECTED_THIS =>
        print("protected[this] ")
      case PROTECTED_WITHIN =>
        print("protected[")
        pprint(acc.symbol, REFERENCE, doc)
        print("] ")
      case UNKNOWN_ACCESSIBILITY | Accessibility.Tag.Unrecognized(_) =>
        print("<?>")
    }
  }

  private def pprint(occ: SymbolOccurrence, doc: TextDocument): Unit = {
    pprint(occ.range, Some(doc))
    occ.role match {
      case REFERENCE => print(" => ")
      case DEFINITION => print(" <= ")
      case UNKNOWN_ROLE | Role.Unrecognized(_) => print(" <?> ")
    }
    println(occ.symbol)
  }

  private def pprint(diag: Diagnostic, doc: TextDocument): Unit = {
    pprint(diag.range, None)
    diag.severity match {
      case ERROR => print("[error] ")
      case WARNING => print("[warning] ")
      case INFORMATION => print("[info] ")
      case HINT => print("[hint] ")
      case UNKNOWN_SEVERITY | Severity.Unrecognized(_) => print("[<?>] ")
    }
    println(diag.message)
  }

  private def pprint(synth: Synthetic, doc: TextDocument): Unit = {
    pprint(synth.range, Some(doc))
    print(" => ")
    synth.text match {
      case Some(text) =>
        println(text.text)
        val occs = text.occurrences.sorted
        rep("  ", occs, "  ")(pprint(_, text))
      case _ =>
        println("<?>")
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
      print(pre)
      rep(xs, sep)(f)
      print(suf)
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
        if (i != 0) print(sep)
        f(x)
    }
  }

  private def opt[T](pre: String, xs: Option[T], suf: String)(f: T => Unit): Unit = {
    xs.foreach { x =>
      print(pre)
      f(x)
      print(suf)
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
      print(pre)
      f(s)
      print(suf)
    }
  }

  private def opt(s: String, suf: String)(f: String => Unit): Unit = {
    opt("", s, suf)(f)
  }

  private def opt(s: String)(f: String => Unit): Unit = {
    opt("", s, "")(f)
  }
}

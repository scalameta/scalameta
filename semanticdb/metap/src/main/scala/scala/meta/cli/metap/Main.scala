package scala.meta.cli.metap

import java.nio.file._
import java.util.WeakHashMap
import scala.collection.{immutable, mutable}
import scala.compat.Platform.EOL
import scala.math.Ordering
import scala.util.control.NonFatal
import scala.meta.internal.semanticdb3._
import Diagnostic._, Severity._
import LiteralType.Tag._
import SymbolInformation._, Kind._, Property._
import SymbolOccurrence._, Role._
import Type.Tag._

object Main {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args))
  }

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
    println(s"Language => ${doc.language}")
    println(s"Symbols => ${doc.symbols.length} entries")
    println(s"Occurrences => ${doc.occurrences.length} entries")
    println(s"Diagnostics => ${doc.diagnostics.length} entries")
    println(s"Synthetics => ${doc.synthetics.length} entries")

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
            def has(prop: Property) = (info.properties & prop.value) != 0
            if (has(PRIVATE)) print("private ")
            if (has(PROTECTED)) print("protected ")
            if (has(ABSTRACT)) print("abstract ")
            if (has(FINAL)) print("final ")
            if (has(SEALED)) print("sealed ")
            if (has(IMPLICIT)) print("implicit ")
            if (has(LAZY)) print("lazy ")
            if (has(CASE)) print("case ")
            if (has(COVARIANT)) print("+")
            if (has(CONTRAVARIANT)) print("-")
            info.kind match {
              case VAL =>
                print("val ")
                print(info.name)
                print(": ")
              case VAR =>
                print("var ")
                print(info.name)
                print(": ")
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
          case _ =>
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
          case _ => ()
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
            case Some(pre) if pre.tag.isSingleType || pre.tag.isThisType || pre.tag.isSuperType =>
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
        case SINGLE_TYPE =>
          val Some(SingleType(pre, sym)) = tpe.singleType
          opt(pre, ".")(prefix)
          ref(sym)
        case THIS_TYPE =>
          val Some(ThisType(sym)) = tpe.thisType
          if (sym.nonEmpty) {
            ref(sym)
            print(".")
          }
          print("this")
        case SUPER_TYPE =>
          val Some(SuperType(pre, mix)) = tpe.superType
          opt(pre, ".")(normal)
          print("super")
          opt("[", mix, "]")(normal)
        case LITERAL_TYPE =>
          tpe.literalType match {
            case Some(LiteralType(UNIT, _, _)) =>
              println("()")
            case Some(LiteralType(BOOLEAN, 0, _)) =>
              println("false")
            case Some(LiteralType(BOOLEAN, 1, _)) =>
              println("true")
            case Some(LiteralType(BYTE | SHORT, x, _)) =>
              println(x)
            case Some(LiteralType(CHAR, x, _)) =>
              println("'" + x.toChar + "'")
            case Some(LiteralType(INT, x, _)) =>
              println(x)
            case Some(LiteralType(LONG, x, _)) =>
              println(x + "L")
            case Some(LiteralType(FLOAT, x, _)) =>
              println(java.lang.Float.intBitsToFloat(x.toInt) + "f")
            case Some(LiteralType(DOUBLE, x, _)) =>
              println(java.lang.Double.longBitsToDouble(x))
            case Some(LiteralType(STRING, _, s)) =>
              println("\"" + s + "\"")
            case Some(LiteralType(NULL, _, _)) =>
              println("null")
            case _ =>
              println("<?>")
          }
        case COMPOUND_TYPE =>
          val Some(CompoundType(parents, decls)) = tpe.compoundType
          rep(parents, " with ")(normal)
          if (decls.nonEmpty || parents.length == 1) {
            print(" { ")
            rep(decls, "; ")(defn)
            print(" }")
          }
        case ANNOTATED_TYPE =>
          val Some(AnnotatedType(utpe, anns)) = tpe.annotatedType
          utpe.foreach(normal)
          print(" ")
          rep("@", anns, " ", "")(normal)
        case EXISTENTIAL_TYPE =>
          val Some(ExistentialType(utpe, decls)) = tpe.existentialType
          utpe.foreach(normal)
          rep(" forSome { ", decls, "; ", " }")(defn)
        case TYPE_LAMBDA =>
          val Some(TypeLambda(tparams, utpe)) = tpe.typeLambda
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
        case _ =>
          print("<?>")
      }
    }
    def normal(tpe: Type): Unit = {
      tpe.tag match {
        case SINGLE_TYPE | THIS_TYPE | SUPER_TYPE =>
          prefix(tpe)
          print(".type")
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
    def has(prop: Property) = (info.properties & prop.value) != 0
    if (has(PRIVATE)) print("private ")
    if (has(PROTECTED)) print("protected ")
    if (has(ABSTRACT)) print("abstract ")
    if (has(FINAL)) print("final ")
    if (has(SEALED)) print("sealed ")
    if (has(IMPLICIT)) print("implicit ")
    if (has(LAZY)) print("lazy ")
    if (has(CASE)) print("case ")
    if (has(COVARIANT)) print("covariant ")
    if (has(CONTRAVARIANT)) print("contravariant ")
    if (has(VALPARAM)) print("val ")
    if (has(VARPARAM)) print("var ")
    if (info.kind == VAL) print("val ")
    if (info.kind == VAR) print("var ")
    if (info.kind == DEF) print("def ")
    if (info.kind == PRIMARY_CONSTRUCTOR) print("primaryctor ")
    if (info.kind == SECONDARY_CONSTRUCTOR) print("secondaryctor ")
    if (info.kind == MACRO) print("macro ")
    if (info.kind == TYPE) print("type ")
    if (info.kind == PARAMETER) print("param ")
    if (info.kind == TYPE_PARAMETER) print("typeparam ")
    if (info.kind == OBJECT) print("object ")
    if (info.kind == PACKAGE) print("package ")
    if (info.kind == PACKAGE_OBJECT) print("package object ")
    if (info.kind == CLASS) print("class ")
    if (info.kind == TRAIT) print("trait ")
    pprint(info.name, doc)
    info.kind match {
      case VAL | VAR | DEF | PRIMARY_CONSTRUCTOR |
           SECONDARY_CONSTRUCTOR | MACRO | TYPE | PARAMETER | TYPE_PARAMETER =>
        info.tpe match {
          case Some(tpe) =>
            print(": ")
            val syms = pprint(tpe, doc)
            println("")
            syms.foreach { sym =>
              print("  ")
              pprint(sym, REFERENCE, doc)
              print(" => ")
              println(sym)
            }
          case None =>
            info.signature match {
              case Some(sig) =>
                print(s": ${sig.text}")
                println("")
                val occs = sig.occurrences.sorted
                occs.foreach { occ => print("  "); pprint(occ, sig) }
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
                parents.foreach{ tpe =>
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
      case _ =>
        println("")
    }
  }

  private def pprint(occ: SymbolOccurrence, doc: TextDocument): Unit = {
    pprint(occ.range, Some(doc))
    occ.role match {
      case REFERENCE => print(" => ")
      case DEFINITION => print(" <= ")
      case _ => print(" <?> ")
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
      case _ => print("[<?>] ")
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
        occs.foreach { occ => print("  "); pprint(occ, text) }
      case _ =>
        println("<?>")
    }
  }

  private implicit def rangeOrder: Ordering[Range] =
    Ordering.by(r => (r.startLine, r.startCharacter, r.endLine, r.endCharacter))
  private implicit def infoOrder: Ordering[SymbolInformation] =
    Ordering.by(_.symbol)
  private implicit def occOrder: Ordering[SymbolOccurrence] =
    Ordering.by(_.range)
  private implicit def diagOrder: Ordering[Diagnostic] =
    Ordering.by(_.range)
  private implicit def synthOrder: Ordering[Synthetic] =
    Ordering.by(_.range)

  private def rep[T](pre: String, xs: Seq[T], sep: String, suf: String)(f: T => Unit): Unit = {
    if (xs.nonEmpty) {
      print(pre)
      rep(xs, sep)(f)
      print(suf)
    }
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
}

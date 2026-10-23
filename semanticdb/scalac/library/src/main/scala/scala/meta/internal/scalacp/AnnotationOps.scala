package scala.meta.internal.scalacp

import scala.meta.internal.classpath.MissingSymbolException
import scala.meta.internal.{semanticdb => s}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.internal.pickling.PickleFormat._
import scala.tools.scalap.scalax.rules.scalasig._
import scala.util.control.NonFatal

trait AnnotationOps {
  self: Scalacp =>

  import AnnotationOps._

  private def parseEntry(ref: Int): Any = node.scalaSig.parseEntry(ref)

  private def entryType(ref: Int): Int = node.scalaSig.table(ref)._1

  private def entryRefs(ref: Int): List[Int] = node.scalaSig
    .applyRule(ScalaSigParsers.parseEntry(entryRefsParser)(ref))

  // the SYMANNOT entries of each symbol, by the entry of the symbol
  private[scalacp] lazy val symbolAnnotations: collection.Map[Int, collection.Seq[Int]] = {
    val res = mutable.Map.empty[Int, mutable.ArrayBuffer[Int]]
    var idx = 0
    val len = node.scalaSig.table.length
    while (idx < len) {
      if (entryType(idx) == SYMANNOT) entryRefs(idx) match {
        case ref :: _ => res.getOrElseUpdate(ref, new mutable.ArrayBuffer[Int]) += idx
        case _ =>
      }
      idx += 1
    }
    res
  }

  implicit class XtensionAnnotation(ann: Int) {
    def toSemanticOpt: Option[s.AnnotationTree] = entryType(ann) match {
      case SYMANNOT => toAnnotation(entryRefs(ann).drop(1))
      case ANNOTINFO => toAnnotation(entryRefs(ann))
      case _ => None
    }

    def toSemantic: s.AnnotationTree = toSemanticOpt.getOrElse(s.AnnotationTree())
  }

  // info_Ref {annotArg_Ref} {name_Ref constAnnotArg_Ref}
  private def toAnnotation(refs: List[Int]): Option[s.AnnotationTree] = refs match {
    case head :: tail => parseEntry(head) match {
        case tpe: Type => tpe.toSemanticTpe match {
            case stpe: s.TypeRef if syntheticAnnotationsSymbols.contains(stpe.symbol) => None
            case stpe => Some(s.AnnotationTree(stpe, toArguments(tail)))
          }
        case _ => None
      }
    case _ => None
  }

  private def toArguments(refs: List[Int]): List[s.Tree] = {
    val res = List.newBuilder[s.Tree]
    @tailrec
    def loop(refs: List[Int]): List[s.Tree] = refs match {
      case name :: arg :: rest if isName(name) =>
        res += s.AssignTree(s.IdTree(parseEntry(name).toString), toArgument(arg))
        loop(rest)
      case arg :: rest =>
        res += toArgument(arg)
        loop(rest)
      case _ => res.result()
    }
    loop(refs)
  }

  // if broken signatures are stubbed, a missing class drops only the argument that refers to it
  private def toArgument(ref: Int): s.Tree =
    try toTree(ref)
    catch {
      case ex: MissingSymbolException =>
        if (settings.logBrokenSignatures) reporter.err
          .println(s"broken annotation argument: ${ex.getMessage}")
        if (settings.stubBrokenSignatures) s.NoTree else throw ex
      case NonFatal(_) => s.NoTree
    }

  private def isName(ref: Int): Boolean = {
    val tpe = entryType(ref)
    tpe == TERMname || tpe == TYPEname
  }

  @tailrec
  private def toTree(ref: Int): s.Tree = entryType(ref) match {
    case ANNOTINFO => toAnnotation(entryRefs(ref)).getOrElse(s.NoTree)
    case ANNOTARGARRAY => s.ApplyTree(s.NoTree, entryRefs(ref).map(toArgument))
    case TREE => entryRefs(ref) match {
        case LITERALtree :: _ :: const :: _ => toTree(const)
        case refs => treeToTree(refs)
      }
    case LITERALshort | LITERALint | LITERALlong | LITERALfloat | LITERALdouble => s
        .LiteralTree(signedConstant(ref))
    case _ => toTree(parseEntry(ref))
  }

  /* convert a tree as metac does, or return NoTree if the tree refers to a symbol outside the
   * pickle: the pickle keeps only the name and the owner of such a symbol, not its overload */
  private def treeToTree(refs: List[Int]): s.Tree = refs match {
    case APPLYtree :: _ :: fun :: args =>
      val sfun = qualifierTree(fun)
      if (sfun eq s.NoTree) s.NoTree
      else {
        val sargs = args.map(toTree)
        if (sargs.contains(s.NoTree)) s.NoTree else s.ApplyTree(sfun, sargs)
      }
    case TYPEAPPLYtree :: _ :: fun :: targs =>
      val sfun = qualifierTree(fun)
      if (sfun eq s.NoTree) s.NoTree else s.TypeApplyTree(sfun, targs.map(treeType))
    case (SELECTtree | IDENTtree | THIStree) :: _ :: sym :: _ => idTree(sym)
    case _ => s.NoTree
  }

  // metac's `toSemanticQualifierTree`
  private def qualifierTree(ref: Int): s.Tree = entryRefs(ref) match {
    case SELECTtree :: _ :: sym :: qual :: _ => entryRefs(qual) match {
        case NEWtree :: _ => idTree(sym)
        case qref :: qrefs =>
          val qsym = treeSymbol(qref, qrefs)
          if (qsym < 0) s.NoTree
          else if (isOwner(qsym, sym)) idTree(sym)
          else {
            val squal = idTreeOrNull(qsym)
            if (squal eq null) s.NoTree
            else {
              val sid = idTreeOrNull(sym)
              if (sid eq null) s.NoTree else s.SelectTree(squal, Some(sid))
            }
          }
        case _ => s.NoTree
      }
    case ref :: refs =>
      val sym = treeSymbol(ref, refs)
      if (sym < 0) s.NoTree else idTree(sym)
    case _ => s.NoTree
  }

  @tailrec // scalac's `Tree.symbol`
  private def treeSymbol(ref: Int, refs: List[Int]): Int = ref match {
    case SELECTtree | IDENTtree | THIStree => refs match {
        case _ :: sym :: _ => sym
        case _ => -1
      }
    case APPLYtree | TYPEAPPLYtree => refs match {
        case _ :: fun :: _ => entryRefs(fun) match {
            case ref :: refs => treeSymbol(ref, refs)
            case _ => -1
          }
        case _ => -1
      }
    case _ => -1
  }

  private def isOwner(owner: Int, sym: Int): Boolean = parseEntry(sym) match {
    case x: Symbol => x.parent match {
        case Some(p: ScalaSigSymbol) => p.index == owner
        case _ => false
      }
    case _ => false
  }

  private def treeType(ref: Int): s.Type = entryRefs(ref) match {
    case _ :: tpe :: _ => parseEntry(tpe) match {
        case x: Type => x.toSemanticTpe
        case _ => s.NoType
      }
    case _ => s.NoType
  }

  // metacp gives a semanticdb local no information, so refer only to a global
  private def idTreeOrNull(sym: Int): s.IdTree = parseEntry(sym) match {
    case x: SymbolInfoSymbol if x.isSemanticdbGlobal => s.IdTree(x.ssym)
    case _ => null
  }
  private def idTree(sym: Int): s.Tree = {
    val res = idTreeOrNull(sym)
    if (res eq null) s.NoTree else res
  }

  // scalap reads the value of a literal without its sign
  private def signedConstant(ref: Int): s.Constant = {
    val bytes = node.scalaSig.getEntry(ref).byteCode
    val shift = java.lang.Long.SIZE - java.lang.Byte.SIZE * bytes.length
    val value = bytes.toLong << shift >> shift
    entryType(ref) match {
      case LITERALshort => s.ShortConstant(value.toInt)
      case LITERALint => s.IntConstant(value.toInt)
      case LITERALfloat => s.FloatConstant(java.lang.Float.intBitsToFloat(value.toInt))
      case LITERALdouble => s.DoubleConstant(java.lang.Double.longBitsToDouble(value))
      case _ => s.LongConstant(value)
    }
  }

  // the signed constant of each numeric constant type, by the type that scalap parsed
  private lazy val constantTypes: java.util.IdentityHashMap[Any, s.Constant] = {
    val map = new java.util.IdentityHashMap[Any, s.Constant]
    node.scalaSig.table.indices.foreach(ref =>
      if (entryType(ref) == CONSTANTtpe) entryRefs(ref).headOption.foreach(literal =>
        entryType(literal) match {
          case LITERALshort | LITERALint | LITERALlong | LITERALfloat | LITERALdouble => map
              .put(parseEntry(ref), signedConstant(literal))
          case _ =>
        },
      ),
    )
    map
  }

  private[scalacp] def signedConstantType(tpe: ConstantType): Option[s.Constant] =
    Option(constantTypes.get(tpe))

  private def toTree(obj: Any): s.Tree = obj match {
    case x: ExternalSymbol => toTree(ConstantType(x).toSemanticTpe)
    case x: Symbol => s.IdTree(x.toSemantic)
    case x: Type => toTree(x.toSemanticTpe)
    case _ => s.Constant.opt(obj).fold[s.Tree](s.NoTree)(s.LiteralTree.apply)
  }

  private def toTree(obj: s.Type): s.Tree = obj match {
    case x: s.ConstantType => s.LiteralTree(x.constant)
    case x: s.SingleType => typeToTree(x.prefix, x.symbol)
    case x: s.TypeRef =>
      val func = typeToTree(x.prefix, x.symbol)
      if (x.typeArguments.isEmpty) func else s.TypeApplyTree(func, x.typeArguments)
    case _ => s.NoTree
  }

  private def typeToTree(prefix: s.Type, symbol: String): s.Tree = {
    val pre = toTree(prefix)
    val sym = s.IdTree(symbol)
    if (pre eq s.NoTree) sym else s.SelectTree(pre, Some(sym))
  }

}

object AnnotationOps {

  private val syntheticAnnotationsSymbols = Set("scala/reflect/macros/internal/macroImpl#")

  private val entryRefsParser = {
    import ScalaSigEntryParsers._
    ref.*
  }

}

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
    case TREE => entryRefs(ref) match {
        case LITERALtree :: _ :: const :: _ => toTree(const)
        case _ => s.NoTree
      }
    case _ => toTree(parseEntry(ref))
  }

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

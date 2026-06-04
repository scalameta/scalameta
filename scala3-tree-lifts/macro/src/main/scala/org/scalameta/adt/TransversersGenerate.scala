package org.scalameta.adt

import scala.meta.internal.trees.{Reflection => AstReflection}

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object TransversersGenerate {
  def generateTransformerParts(): List[String] = macro TransversersGenerateMacros.implTransformer
  def generateTraverserParts(): List[String] = macro TransversersGenerateMacros.implTraverser
}

class TransversersGenerateMacros(val c: Context) extends GenerateHelper with AstReflection {
  import c.universe._

  private lazy val TermAdt = mirror.staticClass("scala.meta.Term").asAdt
  private lazy val TypeAdt = mirror.staticClass("scala.meta.Type").asAdt
  private lazy val DefnAdt = mirror.staticClass("scala.meta.Defn").asAdt
  private lazy val TreeAdt = TreeSymbol.asRoot
  private lazy val QuasiAdt = QuasiSymbol.asAdt

  private def allLeafsNonQuasi: List[Leaf] = TreeAdt.allLeafs.filter(l => !(l <:< QuasiAdt))

  private def categorize(leaves: List[Leaf]): (List[Leaf], List[Leaf], List[Leaf], List[Leaf]) = {
    val terms = List.newBuilder[Leaf]
    val types = List.newBuilder[Leaf]
    val defns = List.newBuilder[Leaf]
    val rest = List.newBuilder[Leaf]
    leaves.foreach(l =>
      if (l <:< TermAdt) terms += l
      else if (l <:< TypeAdt) types += l
      else if (l <:< DefnAdt) defns += l
      else rest += l,
    )
    (terms.result(), types.result(), defns.result(), rest.result())
  }

  private def leafTypeName(l: Leaf): String = getFullName(l.sym)

  private def getSecondaryApply(name: String, leaves: List[Leaf], returnType: String)(
      f: (Leaf, StringBuilder) => Unit,
  ): String = {
    val sb = new StringBuilder
    val header = s"  private def apply$name(tree: scala.meta.Tree): $returnType = tree match {"
    val footer = "\n  }\n"
    sb.append(header)
    leaves.foreach { l =>
      sb.append("\n    ").append("case tree: ").append(leafTypeName(l)).append(" =>")
      f(l, sb)
    }
    sb.append(footer)
    sb.result()
  }

  // ============== TRANSFORMER ==============

  private def transformerLeafHandler(l: Leaf, sb: StringBuilder): Unit = {
    val hasOnlyPrimitiveFields = l.fields
      .forall(f => f.tpe =:= typeOf[Any] || PrimitiveTpe.unapply(f.tpe))
    if (hasOnlyPrimitiveFields) {
      sb.append(" tree")
      return
    }

    sb.append(
      """|
         |        var same = true
         |""".stripMargin,
    )

    def getTpeName(tpe: Type): String = getFullName(tpe.typeSymbol)
    def param(fname: String): String = s"_$fname"
    def header(fname: String): String = s"""|        val ${param(fname)} = {
                                            |          val original = tree.$fname
                                            |""".stripMargin
    val footer = "        }\n"
    l.fields.foreach { f =>
      val fname = f.name.toString
      val code = f.tpe match {
        case tpe @ TreeTpe() =>
          s"""|          val to = apply(original)
              |          to match {
              |            case to: ${getTpeName(tpe)} =>
              |              if (original ne to) same = false
              |              to
              |            case to =>
              |              fail("${l.prefix}.$fname", original, to)
              |          }
              |""".stripMargin
        case OptionTreeTpe(tpe) =>
          s"""|          original match {
              |            case Some(from) =>
              |              apply(from) match {
              |                case to: ${getTpeName(tpe)} =>
              |                  if (from eq to) original
              |                  else { same = false; Some(to) }
              |                case to =>
              |                  fail("${l.prefix}.$fname", from, to)
              |              }
              |            case None => None
              |          }
              |""".stripMargin
        case ListTreeTpe(tpe) =>
          val elemType = getTpeName(tpe)
          s"""|          var samelist = true
              |          val tolist = List.newBuilder[$elemType]
              |          original.foreach { from =>
              |            apply(from) match {
              |              case to: $elemType =>
              |                if (from ne to) samelist = false
              |                tolist += to
              |              case to =>
              |                fail("${l.prefix}.$fname", from, to)
              |            }
              |          }
              |          if (samelist) original
              |          else { same = false; tolist.result() }
              |""".stripMargin
        case _ => "        original\n"
      }
      sb.append(header(fname))
      sb.append(code)
      sb.append(footer)
    }

    val fieldNames = l.fields.map(f => param(f.name.toString)).mkString(", ")
    sb.append(
      s"""|
          |        if (same) tree
          |        else ${leafTypeName(l)}($fieldNames)
          |          .withOrigin(scala.meta.trees.Origin.PartialProxy(tree.origin))
          |""".stripMargin,
    )
  }

  private def secondaryApply(name: String, leaves: List[Leaf], returnType: String): String =
    getSecondaryApply(name, leaves, returnType)(transformerLeafHandler)

  def implTransformer(): c.Expr[List[String]] = {
    val (terms, types, defns, rest) = categorize(allLeafsNonQuasi)

    val header =
      s"""|package scala.meta
          |package transversers
          |
          |abstract class Transformer {
          |  def apply(tree: scala.meta.Tree): scala.meta.Tree = {
          |    tree match {
          |      case t: scala.meta.Term => applyTerm(t)
          |      case t: scala.meta.Type => applyType(t)
          |      case t: scala.meta.Defn => applyDefn(t)
          |      case t => applyRest(t)
          |    }
          |  }
          |""".stripMargin

    val termApply = secondaryApply("Term", terms, "scala.meta.Term")
    val typeApply = secondaryApply("Type", types, "scala.meta.Type")
    val defnApply = secondaryApply("Defn", defns, "scala.meta.Defn")
    val restApply = secondaryApply("Rest", rest, "scala.meta.Tree")

    val footer =
      s"""|
          |  def apply(treeopt: Option[scala.meta.Tree]): Option[scala.meta.Tree] = treeopt match {
          |    case Some(tree) =>
          |      val tree1 = apply(tree)
          |      if (tree eq tree1) treeopt
          |      else Some(tree1)
          |    case None => None
          |  }
          |
          |  def apply(trees: List[scala.meta.Tree]): List[scala.meta.Tree] = {
          |    var same = true
          |    val buf = List.newBuilder[scala.meta.Tree]
          |    trees.foreach { tree =>
          |      val tree1 = apply(tree)
          |      if (tree ne tree1) same = false
          |      buf += tree1
          |    }
          |    if (same) trees
          |    else buf.result()
          |  }
          |
          |  def apply(trees: Seq[scala.meta.Tree]): Seq[scala.meta.Tree] = {
          |    var same = true
          |    val buf = Seq.newBuilder[scala.meta.Tree]
          |    trees.foreach { tree =>
          |      val tree1 = apply(tree)
          |      if (tree ne tree1) same = false
          |      buf += tree1
          |    }
          |    if (same) trees
          |    else buf.result()
          |  }
          |
          |  private def fail(field: String, from: scala.meta.Tree, to: scala.meta.Tree): Nothing = {
          |    import scala.meta.prettyprinters._
          |    val errorPrefix = "Invalid transformation of " + field + ": "
          |    val errorHeader = errorPrefix + from.productPrefix + " -> " + to.productPrefix + ". "
          |    val errorDetails = "From: " + from.structure + ", to: " + to.structure
          |    throw new UnsupportedOperationException(errorHeader + errorDetails)
          |  }
          |}
          |""".stripMargin

    mkStringList(header, termApply, typeApply, defnApply, restApply, footer)
  }

  // ============== TRAVERSER ==============

  private def traverserSecondaryApply(name: String, leaves: List[Leaf]): String =
    getSecondaryApply(name, leaves, "Unit")((l, sb) =>
      l.fields.foreach(f =>
        if (!(f.tpe =:= typeOf[Any]) && !PrimitiveTpe.unapply(f.tpe)) sb
          .append("\n      this.apply(tree.").append(f.name).append(")"),
      ),
    )

  def implTraverser(): c.Expr[List[String]] = {
    val (terms, types, defns, rest) = categorize(allLeafsNonQuasi)

    val header =
      s"""|package scala.meta
          |package transversers
          |
          |abstract class Traverser {
          |  def apply(tree: scala.meta.Tree): Unit = {
          |    tree match {
          |      case t: scala.meta.Term => applyTerm(t)
          |      case t: scala.meta.Type => applyType(t)
          |      case t: scala.meta.Defn => applyDefn(t)
          |      case t => applyRest(t)
          |    }
          |  }
          |""".stripMargin

    val termApply = traverserSecondaryApply("Term", terms)
    val typeApply = traverserSecondaryApply("Type", types)
    val defnApply = traverserSecondaryApply("Defn", defns)
    val restApply = traverserSecondaryApply("Rest", rest)

    val footer =
      s"""|
          |  def apply(treeopt: Option[scala.meta.Tree]): Unit = treeopt.foreach(apply)
          |
          |  def apply(trees: Iterable[scala.meta.Tree]): Unit = trees.foreach(apply)
          |}
          |""".stripMargin

    mkStringList(header, termApply, typeApply, defnApply, restApply, footer)
  }
}

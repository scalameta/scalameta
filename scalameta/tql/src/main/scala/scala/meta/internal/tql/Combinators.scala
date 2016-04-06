package scala.meta
package internal
package tql

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.meta.tql._

/**
 * This class define several bundled macros used to enhance or to make less boilerplaty the usage
 * of the combinators defined in Combinators.scala
 * */
private[meta] class CombinatorMacros(val c: Context) {
  import c.universe._
  import definitions._
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  /**
   * Transform focus*{case Term.If(a,b,c) => true} into guard[Term.If]{case Term.If(a,b,c) => true}
   * as the compiler cannot infer the type in arguments of anonymous functions (see §8.5).
   * * name can change.
   * */
  def filterSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    val (lhs, _) =  getLUBsfromPFs[T](f)
    q"guard[$lhs]($f)"
  }

  /**
   * This allows to inline the method withResult of the CTWithResult implicit class, so as to have the power of an implicit class
   * without the runtime performance impact.
   * */
  def TWithResult[T: c.WeakTypeTag, A : c.WeakTypeTag](a: c.Tree): c.Tree  = c.untypecheck(c.prefix.tree) match {
    case q"$_.CTWithResult[$_]($t)" => q"($t, $a)"
    case q"$_.CTWithResult[$_]($t).andCollect[$_]" => q"($t, $a)"
    case _ => c.abort(c.enclosingPosition, "Bad form in TWithResult " + show(c.prefix.tree))
  }

  /**
   * Inline the method andCOllect of the CTWithResult implicit class (see TWithResult macro).
   * The second parameter is actually the implicit paramter of type Collector.
   * */
  def TAndCollect[T: c.WeakTypeTag, A : c.WeakTypeTag](a: c.Tree)(y: c.Tree): c.Tree =
    TWithResult[T, List[A]](q"($y.builder += $a).result")

  /**
   * Rewrite
   * transform {
   *    case Lit.Int(a) => Lit.Int(a * 3) andCollect a
   *    case Term.If(a, b, c) => Term.If(a, b, c)  andCollect 0
   * }
   * into
   *
   * transformWithResult[Lit, Lit, List[Int] ]({case Lit.Int(a) => Lit.Int(a * 3) andCollect a} |
   * transformWithResult[Term, Term, List[Int] ]({case Term.If(a, b, c) => Term.If(a, b, c)  andCollect 0} |
   *
   *Here's the kind of error you get if you try to do something like that:
   *
   * transform {
   *    case Lit.Int(a) => Lit.Int(a * 3) andCollect a
   *    case Term.If(a, b, c) => Term.If(a, b, c)  //no result here.
   * }.down
   *
   * [error] D:\[..]\scala\meta\tql\Example.scala:44: could not find implicit
   * value for evidence parameter of type tql.Monoid[Any]
   * */
  def transformSugarImpl[T : c.WeakTypeTag](f: c.Tree): c.Tree = {
    val Ttpe = implicitly[c.WeakTypeTag[T]].tpe

    def setTuplesForEveryOne(clauses: List[CaseDef]): List[CaseDef] = {
      def setTupleTo(rhs: c.Tree) = {
        if (rhs.tpe.typeSymbol != TupleClass(2)) q"($rhs, _root_.scala.meta.tql.Monoid.Void)"
        else rhs
      }
      clauses.map{_ match {
        case cq"${lhs: c.Tree} => ${rhs:  c.Tree}" => cq"$lhs => ${setTupleTo(rhs)}"
        case cq"${lhs: c.Tree} if $cond => ${rhs:  c.Tree}" => cq"$lhs if $cond => ${setTupleTo(rhs)}"
        case x => x
      }}
    }

    def getTypesFromTuple2(rhss: List[c.Type]): List[(c.Type, c.Type)] = rhss.map{
      /*It's not like I have a choice..
      http://stackoverflow.com/questions/18735295/how-to-match-a-universetype-via-quasiquotes-or-deconstructors*/
      case TypeRef(_, sym, List(a, b)) if sym.fullName == "scala.Tuple2" => (a, b)
      case x if x <:< Ttpe => (x, typeOf[Unit])
      case _ => c.abort(c.enclosingPosition, "There should be a Tuple2 or a " + show(Ttpe) + " here")
    }

    def normalizeCases(pf: c.Tree) = pf match {
      case q"{case ..$cases}" => setTuplesForEveryOne(cases).map(betterUntypecheck(_))
    }

    val cases = normalizeCases(f)
    val (lhss, rhss) = getTypesFromPFS[T](f).unzip
    val (trhs, ress) = getTypesFromTuple2(rhss).unzip

    val transforms = cases.zip(lhss).zip(trhs).zip(ress).map{
      case (((cas, lhs), rhs), res) =>
       q"${c.prefix}.transformWithResult[$lhs, $rhs, $res]({case $cas})"
    }

    if (transforms.size < 1)
      c.abort(c.enclosingPosition, "No cases found in " + show(f))

    val res = betterUntypecheck(transforms.reduceRight[c.Tree]((c, acc) => q"$c | $acc"))
    res
  }



  protected def getLUBsfromPFs[T : c.WeakTypeTag](f: c.Tree): (c.Type, c.Type) = {
    val tpes = getTypesFromPFS[T](f)
    if (tpes.size > 1) {
      val (lhs, rhs) = tpes.unzip
      (c.universe.lub(lhs), c.universe.lub(rhs))
    }
    else
      tpes.head //TODO guarenteed to have size > 0?
  }

  protected def getTypesFromPFS[T : c.WeakTypeTag](f: c.Tree): List[(c.Type, c.Type)] = {
    f match {
      case q"{case ..$cases}" =>
        cases.map(getTypesFromCase(_))
      case func if func.tpe <:< weakTypeOf[PartialFunction[_, _]] =>
        val lhs :: rhs :: Nil = func.tpe.typeArgs
        List((implicitly[c.WeakTypeTag[T]].tpe, rhs))
      case _ => c.abort(c.enclosingPosition, "Expecting a partial function here")
    }
  }

  protected def getTypesFromCase(cas: c.Tree): (c.Type, c.Type) = {
    import c.universe._
    cas match {
      case cq"${lhs: c.Tree} => ${rhs:  c.Tree}" => (lhs.tpe, rhs.tpe)
      case cq"${lhs: c.Tree} if $_ => ${rhs:  c.Tree}" => (lhs.tpe, rhs.tpe)
      case p => c.abort(c.enclosingPosition, "Bad format in partial function at: " + show(p))
    }
  }

  /**
   * When type checking a partial function an <unapply-selector> thing gets added in the AST of the pattern match
   * and this thing cannot be typechecked a second time. (see : https://issues.scala-lang.org/browse/SI-5465)
   * To solve this problem we have to remonve the <unapply-selector> ourselves, and here is the solution:
   * thanks Eugene : https://gist.github.com/xeno-by/7fbd422c6789299140a7*/
  protected object betterUntypecheck extends Transformer {
    private object ExpandedQuasiquotePattern {
      def unapply(tree: Tree): Option[Tree] = tree match {
        case UnApply(app @ Apply(Select(qual, TermName("unapply")), List(Ident(TermName("<unapply-selector>")))), args) =>
          import scala.tools.nsc.Global
          val global = c.universe.asInstanceOf[Global]
          import global.analyzer._
          val expandee = app.asInstanceOf[global.Tree].attachments.get[MacroExpansionAttachment].map(_.expandee.asInstanceOf[Tree])
          expandee.flatMap({
            case Apply(
                Apply(Select(qual, TermName("unapply")), List(Ident(TermName("<unapply-selector>")))),
                List(dialect)) =>
              qual match {
                case Select(Apply(_, List(realQual)), interp) =>  Some(Apply(Select(realQual, interp), args))
                case _ => None
              }
            case _ =>
              None
          })
        case _ =>
          None
      }
    }
    override def transform(tree: Tree): Tree = tree match {
      case ExpandedQuasiquotePattern(tree) =>
        super.transform(tree)
      case UnApply(Apply(Select(qual, TermName("unapply")), List(Ident(TermName("<unapply-selector>")))), args) =>
        Apply(transform(qual), transformTrees(args))
      case UnApply(Apply(TypeApply(Select(qual, TermName("unapplySeq")),List(TypeTree())), List(Ident(TermName("<unapply-selector>")))), args) =>
        //this is funny
        Apply(transform(qual), transformTrees(args))
      case _ => super.transform(tree)
    }
    def apply(tree: Tree): Tree = c.untypecheck(transform(tree))
  }
}

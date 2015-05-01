package org.scalameta.tql

import scala.reflect.macros.whitebox.Context
import org.scalameta.ast.{Reflection => AstReflection}

class TraverserBuilderMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  /* Get all leaves for the root (i.e. all Tree cases) */
  def getAllLeaves(root: Root) = root.allLeafs

  /* Change order of the leaves to have the ones corresponding to the symbol passed as first, first - this is use
   * to reduce latency overhead in the traversing process. */
  def changeOrderOf(firsts: List[Symbol], allLeafs: List[Symbol]): List[Symbol] = {
    val tmp = firsts.map(_.fullName)
    val rest = for {leaf <- allLeafs if !(tmp contains leaf.fullName)} yield leaf
    val leafFist = firsts.map(x => allLeafs.find(_.fullName == x.fullName).get)
    leafFist ++ rest
  }

  /* Git all leaves sorted, return their companion object. */
  def getAllLeafsOrderedInTree[T : c.WeakTypeTag](firsts: c.Tree*): List[c.Tree] = {
    val leaves: List[Leaf] = getAllLeaves(u.symbolOf[T].asRoot)
    //weird hack so that the types are set in each symbol and the buildImpl function doesn't fail
    leaves.foreach(_.sym.owner.info)
    val leafsWithOrder = changeOrderOf(firsts.map(_.symbol).toList, leaves.map(_.sym))
    leafsWithOrder.map(x => q"${x.companion}")
  }


  /* Construct the Traverser based on all leaves. */
  def buildFromTopSymbolDelegate[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, firsts: c.Tree*): c.Tree = {
    val allLeafs = getAllLeafsOrderedInTree[T](firsts: _*)
    buildImplDelegate[T, A](f, allLeafs: _*)
  }

  /* Return an option containing the list of parameters in the tree 'typ', along with their respective type.
   * NB: Trick to make it work with the Name unapply.
   */
  def getParamsWithTypes(typ: c.Type): Option[(List[TermName], List[c.Type])] = {
    val fields = typ.companion.typeSymbol.asLeaf.fields
    if (!fields.isEmpty){
      Some(fields.map(x => (TermName(c.freshName), x.tpe) ).unzip)
    }
    else
      None
  }

  def buildFuncWith[T : c.WeakTypeTag, A : c.WeakTypeTag](cases: List[c.Tree], parameter: TermName): c.Tree = {
    q"""
        ($parameter: ${implicitly[c.WeakTypeTag[T]]}) => $parameter match {
          case ..$cases
          case v => Some((v, implicitly[_root_.org.scalameta.algebra.Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
        }
    """
  }

  /* Build all cases for the objs (leaves) passed in parameters. */
  def buildImplDelegate[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, objs: c.Tree*): c.Tree = {
    val parameter = c.internal.enclosingOwner.asMethod.paramLists.head.head.name.toTermName //LOL
    val cases = buildCases[T, A](f, objs.toList, parameter)
    buildDelegateWith[T, A](cases, parameter)
  }

  /* Construct the global match{} frame, with all the cases. */
  def buildDelegateWith[T : c.WeakTypeTag, A : c.WeakTypeTag](cases: List[c.Tree], parameter: TermName): c.Tree = {
    q"""
      $parameter match {
        case ..$cases
        case v => Some((v, implicitly[_root_.org.scalameta.algebra.Monoid[${implicitly[c.WeakTypeTag[A]]}]].zero))
      }
     """
  }

  /* Construct all cases to be put in the match */
  def buildCases[T : c.WeakTypeTag, A : c.WeakTypeTag]
                (f: c.Tree, objs: List[c.Tree],
                 parameter: TermName): List[c.Tree] =
    for {
      obj <- objs
      args <- getParamsWithTypes(obj.symbol.info)
      extractorVars = args._1.map(p => pq"$p @ _")
      pat = pq"$obj(..${extractorVars})"
      stat <- caseMatch[T, A](f, obj, parameter, args._1, args._2)
    } yield cq"$pat => $stat"


  /* Creating an enumeration for each one of the parameters:  Seq, Seq of Seq, Optional. */
  def createEnum[T : c.WeakTypeTag, A : c.WeakTypeTag]
                (f: c.Tree, name: TermName, typ: c.Type)/*: List[Option[(TermName, TermName, c.Tree)]] */= {
    val aTpe = implicitly[c.WeakTypeTag[A]]
    val newTree = TermName(c.freshName)
    val newResult = TermName(c.freshName)
    val lhs = pq"($newTree: $typ, $newResult @ _)"
    val rhs = typ match {
      case t if t <:< weakTypeOf[T] =>
        Some(q"$f($name)")
      case t if t <:< weakTypeOf[scala.collection.immutable.Seq[T]] =>
        Some(q"_root_.scala.meta.internal.tql.TraverserHelper.traverseSeq($f, $name)")
      case t if t <:< weakTypeOf[scala.collection.immutable.Seq[scala.collection.immutable.Seq[T]]] =>
        Some(q"_root_.scala.meta.internal.tql.TraverserHelper.traverseSeqOfSeq($f, $name)")
      case t if t <:< weakTypeOf[scala.Option[scala.collection.immutable.Seq[T]]] =>
        Some(q"_root_.scala.meta.internal.tql.TraverserHelper.traverseOptionalSeq($f, $name)")
      case t if t <:< weakTypeOf[scala.Option[T]] =>
        Some(q"_root_.scala.meta.internal.tql.TraverserHelper.optional($f, $name)")
      case _ => None
    }
    rhs.map(r => (newTree, newResult, fq"$lhs <- $r"))
  }


  /* Construct one case match, for each one of the objects. Also received the list of arguments of the current
   * object. */
  def caseMatch[T : c.WeakTypeTag, A : c.WeakTypeTag](f: c.Tree, constructor: c.Tree, origin: TermName,
                                                      names: List[TermName], types: List[c.Type]): Option[c.Tree] = {

    val parameters: List[(TermName, Type)] = names.zip(types)
    val enums = parameters.map(x => createEnum[T, A](f, x._1, x._2))
    val forEnums: List[c.Tree] = enums.flatMap(_.map(_._3))
    val results: List[TermName] = enums.flatMap(_.map(_._2))

    if (!results.isEmpty){
      val addResults = results.tail.foldLeft[c.Tree](q"${results.head}")((a, b) => q"$a + $b")
      val paramsWithNewParams = parameters.unzip._1.zip(enums.map(_.map(_._1)))
      val reconstructParams = paramsWithNewParams.map(x => x._2.getOrElse(x._1))
      val reconstruct = q"$constructor(..$reconstructParams)"
      val eqList = paramsWithNewParams.foldLeft[c.Tree](q"true"){
        (acc, x) => q"$acc && ${x._2.map(y => q"($y eq ${x._1})").getOrElse(q"true")}"}

      val doesReconstruct = q"if ($eqList) $origin else $reconstruct"

      Some(q"""
        for (
          ..$forEnums
        ) yield ($doesReconstruct, $addResults)
      """)
    }
    else
      None
  }
}
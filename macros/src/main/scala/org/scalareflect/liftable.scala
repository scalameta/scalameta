package org.scalareflect
import scala.reflect.api.Universe
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

trait LiftableMaterializer {
  val u: Universe
  def materialize[T]: Any = macro LiftableMaterializerImpl.expand[T]
}

class LiftableMaterializerImpl(val c: Context) { import c.universe._
  val q"$prefix.materialize[..$_]" = c.macroApplication
  val u = q"$prefix.u"

  def unreachable: Nothing = c.abort(c.macroApplication.pos, "unreachable")

  def fieldNames(sym: ClassSymbol) =
    sym.info.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head.map { _.name }

  def isBranch(sym: ClassSymbol) = sym.isTrait && sym.isSealed
  def isLeaf(sym: ClassSymbol) = sym.isCaseClass && sym.isFinal

  def leafs(sym: ClassSymbol): List[ClassSymbol] =
    if (isLeaf(sym)) List(sym)
    else if (isBranch(sym)) sym.knownDirectSubclasses.toList.flatMap { case cs: ClassSymbol => cs.info; leafs(cs) }
    else unreachable

  def materialize(sym: ClassSymbol): (c.Tree, List[ClassSymbol]) = {
    sym.info
    val (body, deps) =
      if (isBranch(sym)) {
        val subclasses = sym.knownDirectSubclasses.map {
          case cs: ClassSymbol => cs.info; cs
        }.toList.sortBy(sym => (isLeaf(sym), sym.fullName.toString))
        val included = {
          def loop(totry: List[ClassSymbol],
                   included: List[ClassSymbol]): List[ClassSymbol] = totry match {
            case Nil          => included
            case head :: tail =>
              if (leafs(head).toSet subsetOf included.map(leafs).flatten.toSet) {
                loop(tail, included)
              } else
                loop(tail, head :: included)
          }
          loop(subclasses, Nil).reverse
        }
        val cases = included.map { subcls =>
          cq""" x: $subcls => implicitly[$u.Liftable[$subcls]].apply(x) """
        }
        val body = if (cases.nonEmpty) q"(value: @_root_.scala.unchecked) match { case ..$cases }" else q"???"
        (body, subclasses.toList)

      } else if (isLeaf(sym)) {
        val arguments = fieldNames(sym).map { case name: TermName => q""" q"$${value.$name}" """ }
        val body = q"$u.Apply($u.reify(${c.mirror.staticModule(sym.fullName)}).tree, List(..$arguments))"
        (body, Nil)
      } else
        unreachable
    val objName = TermName(s"Liftable for ${sym.fullName.replace(".", "_")}")
    val expansion = q"""
      implicit object $objName extends $u.Liftable[$sym] {
        def apply(value: $sym) = $body
      }
    """
    (expansion, deps)
  }

  def recursivelyMaterialize(rootSym: ClassSymbol): List[c.Tree] = {
    def loop(toExpand: List[ClassSymbol], alreadyExpanded: Set[ClassSymbol]): List[c.Tree] = toExpand match {
      case Nil => Nil
      case head :: tail =>
        val (expansion, deps) = materialize(head)
        val newAlreadyExpanded = alreadyExpanded + head
        val newToExpand = (deps ++ tail).filter(dep => !newAlreadyExpanded.contains(dep))
        expansion :: loop(newToExpand, newAlreadyExpanded)
    }
    loop(rootSym :: Nil, Set.empty)
  }

  def expand[T: c.WeakTypeTag]: c.Tree = {
    val instances = recursivelyMaterialize(weakTypeOf[T].typeSymbol.asClass)
    q"new { import $u._; ..$instances }"
  }
}

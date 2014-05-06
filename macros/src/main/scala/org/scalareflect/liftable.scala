package org.scalareflect
import scala.reflect.api.Universe
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

trait LiftableMaterializer {
  val u: Universe
  def materialize[T]: Any = macro LiftableMaterializerImpl.expand[T]
}

class LiftableMaterializerImpl(val c: Context) {
  import c.universe._, internal.gen.{mkAttributedRef => attrRef}

  val q"$prefix.materialize[..$_]" = c.macroApplication
  val u = q"$prefix.u"

  def fields(sym: ClassSymbol) =
    sym.info.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head.map { _.name }

  def materialize(sym: ClassSymbol): (c.Tree, List[ClassSymbol]) = {
    sym.info
    val (body, deps) =
      if (sym.isTrait && sym.isSealed) {
        val subclasses = sym.knownDirectSubclasses.toList.map { case cs: ClassSymbol => cs }
        val cases = subclasses.map { subcls => cq""" x: ${attrRef(subcls)} => q"$$x" """ }
        val body = if (cases.nonEmpty) q"value match { case ..$cases }" else q"???"
        (body, subclasses)
      } else if (sym.isCaseClass && sym.isFinal) {
        val arguments = fields(sym).map { case name: TermName => q""" q"$${value.$name}" """ }
        val body = q"$u.Apply($u.reify(${attrRef(c.mirror.staticModule(sym.fullName))}).tree, List(..$arguments))"
        (body, Nil)
      } else
        c.abort(c.macroApplication.pos, s"can't materialize liftable for $sym")
    val objName = TermName(s"Liftable for ${sym.fullName.replace(".", "_")}")
    val expansion = q"""
      implicit object $objName extends $u.Liftable[${attrRef(sym)}] {
        def apply(value: ${attrRef(sym)}) = $body
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
        val newToExpand = (deps ++ tail).filter(dep => !alreadyExpanded.contains(dep))
        expansion :: loop(newToExpand, newAlreadyExpanded)
    }
    loop(rootSym :: Nil, Set.empty)
  }

  def expand[T: c.WeakTypeTag]: c.Tree = {
    val instances = recursivelyMaterialize(weakTypeOf[T].typeSymbol.asClass)
    q"new { import $u._; ..$instances }"
  }
}

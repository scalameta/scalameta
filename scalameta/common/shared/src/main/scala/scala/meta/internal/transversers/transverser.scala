package scala.meta
package internal
package transversers

import org.scalameta.internal.MacroHelpers
import scala.meta.internal.trees.{Reflection => AstReflection}

import scala.reflect.macros.whitebox.Context

trait TransverserMacros extends MacroHelpers with AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  val c: Context
  import c.universe._

  lazy val TreeClass = tq"_root_.scala.meta.Tree"
  lazy val TreeAdt = TreeSymbol.asRoot
  lazy val QuasiAdt = QuasiSymbol.asAdt
  lazy val Hack1Class = hygienicRef[org.scalameta.overload.Hack1]
  lazy val Hack2Class = hygienicRef[org.scalameta.overload.Hack2]
  lazy val Hack3Class = hygienicRef[org.scalameta.overload.Hack3]
  lazy val Hack4Class = hygienicRef[org.scalameta.overload.Hack4]

  private lazy val TermAdt = mirror.staticClass("scala.meta.Term").asAdt
  private lazy val TypeAdt = mirror.staticClass("scala.meta.Type").asAdt
  private lazy val DefnAdt = mirror.staticClass("scala.meta.Defn").asAdt

  def leafHandler(l: Leaf, treeName: TermName): Tree
  def leafHandlerType(): Tree
  def generatedMethods(): Tree

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
        cdef

      val cdef1 = q"""
        $mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self =>
          ..$stats
          ..${getPrimaryApply()}
          ..${generatedMethods()}
        }
      """
      List(cdef1, mdef)
    }
  })

  private def getSecondaryApply(prefix: String, leaves: List[Leaf])(priority: String*): Tree = {
    val treeName = TermName("_tree")
    val cases = leaves.sortBy { l =>
      val idx = priority.indexOf(l.prefix)
      if (idx != -1) idx else priority.length
    }.map(l => cq"$treeName: ${hygienicRef(l.sym)} => ${leafHandler(l, treeName)}")
    val methodName = TermName(s"apply$prefix")

    q"""
      private def $methodName(tree: $TreeClass): ${leafHandlerType()} = {
        tree match { case ..$cases }
      }
    """
  }

  private def getPrimaryApply(): Tree = {
    val termBuilder = List.newBuilder[Leaf]
    val typeBuilder = List.newBuilder[Leaf]
    val defnBuilder = List.newBuilder[Leaf]
    val restBuilder = List.newBuilder[Leaf]
    TreeAdt.allLeafs.foreach { l =>
      if (l <:< QuasiAdt) {} // do nothing
      else if (l <:< TermAdt) termBuilder += l
      else if (l <:< TypeAdt) typeBuilder += l
      else if (l <:< DefnAdt) defnBuilder += l
      else restBuilder += l
    }

    val termPriority = Seq("Term.Name", "Term.Apply", "Lit", "Term.Param", "Term.ApplyInfix")
    val termTree = getSecondaryApply("Term", termBuilder.result())(termPriority: _*)
    val typeTree = getSecondaryApply("Type", typeBuilder.result())("Type.Name", "Type.Apply")
    val defnTree = getSecondaryApply("Defn", defnBuilder.result())()
    val restTree = getSecondaryApply("Rest", restBuilder.result())()

    q"""
      def apply(tree: $TreeClass): ${leafHandlerType()} = {
        tree match {
          case t: ${hygienicRef(TermAdt.sym)} => applyTerm(t)
          case t: ${hygienicRef(TypeAdt.sym)} => applyType(t)
          case t: ${hygienicRef(DefnAdt.sym)} => applyDefn(t)
          case t => applyRest(t)
        }
      }
      ..$termTree
      ..$typeTree
      ..$defnTree
      ..$restTree
    """
  }

}

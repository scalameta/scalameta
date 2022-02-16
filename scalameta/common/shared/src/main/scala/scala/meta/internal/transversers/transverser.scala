package scala.meta
package internal
package transversers

import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers
import scala.meta.internal.trees.{Reflection => AstReflection}

trait TransverserMacros extends MacroHelpers with AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  val c: Context
  import c.universe._

  lazy val TreeClass = tq"_root_.scala.meta.Tree"
  lazy val TreeAdt = TreeSymbol.asRoot
  lazy val QuasiClass = tq"_root_.scala.meta.internal.trees.Quasi"
  lazy val QuasiAdt = QuasiSymbol.asAdt
  lazy val Hack1Class = hygienicRef[org.scalameta.overload.Hack1]
  lazy val Hack2Class = hygienicRef[org.scalameta.overload.Hack2]
  lazy val Hack3Class = hygienicRef[org.scalameta.overload.Hack3]
  lazy val Hack4Class = hygienicRef[org.scalameta.overload.Hack4]

  def leafHandler(l: Leaf, treeName: TermName): Tree
  def generatedMethods(cases: List[CaseDef]): Tree

  def impl(annottees: Tree*): Tree =
    annottees.transformAnnottees(new ImplTransformer {
      override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
        val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =
          cdef

        val generatedMethods = getGeneratedMethods()

        val cdef1 = q"""
        $mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self =>
          ..$stats
          ..$generatedMethods
        }
      """
        List(cdef1, mdef)
      }
    })

  private def getGeneratedMethods(): Tree = {
    val treeName = TermName("_tree")
    val leaves = TreeAdt.allLeafs.filter(l => !(l <:< QuasiAdt))
    val priority = List(
      "Term.Name",
      "Term.Apply",
      "Lit",
      "Type.Name",
      "Term.Param",
      "Type.Apply",
      "Term.ApplyInfix"
    )
    val cases = leaves
      .sortBy { l =>
        val idx = priority.indexOf(l.prefix)
        if (idx != -1) idx else priority.length
      }
      .map { l =>
        val extractor = hygienicRef(l.sym.companion)
        val binders = l.fields.map(f => pq"${f.name}")
        val handler = leafHandler(l, treeName)
        cq"$treeName @ $extractor(..$binders) => $handler"
      }
    TransverserMacros.this.generatedMethods(cases)
  }
}

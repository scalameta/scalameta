package scala.meta
package internal
package transversers

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers
import scala.meta.internal.ast.{Reflection => AstReflection}

trait TransverserMacros extends MacroHelpers with AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  val c: Context
  import c.universe._

  lazy val TreeClass = tq"_root_.scala.meta.Tree"
  lazy val TreeAdt = TreeSymbol.asRoot
  lazy val QuasiClass = tq"_root_.scala.meta.internal.ast.Quasi"
  lazy val QuasiAdt = QuasiSymbol.asAdt
  lazy val Hack1Class = hygienicRef[org.scalameta.overload.Hack1]
  lazy val Hack2Class = hygienicRef[org.scalameta.overload.Hack2]
  lazy val Hack3Class = hygienicRef[org.scalameta.overload.Hack3]
  lazy val Hack4Class = hygienicRef[org.scalameta.overload.Hack4]

  def leafHandler(l: Leaf): Tree
  def generatedMethods(cases: List[CaseDef]): Tree

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef

      val relevantLeafs = TreeAdt.allLeafs.filter(l => !(l <:< QuasiAdt))
      val highPriority = List(
        "Term.Name",
        "Term.Apply",
        "Lit",
        "Type.Name",
        "Term.Param",
        "Type.Apply",
        "Term.ApplyInfix"
      )
      val orderedRelevantLeafs = relevantLeafs.sortBy(l => {
        val idx = highPriority.indexOf(l.prefix)
        if (idx != -1) idx else highPriority.length
      })

      val cases = orderedRelevantLeafs.map(l => {
        val extractor = hygienicRef(l.sym.companion)
        val binders = l.fields.map(f => pq"${f.name}")
        val relevantFields = l.fields.filter(f => !(f.tpe =:= typeOf[Any]) && !(f.tpe =:= typeOf[String]))
        cq"tree @ $extractor(..$binders) => ${leafHandler(l)}"
      })
      val generatedMethods = TransverserMacros.this.generatedMethods(cases)

      val cdef1 = q"""
        $mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self =>
          ..$stats
          ..$generatedMethods
        }
      """
      List(cdef1, mdef)
    }
  })
}

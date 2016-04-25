package scala.meta
package internal
package classifiers

import scala.reflect.macros.whitebox.Context
import org.scalameta.internal.MacroHelpers
import scala.meta.classifiers.Classifier

// TODO: It really sucks that `@foo object Bar` can't produce `class Bar`.
// Because of that, we have to uglify our classifier DSL.
class ClassifierMacros(val c: Context) extends MacroHelpers {
  import c.universe._

  lazy val ClassifierClass = hygienicRef[Classifier[_, _]]

  def classifier(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformTrait(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"$mods trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef

      def collectUnapplySig(stats: List[Tree]): Option[(Tree, Tree)] = {
        stats.collectFirst { case q"$_ def unapply[..$tparams]($_: $t): $ret = $_" => (t, ret) }
      }
      val unapplySig = collectUnapplySig(mstats).orElse(collectUnapplySig(stats))
      val t = unapplySig match {
        case Some((t, _)) => t // TODO: validate ret or emit different code if it's not Boolean
        case None => c.abort(c.enclosingPosition, "only traits with an unapply method can be @classifier")
      }
      val evidence = q"""
        implicit def classifier[T <: $t]: $ClassifierClass[T, $name] = new $ClassifierClass[T, $name] {
          def apply(x: T): $BooleanClass = $mname.unapply(x)
        }
      """

      val mstats1 = mstats ++ stats ++ List(evidence)
      val cdef1 = q"$mods trait $name[..$tparams] extends { ..$earlydefns } with ..$parents { $self => }"
      val mdef1 = q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}
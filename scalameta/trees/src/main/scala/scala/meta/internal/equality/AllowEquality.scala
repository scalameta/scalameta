package scala.meta
package internal
package equality

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.meta.internal.ast.{Reflection => AstReflection}

trait AllowEquality[T1, T2]
object AllowEquality {
  implicit def materialize[T1, T2]: AllowEquality[T1, T2] = macro Macros.allow[T1, T2]
}

class Macros(val c: BlackboxContext) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror

  import c.universe._
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  lazy val StatSymbol = rootMirror.staticClass("scala.meta.Stat")
  lazy val ScopeSymbol = rootMirror.staticClass("scala.meta.Scope")
  lazy val isTooGeneral = Set[Symbol](TreeSymbol, StatSymbol, ScopeSymbol)

  def allow[T1, T2](implicit T1: WeakTypeTag[T1], T2: WeakTypeTag[T2]): Tree = {
    def fail() = c.abort(c.enclosingPosition, s"can't compare ${T1.tpe} and ${T2.tpe}")
    if ((T1.tpe <:< TreeSymbol.toType) && (T2.tpe <:< TreeSymbol.toType)) {
      def extractAdts(t: Type): List[Adt] = t match {
        case RefinedType(parents, _) => parents.flatMap(extractAdts)
        case tpe if tpe.typeSymbol.isAdt => List(tpe.typeSymbol.asAdt)
        case _ => Nil
      }
      def allowEquality(t1: Adt, t2: Adt): Boolean = {
        def commonBranches(t1: Adt, t2: Adt): List[Adt] = {
          if (t1.isInstanceOf[Root] || t2.isInstanceOf[Root]) Nil
          else if (isTooGeneral(t1.sym) || isTooGeneral(t2.sym)) Nil
          else if (t1 == t2) List(t1)
          else {
            t1.parents.flatMap(t1 => commonBranches(t1, t2)) ++
            t2.parents.flatMap(t2 => commonBranches(t1, t2))
          }
        }
        t1 == t2 || commonBranches(t1, t2).nonEmpty
      }
      val t1s = extractAdts(T1.tpe)
      val t2s = extractAdts(T2.tpe)
      if (t1s.exists(t1 => t2s.exists(t2 => allowEquality(t1, t2)))) q"null"
      else fail()
    } else {
      fail()
    }
  }
}

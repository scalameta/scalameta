package scala.meta
package internal
package tql

import org.scalameta.ast.{Reflection => AstReflection}
import scala.reflect.macros.blackbox.Context
import scala.meta.tql._

private[meta] class AllowedTransformationMacros(val c: Context) extends AstReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"

  def materialize[T : c.WeakTypeTag, I : c.WeakTypeTag, O : c.WeakTypeTag]: c.Expr[AllowedTransformation[I, O]] = {
    val brlhs = getBranch[I].filterNot(_.fullName == u.symbolOf[T].fullName)
    val Tout = u.symbolOf[O].asType
    //c.abort(c.enclosingPosition, show(Tout) + " : " + show(brlhs.filter(x => Tout.toType <:< x.info.typeSymbol.asType.toType)))

    if (brlhs.exists(x => Tout.toType <:< x.info.typeSymbol.asType.toType))
      c.Expr(q"new _root_.scala.meta.tql.AllowedTransformation[${implicitly[c.WeakTypeTag[I]]}, ${implicitly[c.WeakTypeTag[O]]}] {}")
    else
      c.abort(c.enclosingPosition,
        "impossible to materialize AllowedTransformations[" +
          show(implicitly[c.WeakTypeTag[I]].tpe) + ", " +
          show(implicitly[c.WeakTypeTag[O]].tpe) + "]" + "\n" +
          "because " + show(Tout.fullName) + " is not a subtype of any in " + show(brlhs) + "\n")
  }

  private def getBranch[A : c.WeakTypeTag]: List[TypeSymbol] = {
    val Asym = u.symbolOf[A]
    if (Asym.isBranch) List(Asym.asType)
    else if (Asym.isLeaf)
      Asym.asLeaf.sym.asClass.baseClasses
        .filter(_.isBranch)
        //.filter(_.asBranch.leafs.exists(x => x.sym.fullName == Asym.asLeaf.sym.fullName))//gotta find a better solution
        .map(_.asType)
    else c.abort(c.enclosingPosition, show("impossible to get branch from  "  + show(implicitly[c.WeakTypeTag[A]])))
  }
}

package scala.meta
package trees

import scala.collection.mutable
import scala.language.implicitConversions

private[meta] trait Api {
  implicit def typeParamClauseToValues(v: Type.ParamClause): List[Type.Param] = v.values
  implicit def typeValuesToParamClause(v: List[Type.Param]): Type.ParamClause = Type.ParamClause(v)

  implicit def termParamClauseToValues(v: Term.ParamClause): List[Term.Param] = v.values
  implicit def termValuesToParamClause(v: List[Term.Param]): Term.ParamClause = {
    val seen = mutable.Set.empty[Class[_]]
    val mods = List.newBuilder[Mod.ParamsType]
    v.foreach(_.mods.foreach {
      case x: Mod.ParamsType if seen.add(x.getClass) => mods += x
      case _ =>
    })
    Term.ParamClause(v, mods.result())
  }
  implicit def termListValuesToListParamClause(v: List[List[Term.Param]]): List[Term.ParamClause] =
    v.map(termValuesToParamClause)
}

private[meta] trait Aliases {
  // NOTE: all trees are defined immediately in the scala.meta package,
  // so we don't need to alias them here.
}

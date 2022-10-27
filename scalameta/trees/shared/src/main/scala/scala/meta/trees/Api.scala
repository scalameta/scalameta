package scala.meta
package trees

import scala.collection.mutable
import scala.language.implicitConversions

private[meta] trait Api {
  implicit def typeParamClauseToValues(v: Type.ParamClause): List[Type.Param] = v.values
  implicit def typeValuesToParamClause(v: List[Type.Param]): Type.ParamClause = Type.ParamClause(v)

  implicit def termParamClauseToValues(v: Term.ParamClause): List[Term.Param] = v.values
  implicit def termValuesToParamClause(v: List[Term.Param]): Term.ParamClause =
    Term.ParamClause(v, v.flatMap(_.mods).collectFirst { case x: Mod.ParamsType => x })
  implicit def termListValuesToListParamClause(v: List[List[Term.Param]]): List[Term.ParamClause] =
    v.map(termValuesToParamClause)
}

private[meta] trait Aliases {
  // NOTE: all trees are defined immediately in the scala.meta package,
  // so we don't need to alias them here.
}

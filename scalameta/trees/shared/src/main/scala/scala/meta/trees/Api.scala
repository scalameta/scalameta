package scala.meta
package trees

import scala.collection.mutable
import scala.language.implicitConversions

private[meta] trait Api {
  implicit def typeParamClauseToValues(v: Type.ParamClause): List[Type.Param] = v.values
  implicit def typeValuesToParamClause(v: List[Type.Param]): Type.ParamClause = Type.ParamClause(v)

  implicit def termParamClauseToValues(v: Term.ParamClause): List[Term.Param] = v.values
  implicit def termValuesToParamClause(v: List[Term.Param]): Term.ParamClause =
    Term.ParamClause(v, Term.ParamClause.getMod(v))
  implicit def termListValuesToListParamClause(v: List[List[Term.Param]]): List[Term.ParamClause] =
    v.map(termValuesToParamClause)

  implicit def typeArgsToValues(v: Type.ArgClause): List[Type] = v.values
  implicit def typeValuesToArgClause(v: List[Type]): Type.ArgClause = Type.ArgClause(v)
}

private[meta] trait Aliases {
  // NOTE: all trees are defined immediately in the scala.meta package,
  // so we don't need to alias them here.
}

package scala.meta
package trees

import scala.collection.mutable
import scala.language.implicitConversions

private[meta] trait ApiLowPriority {
  implicit def typeValuesToParamClause(v: List[Type.Param]): Type.ParamClause = Type.ParamClause(v)

  implicit def termValuesToParamClause(v: List[Term.Param]): Term.ParamClause =
    Term.ParamClause(v, Term.ParamClause.getMod(v))
  implicit def termListValuesToListParamClause(v: List[List[Term.Param]]): List[Term.ParamClause] =
    v.map(termValuesToParamClause)

  implicit def typeValuesToArgClause(v: List[Type]): Type.ArgClause = Type.ArgClause(v)

  implicit def typeValuesToFuncParamClause(v: List[Type]): Type.FuncParamClause =
    Type.FuncParamClause(v)

  implicit def termValuesToArgClause(v: List[Term]): Term.ArgClause = Term.ArgClause(v, None)
  implicit def termListValuesToListArgClause(v: List[List[Term]]): List[Term.ArgClause] =
    v.map(termValuesToArgClause)

  implicit def patValuesToArgClause(v: List[Pat]): Pat.ArgClause = Pat.ArgClause(v)
  implicit def patListValuesToListArgClause(v: List[List[Pat]]): List[Pat.ArgClause] =
    v.map(patValuesToArgClause)
}

private[meta] trait Api extends ApiLowPriority {
  implicit def typeParamClauseToValues(v: Type.ParamClause): List[Type.Param] = v.values
  implicit def typeValuesToParamClauseWithDialect(v: List[Type.Param])(
      implicit dialect: Dialect
  ): Type.ParamClause = Type.ParamClause(v)

  implicit def termParamClauseToValues(v: Term.ParamClause): List[Term.Param] = v.values
  implicit def termValuesToParamClauseWithDialect(v: List[Term.Param])(
      implicit dialect: Dialect
  ): Term.ParamClause =
    Term.ParamClause(v, Term.ParamClause.getMod(v))
  implicit def termListValuesToListParamClauseWithDialect(v: List[List[Term.Param]])(
      implicit dialect: Dialect
  ): List[Term.ParamClause] =
    v.map(termValuesToParamClauseWithDialect)

  implicit def typeArgsToValues(v: Type.ArgClause): List[Type] = v.values
  implicit def typeValuesToArgClauseWithDialect(v: List[Type])(
      implicit dialect: Dialect
  ): Type.ArgClause = Type.ArgClause(v)

  implicit def typeFuncParamsToValues(v: Type.FuncParamClause): List[Type] = v.values
  implicit def typeValuesToFuncParamClauseWithDialect(v: List[Type])(
      implicit dialect: Dialect
  ): Type.FuncParamClause =
    Type.FuncParamClause(v)

  implicit def termArgsToValues(v: Term.ArgClause): List[Term] = v.values
  implicit def termValuesToArgClauseWithDialect(v: List[Term])(
      implicit dialect: Dialect
  ): Term.ArgClause = Term.ArgClause(v, None)
  implicit def termListValuesToListArgClauseWithDialect(v: List[List[Term]])(
      implicit dialect: Dialect
  ): List[Term.ArgClause] =
    v.map(termValuesToArgClauseWithDialect)

  implicit def patArgsToValues(v: Pat.ArgClause): List[Pat] = v.values
  implicit def patValuesToArgClauseWithDialect(v: List[Pat])(
      implicit dialect: Dialect
  ): Pat.ArgClause = Pat.ArgClause(v)
  implicit def patListValuesToListArgClauseWithDialect(v: List[List[Pat]])(
      implicit dialect: Dialect
  ): List[Pat.ArgClause] =
    v.map(patValuesToArgClauseWithDialect)
}

private[meta] trait Aliases {
  // NOTE: all trees are defined immediately in the scala.meta package,
  // so we don't need to alias them here.
}

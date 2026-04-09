package scala.meta
package internal.prettyprinters

import scala.meta.classifiers._

// NOTE: these groups closely follow non-terminals in the grammar spec from SLS, except for:
// 1) we don't care about tracking non-terminals (with m() and/or p()) when that doesn't affect parenthesization
// 2) `InfixType ::= CompoundType {id [nl] CompoundType}` is incorrect. Should be `CompoundType | InfixType {id [nl] InfixType}`
// 3) `Pattern2 ::= varid ['@' Pattern3]` has become `Pattern2 ::= varid ['@' AnyPattern3]` due to implementational reasons
// 4) `Type ::= ... | InfixType [ExistentialClause]` has become `Type ::= ... | AnyInfixType [ExistentialClause]` due to implementational reasons
// 5) `FunctionArgTypes ::= InfixType | ...` has become `Type ::= AnyInfixType | ...` due to implementational reasons
sealed trait TreeSyntacticGroup {
  def categories: Int
  def precedence: Int
}

object TreeSyntacticGroup {
  trait InfixGroup extends TreeSyntacticGroup {
    def ai: Member.Infix
  }

  trait SimpleGroup extends TreeSyntacticGroup {
    def precedence = 60
  }

  // types
  private val typeCategory = 0x1
  trait TypeGroup extends TreeSyntacticGroup {
    def categories: Int = typeCategory
  }
  object ParamTyp extends TypeGroup {
    def precedence = 0
  }
  object Typ extends TypeGroup {
    def precedence = 10
  }
  object AnyInfixTyp extends TypeGroup {
    def precedence = 15
  }
  class InfixTyp(val ai: Type.ApplyInfix) extends TypeGroup with InfixGroup {
    def precedence = 20
  }
  object RefineTyp extends TypeGroup {
    def precedence = 30
  }
  object WithTyp extends TypeGroup {
    def precedence = 35
  }
  object AnnotTyp extends TypeGroup {
    def precedence = 40
  }
  object SimpleTyp extends TypeGroup with SimpleGroup

  // terms / expressions
  private val exprCategory = 0x2
  trait ExprGroup extends TreeSyntacticGroup {
    def categories: Int = exprCategory
  }
  object Expr extends ExprGroup {
    def precedence = 0
  }
  object Expr1 extends ExprGroup {
    def precedence = 10
  }
  object PostfixExpr extends ExprGroup {
    def precedence = 20
  }
  class InfixExpr(val ai: Term.ApplyInfix) extends ExprGroup with InfixGroup {
    def precedence = 30
  }
  object PrefixExpr extends ExprGroup {
    def precedence = 40
  }
  object SimpleExpr extends ExprGroup {
    def precedence = 50
  }
  object SimpleExpr1 extends ExprGroup with SimpleGroup

  // patterns
  private val patCategory = 0x4
  trait PatGroup extends TreeSyntacticGroup {
    def categories: Int = patCategory
  }
  object Pattern extends PatGroup {
    def precedence = 0
  }
  object Pattern1 extends PatGroup {
    def precedence = 10
  }
  object Pattern2 extends PatGroup {
    def precedence = 20
  }
  object AnyPattern3 extends PatGroup {
    def precedence = 25
  }
  class InfixPat(val ai: Pat.ExtractInfix) extends PatGroup with InfixGroup {
    def precedence = 30
  }
  object SimplePattern extends PatGroup with SimpleGroup

  trait AllSimpleGroup extends ExprGroup with PatGroup with TypeGroup with SimpleGroup {
    override def categories: Int = typeCategory | exprCategory | patCategory
  }

  object Literal extends AllSimpleGroup
  object Path extends AllSimpleGroup

  def opNeedsParens(
      outerPrecedence: Int,
      innerPrecedence: Int,
      ifSamePrecedence: => Boolean
  ): Boolean = {
    val diffPrecedence = outerPrecedence - innerPrecedence
    diffPrecedence > 0 || diffPrecedence == 0 && ifSamePrecedence
  }

  def opNeedsParens(outerOp: Name, innerOp: Name, isLhs: => Boolean)(implicit
      dialect: Dialect
  ): Boolean = {
    val outerIsLeftAssoc = outerOp.isLeftAssoc
    @inline
    def ifSamePrecedence = outerIsLeftAssoc != isLhs
    if (outerIsLeftAssoc != innerOp.isLeftAssoc) true // not really true but visually clearer
    else if (!outerOp.is[Type] || dialect.useInfixTypePrecedence)
      opNeedsParens(outerOp.precedence, innerOp.precedence, ifSamePrecedence)
    else ifSamePrecedence
  }

  def opNeedsParens(outer: Member.Infix, inner: Member.Infix)(implicit dialect: Dialect): Boolean =
    opNeedsParens(outer.op, inner.op, outer.lhs eq inner)

  def groupNeedsParens(outer: TreeSyntacticGroup, inner: TreeSyntacticGroup)(implicit
      dialect: Dialect
  ): Boolean = {
    require((outer.categories & inner.categories) != 0)
    (outer, inner) match {
      case (outer: InfixGroup, inner: InfixGroup) => opNeedsParens(outer.ai, inner.ai)
      case _ => outer.precedence > inner.precedence
    }
  }

}

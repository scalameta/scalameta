package scala.meta
package internal.prettyprinters

import org.scalameta.adt.{branch, leaf, root}
import org.scalameta.invariants._
import scala.meta.classifiers._

// NOTE: these groups closely follow non-terminals in the grammar spec from SLS, except for:
// 1) we don't care about tracking non-terminals (with m() and/or p()) when that doesn't affect parenthesization
// 2) `InfixType ::= CompoundType {id [nl] CompoundType}` is incorrect. Should be `CompoundType | InfixType {id [nl] InfixType}`
// 3) `Pattern2 ::= varid ['@' Pattern3]` has become `Pattern2 ::= varid ['@' AnyPattern3]` due to implementational reasons
// 4) `Type ::= ... | InfixType [ExistentialClause]` has become `Type ::= ... | AnyInfixType [ExistentialClause]` due to implementational reasons
// 5) `FunctionArgTypes ::= InfixType | ...` has become `Type ::= AnyInfixType | ...` due to implementational reasons
@root
trait TreeSyntacticGroup {
  def categories: Int
  def precedence: Int
}

object TreeSyntacticGroup {
  @branch
  trait InfixGroup extends TreeSyntacticGroup {
    def ai: Member.Infix
  }

  @branch
  trait SimpleGroup extends TreeSyntacticGroup {
    def precedence = 60
  }

  // types
  private val typeCategory = 0x1
  @branch
  trait TypeGroup extends TreeSyntacticGroup {
    def categories: Int = typeCategory
  }
  @leaf
  object ParamTyp extends TypeGroup {
    def precedence = 0
  }
  @leaf
  object Typ extends TypeGroup {
    def precedence = 10
  }
  @leaf
  object AnyInfixTyp extends TypeGroup {
    def precedence = 15
  }
  @leaf
  class InfixTyp(ai: Type.ApplyInfix) extends TypeGroup with InfixGroup {
    def precedence = 20
  }
  @leaf
  object RefineTyp extends TypeGroup {
    def precedence = 30
  }
  @leaf
  object WithTyp extends TypeGroup {
    def precedence = 35
  }
  @leaf
  object AnnotTyp extends TypeGroup {
    def precedence = 40
  }
  @leaf
  object SimpleTyp extends TypeGroup with SimpleGroup

  // terms / expressions
  private val exprCategory = 0x2
  @branch
  trait ExprGroup extends TreeSyntacticGroup {
    def categories: Int = exprCategory
  }
  @leaf
  object Expr extends ExprGroup {
    def precedence = 0
  }
  @leaf
  object Expr1 extends ExprGroup {
    def precedence = 10
  }
  @leaf
  object PostfixExpr extends ExprGroup {
    def precedence = 20
  }
  @leaf
  class InfixExpr(ai: Term.ApplyInfix) extends ExprGroup with InfixGroup {
    def precedence = 30
  }
  @leaf
  object PrefixExpr extends ExprGroup {
    def precedence = 40
  }
  @leaf
  object SimpleExpr extends ExprGroup {
    def precedence = 50
  }
  @leaf
  object SimpleExpr1 extends ExprGroup with SimpleGroup

  // patterns
  private val patCategory = 0x4
  @branch
  trait PatGroup extends TreeSyntacticGroup {
    def categories: Int = patCategory
  }
  @leaf
  object Pattern extends PatGroup {
    def precedence = 0
  }
  @leaf
  object Pattern1 extends PatGroup {
    def precedence = 10
  }
  @leaf
  object Pattern2 extends PatGroup {
    def precedence = 20
  }
  @leaf
  object AnyPattern3 extends PatGroup {
    def precedence = 25
  }
  @leaf
  class InfixPat(ai: Pat.ExtractInfix) extends PatGroup with InfixGroup {
    def precedence = 30
  }
  @leaf
  object SimplePattern extends PatGroup with SimpleGroup

  @branch
  trait AllSimpleGroup extends ExprGroup with PatGroup with TypeGroup with SimpleGroup {
    override def categories: Int = typeCategory | exprCategory | patCategory
  }

  @leaf
  object Literal extends AllSimpleGroup
  @leaf
  object Path extends AllSimpleGroup

  def opNeedsParensSameAssoc(
      outerPrecedence: Int,
      innerPrecedence: Int,
      isLeftAssoc: => Boolean,
      isLhs: => Boolean
  ): Boolean = {
    val diffPrecedence = outerPrecedence - innerPrecedence
    if (diffPrecedence < 0) !isLeftAssoc
    else if (diffPrecedence > 0) isLeftAssoc
    else isLeftAssoc != isLhs
  }

  def opNeedsParens(outerOp: Name, innerOp: Name, isLhs: => Boolean)(implicit
      dialect: Dialect
  ): Boolean = {
    val outerIsLeftAssoc = outerOp.isLeftAssoc
    if (outerIsLeftAssoc != innerOp.isLeftAssoc) true
    else if (!outerOp.is[Type] || dialect.useInfixTypePrecedence)
      opNeedsParensSameAssoc(outerOp.precedence, innerOp.precedence, outerIsLeftAssoc, isLhs)
    else outerIsLeftAssoc != isLhs
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

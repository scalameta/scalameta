package org.scalameta.invariants

import org.scalameta.internal.MacroHelpers

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Macros(val c: Context) extends MacroHelpers {
  import c.universe._

  def require(requirement: Tree): Tree = requireWithClue(requirement, q"null")

  def requireWithClue(requirement: Tree, clue: Tree): Tree = {
    val failures = c.freshName(TermName("failures"))
    val allDebuggees = freeLocals(requirement) ++ debuggees(requirement)
    q"""
        val $failures = ${instrument(requirement)}
        if (!$failures.isEmpty)
          $InvariantFailedRaiseMethod(${showCode(requirement)}, $clue, $failures, $allDebuggees)
      """
  }

  def requireCast[U](ev: c.Tree)(U: c.WeakTypeTag[U]): c.Tree = {
    val q"$_($x)" = c.prefix.tree
    val clue = s"${showCode(x)} can not be cast to type ${showDecl(U.tpe.typeSymbol)}"
    q"""
        val temp = ${c.untypecheck(x)}
        val tempClass = if (temp != null) temp.getClass else null
        $InvariantsRequireMethod(
          tempClass != null && _root_.scala.reflect.classTag[$U].unapply(temp).isDefined,
          $clue
        )
        temp.asInstanceOf[$U]
      """
  }

  private def instrument(tree: Tree): Tree = {
    sealed abstract class Prop {
      lazy val result = c.freshName(TermName("result"))
      lazy val failures = c.freshName(TermName("failures"))
      lazy val temp = c.freshName(TermName("temp"))
      def emit: Tree
    }
    sealed trait Simple extends Prop {
      def diagnostic: String
      def tree: Tree
      override def emit = q"""
            val $result = $tree
            if ($result) _root_.scala.collection.immutable.Nil
            else _root_.scala.collection.immutable.List($diagnostic)
          """
    }
    case class Debug() extends Prop {
      override def emit = q"_root_.scala.collection.immutable.Nil"
    }
    case class Atom(tree: Tree) extends Prop with Simple {
      override def diagnostic = showCode(tree) + " is false"
    }
    case class Not(prop: Prop) extends Prop with Simple {
      override def tree = q"!${prop.asInstanceOf[Atom].tree}"
      override def diagnostic = showCode(tree) + " is true"
    }
    case class And(props: Prop*) extends Prop {
      override def emit = {
        def loop(props: List[Prop]): Tree = props match {
          case Nil => ???
          case prop :: Nil => prop.emit
          case prop :: rest => q"""
                val $failures = ${prop.emit}
                if (!$failures.isEmpty) $failures
                else ${loop(rest)}
              """
        }
        loop(props.toList)
      }
    }
    case class Or(props: Prop*) extends Prop {
      override def emit = {
        def loop(props: List[Prop]): Tree = props match {
          case Nil => ???
          case prop :: Nil => prop.emit
          case prop :: rest =>
            val restResult = c.freshName(TermName("restResult"))
            val restFailures = c.freshName(TermName("restFailures"))
            q"""
                val $failures = ${prop.emit}
                if ($failures.isEmpty) _root_.scala.collection.immutable.Nil
                else {
                  val $restFailures = ${loop(rest)}
                  if ($restFailures.isEmpty) _root_.scala.collection.immutable.Nil
                  else $failures ++ $restFailures
                }
              """
        }
        loop(props.toList)
      }
    }
    case class Eq(atom1: Atom, atom2: Atom) extends Prop with Simple {
      override def tree = q"${atom1.tree} == ${atom2.tree}"
      override def diagnostic = showCode(atom1.tree) + " is not equal to " + showCode(atom2.tree)
    }
    case class Ne(atom1: Atom, atom2: Atom) extends Prop with Simple {
      override def tree = q"${atom1.tree} != ${atom2.tree}"
      override def diagnostic = showCode(atom1.tree) + " is equal to " + showCode(atom2.tree)
    }
    case class Forall(list: Atom, fn: Atom) extends Prop {
      override def emit = Atom(q"${list.tree}.forall(${fn.tree})").emit
    }
    case class Exists(list: Atom, fn: Atom) extends Prop {
      override def emit = Atom(q"${list.tree}.exists(${fn.tree})").emit
    }
    case class Imply(atom1: Atom, atom2: Atom) extends Prop with Simple {
      override def tree = q"!${atom1.tree} || ${atom2.tree}"
      override def diagnostic = showCode(atom1.tree) + " is true, but " + showCode(atom2.tree) +
        " is false"
    }

    def propify(tree: Tree): Prop = tree match {
      case q"$_.debug(..$_)" => Debug()
      case q"!$x" => Not(propify(x))
      case q"$x && $y" => And(propify(x), propify(y))
      case q"$x || $y" => Or(propify(x), propify(y))
      case q"$x == $y" => Eq(Atom(x), Atom(y))
      case q"$x != $y" => Ne(Atom(x), Atom(y))
      case q"$x.forall($y)" => Forall(Atom(x), Atom(y))
      case q"$x.exists($y)" => Exists(Atom(x), Atom(y))
      case q"$_.Implication($x).==>($y)" => Imply(Atom(x), Atom(y))
      case q"$_.Implication($x).<==($y)" => Imply(Atom(y), Atom(x))
      case q"$_.Implication($x).<==>($y)" => And(Imply(Atom(x), Atom(y)), Imply(Atom(y), Atom(x)))
      case x => Atom(x)
    }

    def negate(fn: Tree): Tree = fn match {
      case Function(p :: Nil, body) => q"($p => !$body)"
      case fn => q"(x => !$fn(x))"
    }

    def simplify(prop: Prop): Prop = prop match {
      case Not(Atom(tree)) => Not(Atom(tree))
      case Not(Not(prop)) => simplify(prop)
      case Not(And(props @ _*)) => simplify(Or(props.map(prop => Not(prop)): _*))
      case Not(Or(props @ _*)) => simplify(And(props.map(prop => Not(prop)): _*))
      case Not(Eq(atom1, atom2)) => simplify(Ne(atom1, atom2))
      case Not(Ne(atom1, atom2)) => simplify(Eq(atom1, atom2))
      case Not(Forall(list, Atom(fn))) => simplify(Exists(list, Atom(negate(fn))))
      case Not(Exists(list, Atom(fn))) => simplify(Forall(list, Atom(negate(fn))))
      case And(props @ _*) if props.exists(_.isInstanceOf[Debug]) =>
        simplify(And(props.filter(!_.isInstanceOf[Debug]): _*))
      case And(props @ _*) if props.exists(_.isInstanceOf[And]) =>
        val i = props.indexWhere(_.isInstanceOf[And])
        simplify(And(props.take(i) ++ props(i).asInstanceOf[And].props ++ props.drop(i + 1): _*))
      case And(props @ _*) => And(props.map(simplify): _*)
      case Or(props @ _*) if props.exists(_.isInstanceOf[Debug]) => Debug()
      case Or(props @ _*) if props.exists(_.isInstanceOf[Or]) =>
        val i = props.indexWhere(_.isInstanceOf[Or])
        simplify(Or(props.take(i) ++ props(i).asInstanceOf[Or].props ++ props.drop(i + 1): _*))
      case Or(props @ _*) => Or(props.map(simplify): _*)
      case prop => prop
    }

    val prop = simplify(propify(tree))
    // println(tree)
    // println(prop)
    // println(prop.emit)
    // println("=====")
    c.untypecheck(prop.emit)
  }
}

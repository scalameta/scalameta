package org.scalareflect

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object invariants {
  def require[T](x: T): Unit = macro Macros.require
  // TODO: add pretty printed support for implication
  implicit class Implication(left: Boolean) {
    def ==>(right: Boolean) = !left || right
  }
}

package invariants {
  class Macros(val c: Context) {
    import c.universe._
    def require(x: Tree): Tree = {
      sealed abstract class Prop {
        lazy val result = c.freshName(TermName("result"))
        lazy val failures = c.freshName(TermName("failures"))
        lazy val temp = c.freshName(TermName("temp"))
        def emit: Tree
      }
      trait Simple extends Prop {
        def diagnostic: String
        def tree: Tree
        override def emit = {
          q"""
            val $result = $tree
            if ($result) (true, Nil)
            else (false, List($diagnostic))
          """
        }
      }
      case class Atom(tree: Tree) extends Prop with Simple {
        override def diagnostic = showCode(tree) + " is false"
      }
      case class Not(prop: Prop) extends Prop with Simple {
        override def tree = prop.asInstanceOf[Atom].tree
        override def diagnostic = showCode(tree) + " is true"
      }
      case class And(props: Prop*) extends Prop {
        override def emit = {
          def loop(props: List[Prop]): Tree = props.toList match {
            case Nil =>
              ???
            case prop :: Nil =>
              prop.emit
            case prop :: rest =>
              q"""
                val ($result, $failures) = ${prop.emit}
                if (!$result) (false, $failures)
                else ${loop(rest)}
              """
          }
          loop(props.toList)
        }
      }
      case class Or(props: Prop*) extends Prop {
        override def emit = {
          def loop(props: List[Prop]): Tree = props.toList match {
            case Nil =>
              ???
            case prop :: Nil =>
              prop.emit
            case prop :: rest =>
              val restResult = c.freshName(TermName("restResult"))
              val restFailures = c.freshName(TermName("restFailures"))
              q"""
                val ($result, $failures) = ${prop.emit}
                if ($result) (true, Nil)
                else {
                  val ($restResult, $restFailures) = ${loop(rest)}
                  if ($restResult) (true, Nil)
                  else (false, $failures ++ $restFailures)
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

      def propify(tree: Tree): Prop = tree match {
        case q"!$x" => Not(propify(x))
        case q"$x && $y" => And(propify(x), propify(y))
        case q"$x || $y" => Or(propify(x), propify(y))
        case q"$x == $y" => Eq(Atom(x), Atom(y))
        case q"$x != $y" => Ne(Atom(x), Atom(y))
        case q"$x.forall($y)" => Forall(Atom(x), Atom(y))
        case q"$x.exists($y)" => Exists(Atom(x), Atom(y))
        case x => Atom(x)
      }

      def negate(fn: Tree): Tree = fn match {
        case Function(p :: Nil, body) => q"($p => !$body)"
        case fn => q"(x => !$fn(x))"
      }

      def simplify(prop: Prop): Prop = prop match {
        case Not(Atom(tree)) =>
          Not(Atom(tree))
        case Not(Not(prop)) =>
          simplify(prop)
        case Not(And(props @ _*)) =>
          simplify(Or(props.map(prop => Not(prop)): _*))
        case Not(Or(props @ _*)) =>
          simplify(And(props.map(prop => Not(prop)): _*))
        case Not(Eq(atom1, atom2)) =>
          simplify(Ne(atom1, atom2))
        case Not(Ne(atom1, atom2)) =>
          simplify(Eq(atom1, atom2))
        case Not(Forall(list, Atom(fn))) =>
          simplify(Exists(list, Atom(negate(fn))))
        case Not(Exists(list, Atom(fn))) =>
          simplify(Forall(list, Atom(negate(fn))))
        case And(props @ _*) if props.exists(_.isInstanceOf[And]) =>
          val i = props.indexWhere(_.isInstanceOf[And])
          simplify(And(props.take(i) ++ props(i).asInstanceOf[And].props ++ props.drop(i + 1): _*))
        case And(props @ _*) =>
          And(props.map(simplify): _*)
        case Or(props @ _*) if props.exists(_.isInstanceOf[Or]) =>
          val i = props.indexWhere(_.isInstanceOf[Or])
          simplify(Or(props.take(i) ++ props(i).asInstanceOf[Or].props ++ props.drop(i + 1): _*))
        case Or(props @ _*) =>
          Or(props.map(simplify): _*)
        case prop =>
          prop
      }

      val prop = simplify(propify(x))
      // println(x)
      // println(prop)
      // println(prop.emit)
      // println("=======")

      val failures = c.freshName(TermName("failures"))
      def enclosingClass = {
        def loop(sym: Symbol): Symbol = {
          if (sym.isClass) sym
          else loop(sym.owner)
        }
        loop(c.internal.enclosingOwner)
      }
      q"""
        ${c.untypecheck(prop.emit)} match {
          case (true, _) => ()
          case (false, $failures) => org.scalareflect.invariants.InvariantFailedException.raise(${showCode(x)}, $failures, scala.Some($enclosingClass.this))
        }
      """
    }
  }
}

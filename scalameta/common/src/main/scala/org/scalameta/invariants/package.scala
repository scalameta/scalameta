package org.scalameta

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.{ClassTag, classTag}
import org.scalameta.internal.MacroHelpers

package object invariants {
  // This macro behaves like `Predef.require` with an additional twist
  // of taking apart `requirement` and generating out an informative exception message
  // if things does wrong.
  //
  // Things that end up on an error message:
  // 1) Values of local variables.
  // 2) Calls to `org.scalameta.debug` as explained in documentation to that method.
  def require(requirement: Boolean): Unit = macro Macros.require

  implicit class XtensionRequireCast[T](x: T) {
    // Equivalent to requiring that `x.getClass` is assignable from `U`.
    // Implemented as a macro, because there's no other way to delegate to another macro.
    def require[U: ClassTag]: U = macro Macros.requireCast[U]
  }

  // Provides pretty notation for implications of different kinds.
  // This is surprisingly helpful when writing certain complex `require` calls.
  implicit class XtensionImplication(left: Boolean) {
    def ==>(right: Boolean) = !left || right
    def <==(right: Boolean) = (right ==> left)
    def <==>(right: Boolean) = (left ==> right) && (right ==> left)
  }
}

package invariants {
  class Macros(val c: Context) extends MacroHelpers {
    import c.universe._

    def require(requirement: Tree): Tree = {
      val failures = c.freshName(TermName("failures"))
      val allDebuggees = freeLocals(requirement) ++ debuggees(requirement)
      q"""
        ${instrument(requirement)} match {
          case (true, _) => ()
          case (false, $failures) => $InvariantFailedRaiseMethod(${showCode(requirement)}, $failures, $allDebuggees)
        }
      """
    }

    def requireCast[U](ev: c.Tree)(U: c.WeakTypeTag[U]): c.Tree = {
      val q"$_($x)" = c.prefix.tree
      q"""
        val temp = ${c.untypecheck(x)}
        val tempClass = if (temp != null) temp.getClass else null
        $InvariantsRequireMethod(tempClass != null && _root_.scala.reflect.classTag[$U].unapply(temp).isDefined)
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
        override def emit = {
          q"""
            val $result = $tree
            if ($result) (true, _root_.scala.collection.immutable.Nil)
            else (false, _root_.scala.collection.immutable.List($diagnostic))
          """
        }
      }
      case class Debug() extends Prop {
        override def emit = q"(true, _root_.scala.collection.immutable.Nil)"
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
                if ($result) (true, _root_.scala.collection.immutable.Nil)
                else {
                  val ($restResult, $restFailures) = ${loop(rest)}
                  if ($restResult) (true, _root_.scala.collection.immutable.Nil)
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
      case class Imply(atom1: Atom, atom2: Atom) extends Prop with Simple {
        override def tree = q"!${atom1.tree} || ${atom2.tree}"
        override def diagnostic = showCode(atom1.tree) + " is true, but " + showCode(atom2.tree) + " is false"
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
        case And(props @ _*) if props.exists(_.isInstanceOf[Debug]) =>
          simplify(And(props.filter(!_.isInstanceOf[Debug]): _*))
        case And(props @ _*) if props.exists(_.isInstanceOf[And]) =>
          val i = props.indexWhere(_.isInstanceOf[And])
          simplify(And(props.take(i) ++ props(i).asInstanceOf[And].props ++ props.drop(i + 1): _*))
        case And(props @ _*) =>
          And(props.map(simplify): _*)
        case Or(props @ _*) if props.exists(_.isInstanceOf[Debug]) =>
          Debug()
        case Or(props @ _*) if props.exists(_.isInstanceOf[Or]) =>
          val i = props.indexWhere(_.isInstanceOf[Or])
          simplify(Or(props.take(i) ++ props(i).asInstanceOf[Or].props ++ props.drop(i + 1): _*))
        case Or(props @ _*) =>
          Or(props.map(simplify): _*)
        case prop =>
          prop
      }

      val prop = simplify(propify(tree))
      // println(tree)
      // println(prop)
      // println(prop.emit)
      // println("=======")
      c.untypecheck(prop.emit)
    }
  }
}

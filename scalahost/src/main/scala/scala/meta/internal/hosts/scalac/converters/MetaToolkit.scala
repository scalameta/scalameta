package scala.meta.internal.hosts.scalac
package converters

import org.scalameta.unreachable
import org.scalameta.invariants._
import scala.collection.mutable
import scala.{meta => mapi}
import scala.meta.internal.{ast => impl}
import scala.meta.internal.ast._
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.semantic._
import scala.meta.{SemanticException, Semantics}
import scala.meta.internal.prettyprinters._

trait MetaToolkit {
  implicit class RichMetaToolkitDenotation(denot: Denotation) {
    def requireSymbol: Symbol = denot.symbols match {
      case Nil => throw new Exception(s"no symbols in denotation $denot")
      case List(symbol) => symbol
      case List(head, rest @ _*) => throw new Exception(s"multiple symbols in denotation $denot: ${denot.symbols}")
    }
  }

  implicit class RichMetaToolkitStat(stat: mapi.Stat) {
    def binders: Seq[Name] = stat match {
      case tree: Decl.Val => tree.pats.map(_.name)
      case tree: Decl.Var => tree.pats.map(_.name)
      case tree: Decl.Def => List(tree.name)
      case tree: Decl.Type => List(tree.name)
      case tree: Defn.Val => ???
      case tree: Defn.Var => ???
      case tree: Defn.Def => List(tree.name)
      case tree: Defn.Macro => List(tree.name)
      case tree: Defn.Type => List(tree.name)
      case tree: Defn.Class => List(tree.name)
      case tree: Defn.Trait => List(tree.name)
      case tree: Defn.Object => List(tree.name)
      case       Pkg(name: Term.Name, _) => List(name)
      case       Pkg(Term.Select(_, name: Term.Name), _) => List(name)
      case tree: Pkg.Object => List(tree.name)
      case tree: Ctor.Secondary => List(tree.name)
      case _ => Nil
    }
    def member: Member = stat match {
      case Decl.Val(_, List(member: Pat.Var.Term), _) => member
      case Decl.Var(_, List(member: Pat.Var.Term), _) => member
      case Defn.Val(_, List(member: Pat.Var.Term), _, _) => member
      case Defn.Var(_, List(member: Pat.Var.Term), _, _) => member
      case tree: Member => tree
      case _ => sys.error(s"unsupported stat ${stat.productPrefix}: $stat")
    }
  }

  implicit class RichMetaToolkitMember(member: mapi.Member) {
    def stat: Stat = member match {
      case tree: Pat.Var.Term => tree.firstNonPatParent.get.require[Stat]
      case stat: Stat => stat
      case _ => sys.error(s"unsupported member ${member.productPrefix}: $member")
    }
  }

  implicit class RichForceTree[T <: mapi.Tree](tree: T) {
    def forceTypechecked: T = {
      try {
        def traverse(tree: Tree): Unit = {
          def process(field: Any): Unit = field match {
            case x: Tree => traverse(x)
            case Seq(xs @ _*) => xs.foreach(x => process(x))
            case Some(x) => process(x)
            case x => // do nothing
          }
          if (tree.isTypechecked) return
          val _ = tree.setTypechecked
          tree.productIterator.foreach(process)
        }
        traverse(tree.asInstanceOf[Tree])
        tree.setTypechecked.asInstanceOf[T]
      } catch {
        case ex: UnsupportedOperationException =>
          throw new UnsupportedOperationException("failed to force TYPECHECKED for " + tree.show[Attributes], ex)
      }
    }
  }
}
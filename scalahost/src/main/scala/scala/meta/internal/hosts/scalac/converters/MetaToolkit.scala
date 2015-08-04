package scala.meta.internal.hosts.scalac
package converters

import org.scalameta.unreachable
import org.scalameta.invariants._
import scala.collection.mutable
import scala.{meta => mapi}
import scala.meta.internal.{ast => impl}
import scala.meta.internal.ast._
import scala.meta.internal.semantic._
import scala.meta.{SemanticException, Semantics}

trait MetaToolkit {
  implicit class RichMetaToolkitTree(tree: mapi.Tree) {
    def requireDenoted(): Unit = {
      val offenders = mutable.ListBuffer[(Tree, List[String])]()
      def traverse(tree: Tree, path: List[String]): Unit = {
        def check(tree: Tree): Boolean = tree match {
          case tree: Name =>
            tree.denot match {
              case Denotation.Single(Prefix.Zero, _) =>
                true
              case Denotation.Single(Prefix.Type(prefix: impl.Tree), _) =>
                traverse(prefix, path :+ "Denotation")
                true
              case Denotation.Multi(Prefix.Zero, _) =>
                true
              case Denotation.Multi(Prefix.Type(prefix: impl.Tree), _) =>
                traverse(prefix, path :+ "Denotation")
                true
              case _ =>
                false
            }
          case _ =>
            true
        }
        def loop(x: Any): Unit = x match {
          case x: Tree => traverse(x, path :+ tree.productPrefix)
          case x: List[_] => x.foreach(loop)
          case x: Some[_] => loop(x.get)
          case x => // do nothing
        }
        if (!check(tree)) offenders += ((tree, path))
        tree.productIterator.toList.map(loop)
      }
      traverse(tree.require[impl.Tree], Nil)
      if (offenders.nonEmpty) {
        val grouped = offenders.groupBy(_._1.productPrefix).toList.sortBy(_._1)
        def commaCommaAnd[T](list: List[T]): String = list.init.mkString(", ") + (if (list.length == 1) "" else " and ") + list.last
        // The problem is caused by 6 Idents that are either unattributed or erroneous:
        val offenderSummary = commaCommaAnd(grouped.map{ case (k, v) => s"${v.length} $k${if (v.length == 1) "" else "s"}" })
        // 1: _ (... CaseDef > Bind > UnApply > Typed > Ident)
        // 2: _ (...  DefDef > Match > CaseDef > Bind > Ident)
        // ...
        val offenderPrintout = grouped.flatMap(_._2).map({ case (tree, path) =>
          var prefix = tree.toString.replace("\n", " ")
          if (prefix.length > 60) prefix = prefix.take(60) + "..."
          val s_path = path.mkString(" > ")
          var suffix = if (s_path.length <= 40) s_path else "... " + s_path.takeRight(40)
          if (suffix != "") suffix = s" ($suffix)"
          s"$prefix$suffix"
        }).zipWithIndex.map{ case (s, i) => s"${i + 1}: $s" }.mkString("\n")
        throw new SemanticException(s"""
          |Input scala.meta tree is not fully attributed and can't be converted to a scala.reflect artifact.
          |The problem is caused by $offenderSummary that ${if (offenders.length == 1) "doesn't" else "don't"} have denotations:
          |$offenderPrintout
          |The input tree that has caused problems to the converter is printed out below:
          |(Note that show[Semantics] output at the end of the printout is supposed to contain denotations)
          |$tree
          |${tree.show[Semantics]}
        """.stripMargin)
      }
    }
    def requireTyped(): mapi.Type.Arg = {
      def requireTyped(typing: Typing) = typing match {
        case Typing.Zero => throw new SemanticException(s"implementation restriction: internal cache has no type associated with $tree")
        case Typing.Specified(tpe) => tpe
      }
      tree match {
        case tree: Term => requireTyped(tree.typing)
        case tree: Term.Param => requireTyped(tree.typing)
        case _ => unreachable(debug(tree))
      }
    }
  }

  implicit class RichMetaToolkitDenotation(denot: Denotation) {
    def requireSymbol: Symbol = denot.symbols match {
      case Nil => throw new Exception(s"no symbols in denotation $denot")
      case List(symbol) => symbol
      case List(head, rest @ _*) => throw new Exception(s"multiple symbols in denotation $denot: ${denot.symbols}")
    }
  }

  implicit class RichMetaToolkitStat(stat: Stat) {
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

  implicit class RichMetaToolkitMember(member: Member) {
    private def firstNonPatParent(pat: Pat): Option[Tree] = {
      pat.parent.collect{case pat: Pat => pat}.flatMap(firstNonPatParent).orElse(pat.parent.map(_.require[Tree]))
    }
    def stat: Stat = member match {
      case tree: Pat.Var.Term => firstNonPatParent(tree).get.require[Stat]
      case stat: Stat => stat
      case _ => sys.error(s"unsupported member ${member.productPrefix}: $member")
    }
  }
}
package scala.meta.internal.hosts.scalac
package reflect

import scala.tools.nsc.Global
import scala.reflect.macros.Attachments
import scala.reflect.internal.Flags
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

trait TreeHelpers {
  self: ReflectToolkit =>

  import global.{require => _, _}
  import definitions._
  import treeInfo._
  import build._
  import analyzer._

  implicit class RichFoundationSymbol(sym: Symbol) {
    def displayName: String = Ident(sym).displayName
  }

  implicit class RichFoundationNameTree(tree: Tree) {
    // NOTE: scala.reflect's tree don't have parent links, so we have to approximate if we encounter an unattributed package object
    def displayName: String = {
      def packageName(tree: ModuleDef): Name = {
        if (tree.symbol != NoSymbol) tree.symbol.owner.name
        else tree.parent match {
          case _: PackageDef => TermName(tree.parent.displayName)
          case _ => TermName("package")
        }
      }
      tree match {
        case tree: ModuleDef if tree.name == nme.PACKAGE => packageName(tree).displayName
        case tree: NameTree => tree.name.displayName
        case This(name) => name.displayName
        case Super(_, name) => name.displayName
        case _ => unreachable(debug(tree, showRaw(tree)))
      }
    }
  }

  implicit class RichFoundationHelperName(name: Name) {
    def isAnonymous = {
      val isTermPlaceholder = name.isTermName && name.startsWith(nme.FRESH_TERM_NAME_PREFIX)
      val isTypePlaceholder = name.isTypeName && name.startsWith("_$")
      val isAnonymousSelf = name.isTermName && (name.startsWith(nme.FRESH_TERM_NAME_PREFIX) || name == nme.this_)
      val isAnonymousTypeParameter = name == tpnme.WILDCARD
      isTermPlaceholder || isTypePlaceholder || isAnonymousSelf || isAnonymousTypeParameter
    }
    def looksLikeInfix = {
      val hasSymbolicName = !name.decoded.forall(c => Character.isLetter(c) || Character.isDigit(c) || c == '_')
      val idiomaticallyUsedAsInfix = name == nme.eq || name == nme.ne
      hasSymbolicName || idiomaticallyUsedAsInfix
    }
    def displayName = {
      // NOTE: "<empty>", the internal name for empty package, isn't a valid Scala identifier, so we hack around
      if (name == rootMirror.EmptyPackage.name) "_empty_"
      else if (name == rootMirror.EmptyPackageClass.name) "_empty_"
      else if (name.isAnonymous) "_"
      else name.decodedName.toString
    }
  }

  case class Memento(metadata: Metadata[Tree], attachments: Attachments)

  implicit class RichFoundationOriginalTree(tree: Tree) {
    def original: Tree = {
      def desugaringOriginal: Option[Tree] = tree.metadata.get("original").map(_.asInstanceOf[Tree])
      def macroExpandee: Option[Tree] = {
        def loop(tree: g.Tree): g.Tree = {
          val maybeAttachment = tree.attachments.get[MacroExpansionAttachment]
          val maybeExpandee = maybeAttachment.map(_.expandee.asInstanceOf[Tree])
          val maybeRecursiveExpandee = maybeExpandee.map(loop)
          maybeRecursiveExpandee.getOrElse(tree)
        }
        val result = loop(tree)
        if (tree != result) Some(result)
        else None
      }
      desugaringOriginal.orElse(macroExpandee).getOrElse(tree)
    }

    def forgetOriginal: (Tree, Memento) = {
      val memento = Memento(tree.metadata, tree.attachments)
      val tree1 = { tree.metadata.remove("original"); tree }
      val tree2 = { tree1.attachments.remove[MacroExpansionAttachment]; tree1 }
      (tree2, memento)
    }

    def rememberOriginal(memento: Memento): Tree = {
      val tree05 = memento.metadata.get("original").map(original => { tree.metadata.update("original", original); tree })
      val tree1 = tree05.getOrElse(tree)
      val tree15 = memento.attachments.get[MacroExpansionAttachment].map(tree1.updateAttachment)
      val tree2 = tree15.getOrElse(tree1)
      tree2
    }
  }
}

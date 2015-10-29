package scala.meta.internal.hosts.scalac
package reflect

import org.scalameta.invariants._
import org.scalameta.adt._
import scala.reflect.macros.Attachments

trait UninferrableDesugarings {
  self: ReflectToolkit =>

  import global.{require => _, _}
  import definitions._
  import treeInfo._
  import analyzer._

  // Here we keep track of desugarings that we can't undo by correlation with the original source,
  // i.e. if we pass the original code as `sy` and the desugared code as `se` into `mergeTrees`,
  // we wouldn't be able to merge, i.e. to figure out attributes for sy's subtrees, them adequately.
  //
  // Every such desugaring requires special treatment as follows:
  //   1) scalac's typechecker must attach the original to the desugared tree.
  //   2) dotc's compilation pipeline must either: a) not perform the desugaring, or b) do #1.
  //   3) TASTY persistence must serialize the original (or leave additional hints,
  //      but that'll mean changes in the format, so we want to avoid that).
  //
  // In order to achieve #1, we use HijackAnalyzer to install our own Analyzer
  // that attaches originals as necessary. We hope that the Scala team at Typesafe
  // will later merge this logic into scala/scala, so that we don't have to
  // jump through the hoops in the long run.
  //
  // In order to achieve #2, we are constantly in touch with the Dotty team
  // (whose members are located within 10 meters of my desk :)). Dotty development process
  // is much less rigid, so we hope that will make it possible to accommodate our requests.
  //
  // In order to achieve #3, we also keep tabs on TASTY development.
  // It's very much a prototype at the moment, so we make sure that the team
  // understands our needs. At the moment, we use Java serialization to persist
  // scala.meta trees, so #3 is of no practical concern for now. Later on, though,
  // when fixing #147, we'll have to worry about #3 as well.
  @root trait UninferrableDesugaring {
    def original: Tree // An attributed(!!) tree with this particular desugaring undone
    def expansion: Tree // An attributed desugared tree, may be EmptyTree if the desugaring isn't useful
    require(original.tpe != null && (expansion.isEmpty || expansion.tpe != null))
  }

  // #1: Already supported by scalac since 2.10.x.
  // #2: Dotty doesn't have macros, so this desugaring is irrelevant.
  @leaf class MacroExpansion(original: Tree, expansion: Tree) extends UninferrableDesugaring
  object MacroExpansion {
    def read(maybeExpansion: Tree): Option[MacroExpansion] = {
      def loop(maybeExpansion: g.Tree): g.Tree = {
        val maybeAttachment = maybeExpansion.attachments.get[MacroExpansionAttachment]
        val maybeExpandee = maybeAttachment.map(_.expandee.asInstanceOf[Tree])
        val maybeRecursiveExpandee = maybeExpandee.map(loop)
        maybeRecursiveExpandee.getOrElse(maybeExpansion)
      }
      val original = loop(maybeExpansion)
      if (maybeExpansion != original) Some(MacroExpansion(original, maybeExpansion))
      else None
    }
    // NOTE: this attachment is managed in scalac's typechecker
    // def write(original: Tree, expansion: Tree): Unit = {
    //   linkExpandeeAndExpanded(original, expansion)
    // }
  }

  // #1: Established by our compiler plugin, reached a preliminary agreement with Jason.
  // #2: Dotty doesn't constfold in typer, but the subsequent phase messes things up.
  // A conversation with Dmitry and Martin has concluded with an admission that this is a bug.
  @leaf class ConstantFolding(original: Tree, expansion: Tree) extends UninferrableDesugaring
  object ConstantFolding {
    def read(maybeExpansion: Tree): Option[ConstantFolding] = {
      val untyped = UninferrableDesugaring.read(maybeExpansion, "ConstantFolding")
      untyped.map(_.asInstanceOf[ConstantFolding])
    }
    def write(original: Tree, expansion: Tree): Unit = {
      if (original == expansion) return
      UninferrableDesugaring.write(expansion, ConstantFolding(original, expansion))
    }
  }

  // NOTE: Below you can find the public API underlying UninferrableDesugarings.
  // There we have ways to read and write attachments as well as to temporarily drop them.

  object UninferrableDesugaring {
    def unapply(tree: Tree): Option[(Tree, Tree)] = {
      val desugaring = MacroExpansion.read(tree).orElse(ConstantFolding.read(tree))
      desugaring.map(d => (d.original, d.expansion))
    }

    private def storage(tree: Tree): Option[Map[String, UninferrableDesugaring]] = {
      val untyped = tree.metadata.get("uninferrableDesugarings")
      untyped.map(_.asInstanceOf[Map[String, UninferrableDesugaring]])
    }
    private[UninferrableDesugarings] def read(tree: Tree, key: String): Option[UninferrableDesugaring] = {
      storage(tree).flatMap(_.get(key))
    }
    private[UninferrableDesugarings] def write(tree: Tree, payload: UninferrableDesugaring): Unit = {
      val storage0 = this.storage(tree)
      val storage1 = storage0.getOrElse(Map()) + (payload.productPrefix -> payload)
      tree.appendMetadata("uninferrableDesugarings" -> storage1)
    }
  }

  case class Memento(metadata: Metadata[Tree], attachments: Attachments)

  implicit class RichUninferrableDesugaringsTree(tree: Tree) {
    def rememberConstantFoldingOf(original: Tree): Tree = {
      ConstantFolding.write(original, tree)
      tree
    }

    def forgetOriginal: (Tree, Memento) = {
      val memento = Memento(tree.metadata, tree.attachments)
      val tree1 = { tree.metadata.remove("uninferrableDesugarings"); tree }
      val tree2 = { tree1.attachments.remove[MacroExpansionAttachment]; tree1 }
      (tree2, memento)
    }

    def rememberOriginal(memento: Memento): Tree = {
      val tree05 = memento.metadata.get("uninferrableDesugarings").map(original => { tree.metadata.update("uninferrableDesugarings", original); tree })
      val tree1 = tree05.getOrElse(tree)
      val tree15 = memento.attachments.get[MacroExpansionAttachment].map(tree1.updateAttachment)
      val tree2 = tree15.getOrElse(tree1)
      tree2
    }
  }
}
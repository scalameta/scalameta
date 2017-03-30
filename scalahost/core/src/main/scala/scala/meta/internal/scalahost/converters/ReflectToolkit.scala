package scala.meta
package internal
package scalahost
package converters

import scala.collection.mutable
import scala.tools.nsc.Global

trait ReflectToolkit {
  val global: Global
  lazy val g: global.type = global

  import global._
  import definitions._

  implicit class XtensionName(name: g.Name) {
    def isAnonymous = {
      val isTermPlaceholder = name.isTermName && name.startsWith(nme.FRESH_TERM_NAME_PREFIX)
      val isTypePlaceholder = name.isTypeName && name.startsWith("_$")
      val isAnonymousSelf = name.isTermName && (name.startsWith(nme.FRESH_TERM_NAME_PREFIX) || name == nme.this_)
      val isAnonymousTypeParameter = name == tpnme.WILDCARD
      isTermPlaceholder || isTypePlaceholder || isAnonymousSelf || isAnonymousTypeParameter
    }
    def looksLikeInfix = {
      val hasSymbolicName =
        !name.decoded.forall(c => Character.isLetter(c) || Character.isDigit(c) || c == '_')
      val idiomaticallyUsedAsInfix = name == nme.eq || name == nme.ne
      val idiomaticallyNotUsedAsInfix = name == nme.CONSTRUCTOR
      (hasSymbolicName || idiomaticallyUsedAsInfix) && !idiomaticallyNotUsedAsInfix
    }
    def isRightAssoc = {
      name.decoded.endsWith(":")
    }
    def displayName = {
      // NOTE: "<empty>", the internal name for empty package, isn't a valid Scala identifier, so we hack around
      if (name == null || name == rootMirror.EmptyPackage.name || name == rootMirror.EmptyPackageClass.name)
        "_empty_"
      // TODO: why did we need this in the past?
      // else if (name.isAnonymous) "_"
      else name.decodedName.toString
    }
  }

  implicit class XtensionClassDef(tree: ClassDef) {
    def primaryCtor: DefDef = {
      tree.impl.body.collectFirst {
        case ctor @ g.DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => ctor
      }.get
    }
  }

  implicit class XtensionPat(tree: Tree) {
    def childrenPatterns: Set[Tree] = {
      val patterns = mutable.Set[g.Tree]()
      object traverser extends g.Traverser {
        override def traverse(tree: Tree): Unit = {
          tree match {
            case g.CaseDef(pat, _, _) =>
              object traverser extends g.Traverser {
                override def traverse(tree: Tree): Unit = {
                  tree match {
                    case g.Ident(nme.WILDCARD) =>
                      patterns += tree
                    case g.Ident(_) =>
                    // converted as term
                    case g.Select(_, _) =>
                    // converted as term
                    case g.Bind(_, body) =>
                      patterns += tree
                      traverse(body)
                    case g.Alternative(alts) =>
                      patterns += tree
                      alts.foreach(traverse)
                    case g.Annotated(annot, arg) =>
                      patterns += tree
                      traverse(annot)
                      traverse(arg)
                    case g.CompoundTypeTree(templ) =>
                      patterns += tree
                      traverse(templ)
                    case g.Typed(ident, tpat) =>
                      patterns += ident
                      patterns += tree
                      traverse(tpat)
                    case g.Apply(_, args) =>
                      patterns += tree
                      args.foreach(traverse)
                    case g.AppliedTypeTree(tpt, args) =>
                      patterns += tree
                      args.foreach(traverse)
                    case _ =>
                    // do nothing special
                  }
                }
              }
              traverser.traverse(pat)
            case _ =>
            // do nothing special
          }
          super.traverse(tree)
        }
      }
      traverser.traverse(tree)
      patterns.toSet
    }
  }
}

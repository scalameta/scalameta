package scala.meta
package internal.hosts.scalac
package converters

import org.scalameta.meta.{Toolkit => MetaToolkit}
import org.scalameta.reflection._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.reflect.runtime.universe.{Type => Pt}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.parsers.Helpers.{XtensionTermOps => _, _}

// This module exposes a method that can convert scala.reflect trees
// into equivalent scala.meta trees.
//
// Unlike in the previous implementation, we don't care
// about preserving syntactic details of the original code:
// we just produce scala.meta trees for everything that we see
// (desugared forms or non-desugared forms alike),
// so that the result of the conversion captures all the semantics of the original code.
//
// In order to obtain a scala.meta tree that combines syntactic and semantic precision,
// you will need to use a dedicated module called `mergeTrees`
// that is capable of merging syntactically precise trees (obtained from parsing)
// and semantically precise trees (obtain from converting).
trait ToMtree extends GlobalToolkit with MetaToolkit {
  self: Api =>

  def toMtree(gtree: g.Tree): m.Tree = toMtree(gtree, g.EmptyTree)

  private def toMtree(gtree: g.Tree, gparent: g.Tree): m.Tree = {
    val mtree = gtree match {
      case g.PackageDef(gpid, gstats) =>
        if (gpid.name == g.nme.EMPTY_PACKAGE_NAME) {
          require(gparent.isEmpty)
          m.Source(toMtrees(gstats, gtree).require[Seq[m.Stat]])
        } else {
          val mroot = gstats match {
            case List(gstat: g.ModuleDef) if gstat.name == g.nme.PACKAGE =>
              toMtree(gstat, gparent).require[m.Pkg.Object]
            case _ =>
              val mpid = toMname(gpid, gtree).require[m.Term.Ref]
              val mstats = toMtrees(gstats, gparent).require[Seq[m.Stat]]
              m.Pkg(mpid, mstats)
          }
          m.Source(List(mroot))
        }
      case _ =>
        unreachable(debug(gtree, g.showRaw(gtree)))
    }
    if (sys.props("convert.debug") != null) {
      println(gtree)
      println(mtree)
      println(g.showRaw(gtree, printIds = true))
      println(mtree.show[Semantics])
    }
    mtree
  }

  private def toMtrees(gtrees: List[g.Tree], gparent: g.Tree): Seq[m.Tree] = {
    gtrees.map(gtree => toMtree(gtree, gparent))
  }

  private def toMtreess(gtreess: List[List[g.Tree]], gparent: g.Tree): Seq[Seq[m.Tree]] = {
    gtreess.map(gtrees => toMtrees(gtrees, gparent))
  }

  private def toMname(gtree: g.NameTree, gparent: g.Tree): m.Name = {
    val sname = gtree.displayName(gparent)
    val mname = {
      if (sname == "_") m.Name.Anonymous()
      else if (gtree.name.isTermName) m.Term.Name(sname)
      else if (gtree.name.isTypeName) m.Type.Name(sname)
      else unreachable(debug(gtree, g.showRaw(gtree)))
    }
    // TODO: attach denotations here
    mname
  }
}
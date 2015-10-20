// TODO: fix the duplication wrt tokens/.../tests/.../InferSuite.scala

package scala.meta.tests
package parsers

import org.scalatest._
import java.net._
import java.io._

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.{ ast => impl }
import scala.meta.tql._

import org.scalatest.FunSuite

import scala.util.{ Try, Failure, Success }

abstract class InferSuite extends ParseSuite {

  // Force synthetic tokens for all tree nodes using TQL
  val forceInferAllTQL = topDown(transform {

    case t: impl.Term.This => t.copy()
    case t: impl.Term.Super => t.copy()
    case t: impl.Term.Select => t.copy()
    case t: impl.Term.Interpolate => t.copy()
    case t: impl.Term.Apply => t.copy()
    case t: impl.Term.ApplyType => t.copy()
    case t: impl.Term.ApplyInfix => t.copy()
    case t: impl.Term.ApplyUnary => t.copy()
    case t: impl.Term.Assign => t.copy()
    case t: impl.Term.Update => t.copy()
    case t: impl.Term.Return => t.copy()
    case t: impl.Term.Throw => t.copy()
    case t: impl.Term.Ascribe => t.copy()
    case t: impl.Term.Annotate => t.copy()
    case t: impl.Term.Tuple => t.copy()
    case t: impl.Term.Block => t.copy()
    case t: impl.Term.If => t.copy()
    case t: impl.Term.TryWithCases => t.copy()
    case t: impl.Term.TryWithTerm => t.copy()
    case t: impl.Term.Function => t.copy()
    case t: impl.Term.PartialFunction => t.copy()
    case t: impl.Term.While => t.copy()
    case t: impl.Term.Do => t.copy()
    case t: impl.Term.For => t.copy()
    case t: impl.Term.ForYield => t.copy()
    case t: impl.Term.New => t.copy()
    case t: impl.Term.Placeholder => t.copy()
    case t: impl.Term.Eta => t.copy()
    case t: impl.Term.Arg.Named => t.copy()
    case t: impl.Term.Arg.Repeated => t.copy()
    case t: impl.Term.Param => t.copy()

    case t: impl.Type.Select => t.copy()
    case t: impl.Type.Project => t.copy()
    case t: impl.Type.Singleton => t.copy()
    case t: impl.Type.Apply => t.copy()
    case t: impl.Type.ApplyInfix => t.copy()
    case t: impl.Type.Function => t.copy()
    case t: impl.Type.Tuple => t.copy()
    case t: impl.Type.Compound => t.copy()
    case t: impl.Type.Existential => t.copy()
    case t: impl.Type.Annotate => t.copy()
    case t: impl.Type.Placeholder => t.copy()
    case t: impl.Type.Arg.Repeated => t.copy()
    case t: impl.Type.Arg.ByName => t.copy()
    case t: impl.Type.Param => t.copy()

    case t: impl.Pat.Var.Term => t.copy()
    case t: impl.Pat.Wildcard => t.copy()
    case t: impl.Pat.Bind => t.copy()
    case t: impl.Pat.Alternative => t.copy()
    case t: impl.Pat.Tuple => t.copy()
    case t: impl.Pat.Extract => t.copy()
    case t: impl.Pat.ExtractInfix => t.copy()
    case t: impl.Pat.Interpolate => t.copy()
    case t: impl.Pat.Typed => t.copy()
    case t: impl.Pat.Arg.SeqWildcard => t.copy()

    case t: impl.Pat.Type.Wildcard => t.copy()
    case t: impl.Pat.Var.Type => t.copy()
    case t: impl.Pat.Type.Project => t.copy()
    case t: impl.Pat.Type.Apply => t.copy()
    case t: impl.Pat.Type.ApplyInfix => t.copy()
    case t: impl.Pat.Type.Function => t.copy()
    case t: impl.Pat.Type.Tuple => t.copy()
    case t: impl.Pat.Type.Compound => t.copy()
    case t: impl.Pat.Type.Existential => t.copy()
    case t: impl.Pat.Type.Annotate => t.copy()

    case t: impl.Lit => t.copy()

    case t: impl.Decl.Val => t.copy()
    case t: impl.Decl.Var => t.copy()
    case t: impl.Decl.Type => t.copy()
    case t: impl.Decl.Def => t.copy()

    case t: impl.Defn.Val => t.copy()
    case t: impl.Defn.Var => t.copy()
    case t: impl.Defn.Type => t.copy()
    case t: impl.Defn.Class => t.copy()
    case t: impl.Defn.Trait => t.copy()
    case t: impl.Defn.Object => t.copy()
    case t: impl.Defn.Def => t.copy()
    case t: impl.Defn.Macro => t.copy()
    case t: impl.Pkg => t.copy()
    case t: impl.Pkg.Object => t.copy()
    case t: impl.Ctor.Primary => t.copy()
    case t: impl.Ctor.Secondary => t.copy()

    case t: impl.Template => t.copy()

    case t: impl.Mod.Annot => t.copy()
    case t: impl.Mod.Private => t.copy()
    case t: impl.Mod.Protected => t.copy()
    case t: impl.Mod.Implicit => t.copy()
    case t: impl.Mod.Final => t.copy()
    case t: impl.Mod.Sealed => t.copy()
    case t: impl.Mod.Override => t.copy()
    case t: impl.Mod.Case => t.copy()
    case t: impl.Mod.Abstract => t.copy()
    case t: impl.Mod.Covariant => t.copy()
    case t: impl.Mod.Contravariant => t.copy()
    case t: impl.Mod.Lazy => t.copy()
    case t: impl.Mod.ValParam => t.copy()
    case t: impl.Mod.VarParam => t.copy()

    case t: impl.Enumerator.Val => t.copy()
    case t: impl.Enumerator.Generator => t.copy()
    case t: impl.Enumerator.Guard => t.copy()

    case t: impl.Import.Selector.Name => t.copy()
    case t: impl.Import.Selector.Rename => t.copy()
    case t: impl.Import.Selector.Unimport => t.copy()
    case t: impl.Import.Selector.Wildcard => t.copy()
    case t: impl.Import => t.copy()

    case t: impl.Case => t.copy()

    case t: impl.Source => t.copy()
  })

  def forceInferAll(t: Tree): impl.Tree = forceInferAllTQL(t).tree.get.asInstanceOf[impl.Tree]

  def printCodes(codes: String*)(errOpt: Option[Throwable]): Unit = {
    println("====================================================")
    codes foreach { c =>
      println(c)
      println("----------------------------------------------------")
    }
    errOpt.map { err => println(err.getMessage) }
    println("====================================================")
  }
}

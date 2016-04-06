// TODO: fix the duplication wrt tokens/.../tests/.../InferSuite.scala

package scala.meta.tests
package parsers

import org.scalatest._
import java.net._
import java.io._

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.tql._

import org.scalatest.FunSuite

import scala.util.{ Try, Failure, Success }

abstract class InferSuite extends ParseSuite {

  // Force synthetic tokens for all tree nodes using TQL
  val forceInferAllTQL = topDown(transform {

    case t: Term.This => t.copy()
    case t: Term.Super => t.copy()
    case t: Term.Select => t.copy()
    case t: Term.Interpolate => t.copy()
    case t: Term.Apply => t.copy()
    case t: Term.ApplyType => t.copy()
    case t: Term.ApplyInfix => t.copy()
    case t: Term.ApplyUnary => t.copy()
    case t: Term.Assign => t.copy()
    case t: Term.Update => t.copy()
    case t: Term.Return => t.copy()
    case t: Term.Throw => t.copy()
    case t: Term.Ascribe => t.copy()
    case t: Term.Annotate => t.copy()
    case t: Term.Tuple => t.copy()
    case t: Term.Block => t.copy()
    case t: Term.If => t.copy()
    case t: Term.TryWithCases => t.copy()
    case t: Term.TryWithTerm => t.copy()
    case t: Term.Function => t.copy()
    case t: Term.PartialFunction => t.copy()
    case t: Term.While => t.copy()
    case t: Term.Do => t.copy()
    case t: Term.For => t.copy()
    case t: Term.ForYield => t.copy()
    case t: Term.New => t.copy()
    case t: Term.Placeholder => t.copy()
    case t: Term.Eta => t.copy()
    case t: Term.Arg.Named => t.copy()
    case t: Term.Arg.Repeated => t.copy()
    case t: Term.Param => t.copy()

    case t: Type.Select => t.copy()
    case t: Type.Project => t.copy()
    case t: Type.Singleton => t.copy()
    case t: Type.Apply => t.copy()
    case t: Type.ApplyInfix => t.copy()
    case t: Type.Function => t.copy()
    case t: Type.Tuple => t.copy()
    case t: Type.Compound => t.copy()
    case t: Type.Existential => t.copy()
    case t: Type.Annotate => t.copy()
    case t: Type.Placeholder => t.copy()
    case t: Type.Arg.Repeated => t.copy()
    case t: Type.Arg.ByName => t.copy()
    case t: Type.Param => t.copy()

    case t: Pat.Var.Term => t.copy()
    case t: Pat.Wildcard => t.copy()
    case t: Pat.Bind => t.copy()
    case t: Pat.Alternative => t.copy()
    case t: Pat.Tuple => t.copy()
    case t: Pat.Extract => t.copy()
    case t: Pat.ExtractInfix => t.copy()
    case t: Pat.Interpolate => t.copy()
    case t: Pat.Typed => t.copy()
    case t: Pat.Arg.SeqWildcard => t.copy()

    case t: Pat.Type.Wildcard => t.copy()
    case t: Pat.Var.Type => t.copy()
    case t: Pat.Type.Project => t.copy()
    case t: Pat.Type.Apply => t.copy()
    case t: Pat.Type.ApplyInfix => t.copy()
    case t: Pat.Type.Function => t.copy()
    case t: Pat.Type.Tuple => t.copy()
    case t: Pat.Type.Compound => t.copy()
    case t: Pat.Type.Existential => t.copy()
    case t: Pat.Type.Annotate => t.copy()

    case t: Lit => t.copy()

    case t: Decl.Val => t.copy()
    case t: Decl.Var => t.copy()
    case t: Decl.Type => t.copy()
    case t: Decl.Def => t.copy()

    case t: Defn.Val => t.copy()
    case t: Defn.Var => t.copy()
    case t: Defn.Type => t.copy()
    case t: Defn.Class => t.copy()
    case t: Defn.Trait => t.copy()
    case t: Defn.Object => t.copy()
    case t: Defn.Def => t.copy()
    case t: Defn.Macro => t.copy()
    case t: Pkg => t.copy()
    case t: Pkg.Object => t.copy()
    case t: Ctor.Primary => t.copy()
    case t: Ctor.Secondary => t.copy()

    case t: Template => t.copy()

    case t: Mod.Annot => t.copy()
    case t: Mod.Private => t.copy()
    case t: Mod.Protected => t.copy()
    case t: Mod.Implicit => t.copy()
    case t: Mod.Final => t.copy()
    case t: Mod.Sealed => t.copy()
    case t: Mod.Override => t.copy()
    case t: Mod.Case => t.copy()
    case t: Mod.Abstract => t.copy()
    case t: Mod.Covariant => t.copy()
    case t: Mod.Contravariant => t.copy()
    case t: Mod.Lazy => t.copy()
    case t: Mod.ValParam => t.copy()
    case t: Mod.VarParam => t.copy()

    case t: Enumerator.Val => t.copy()
    case t: Enumerator.Generator => t.copy()
    case t: Enumerator.Guard => t.copy()

    case t: Importee.Name => t.copy()
    case t: Importee.Rename => t.copy()
    case t: Importee.Unimport => t.copy()
    case t: Importee.Wildcard => t.copy()
    case t: Importer => t.copy()
    case t: Import => t.copy()

    case t: Case => t.copy()

    case t: Source => t.copy()
  })

  def forceInferAll(t: Tree): Tree = forceInferAllTQL(t).tree.get

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

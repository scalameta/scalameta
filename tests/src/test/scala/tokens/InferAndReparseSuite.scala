import org.scalatest._
import java.net._
import java.io._

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.internal.{ast => impl}
import scala.meta.tql._

import org.scalatest.FunSuite

import scala.util.{Try, Failure, Success}

class InferAndReparseSuite extends FunSuite {
  var dir = new File(new File(System.getProperty("sbt.paths.tests.source")).getAbsolutePath)
  def isProjectRoot(dir: File) = dir != null && new File(dir.getAbsolutePath + File.separatorChar + "project" + File.separatorChar + "build.scala").exists
  while (dir != null && !isProjectRoot(dir)) dir = dir.getParentFile
  test("ProjectDir (" + dir.getAbsolutePath + ")")(assert(isProjectRoot(dir)))

  // Avoiding XML literals
  val ignoredFiles = List("build.scala")

  // Force synthetic tokens for all tree nodes using TQL
  val transformAll = topDown(transform {

      case t: impl.Term.This => t.copy() andCollect Unit
      case t: impl.Term.Super => t.copy() andCollect Unit
      case t: impl.Term.Select => t.copy() andCollect Unit
      case t: impl.Term.Interpolate => t.copy() andCollect Unit
      case t: impl.Term.Apply => t.copy() andCollect Unit
      case t: impl.Term.ApplyType => t.copy() andCollect Unit
      case t: impl.Term.ApplyInfix => t.copy() andCollect Unit
      case t: impl.Term.ApplyUnary => t.copy() andCollect Unit
      case t: impl.Term.Assign => t.copy() andCollect Unit
      case t: impl.Term.Update => t.copy() andCollect Unit
      case t: impl.Term.Return => t.copy() andCollect Unit
      case t: impl.Term.Throw => t.copy() andCollect Unit
      case t: impl.Term.Ascribe => t.copy() andCollect Unit
      case t: impl.Term.Annotate => t.copy() andCollect Unit
      case t: impl.Term.Tuple => t.copy() andCollect Unit
      case t: impl.Term.Block => t.copy() andCollect Unit
      case t: impl.Term.If => t.copy() andCollect Unit
      case t: impl.Term.TryWithCases => t.copy() andCollect Unit
      case t: impl.Term.TryWithTerm => t.copy() andCollect Unit
      case t: impl.Term.Function => t.copy() andCollect Unit
      case t: impl.Term.PartialFunction => t.copy() andCollect Unit
      case t: impl.Term.While => t.copy() andCollect Unit
      case t: impl.Term.Do => t.copy() andCollect Unit
      case t: impl.Term.For => t.copy() andCollect Unit
      case t: impl.Term.ForYield => t.copy() andCollect Unit
      case t: impl.Term.New => t.copy() andCollect Unit
      case t: impl.Term.Placeholder => t.copy() andCollect Unit
      case t: impl.Term.Eta => t.copy() andCollect Unit
      case t: impl.Term.Arg.Named => t.copy() andCollect Unit
      case t: impl.Term.Arg.Repeated => t.copy() andCollect Unit
      case t: impl.Term.Param => t.copy() andCollect Unit

      case t: impl.Type.Select => t.copy() andCollect Unit
      case t: impl.Type.Project => t.copy() andCollect Unit
      case t: impl.Type.Singleton => t.copy() andCollect Unit
      case t: impl.Type.Apply => t.copy() andCollect Unit
      case t: impl.Type.ApplyInfix => t.copy() andCollect Unit
      case t: impl.Type.Function => t.copy() andCollect Unit
      case t: impl.Type.Tuple => t.copy() andCollect Unit
      case t: impl.Type.Compound => t.copy() andCollect Unit
      case t: impl.Type.Existential => t.copy() andCollect Unit
      case t: impl.Type.Annotate => t.copy() andCollect Unit
      case t: impl.Type.Placeholder => t.copy() andCollect Unit
      case t: impl.Type.Arg.Repeated => t.copy() andCollect Unit
      case t: impl.Type.Arg.ByName => t.copy() andCollect Unit
      case t: impl.Type.Param => t.copy() andCollect Unit

      case t: impl.Pat.Var.Term => t.copy() andCollect Unit
      case t: impl.Pat.Wildcard => t.copy() andCollect Unit
      case t: impl.Pat.Bind => t.copy() andCollect Unit
      case t: impl.Pat.Alternative => t.copy() andCollect Unit
      case t: impl.Pat.Tuple => t.copy() andCollect Unit
      case t: impl.Pat.Extract => t.copy() andCollect Unit
      case t: impl.Pat.ExtractInfix => t.copy() andCollect Unit
      case t: impl.Pat.Interpolate => t.copy() andCollect Unit
      case t: impl.Pat.Typed => t.copy() andCollect Unit
      case t: impl.Pat.Arg.SeqWildcard => t.copy() andCollect Unit

      case t: impl.Pat.Type.Wildcard => t.copy() andCollect Unit
      case t: impl.Pat.Var.Type => t.copy() andCollect Unit
      case t: impl.Pat.Type.Project => t.copy() andCollect Unit
      case t: impl.Pat.Type.Apply => t.copy() andCollect Unit
      case t: impl.Pat.Type.ApplyInfix => t.copy() andCollect Unit
      case t: impl.Pat.Type.Function => t.copy() andCollect Unit
      case t: impl.Pat.Type.Tuple => t.copy() andCollect Unit
      case t: impl.Pat.Type.Compound => t.copy() andCollect Unit
      case t: impl.Pat.Type.Existential => t.copy() andCollect Unit
      case t: impl.Pat.Type.Annotate => t.copy() andCollect Unit

      case t: impl.Lit.Bool => t.copy() andCollect Unit
      case t: impl.Lit.Byte => t.copy() andCollect Unit
      case t: impl.Lit.Short => t.copy() andCollect Unit
      case t: impl.Lit.Int => t.copy() andCollect Unit
      case t: impl.Lit.Long => t.copy() andCollect Unit
      case t: impl.Lit.Float => t.copy() andCollect Unit
      case t: impl.Lit.Double => t.copy() andCollect Unit
      case t: impl.Lit.Char => t.copy() andCollect Unit
      case t: impl.Lit.String => t.copy() andCollect Unit
      case t: impl.Lit.Symbol => t.copy() andCollect Unit
      case t: impl.Lit.Null => t.copy() andCollect Unit
      case t: impl.Lit.Unit => t andCollect Unit

      case t: impl.Decl.Val => t.copy() andCollect Unit      
      case t: impl.Decl.Var => t.copy() andCollect Unit      
      case t: impl.Decl.Type => t.copy() andCollect Unit      
      case t: impl.Decl.Def => t.copy() andCollect Unit      

      case t: impl.Defn.Val => t.copy() andCollect Unit      
      case t: impl.Defn.Var => t.copy() andCollect Unit      
      case t: impl.Defn.Type => t.copy() andCollect Unit
      case t: impl.Defn.Class => t.copy() andCollect Unit 
      case t: impl.Defn.Trait => t.copy() andCollect Unit
      case t: impl.Defn.Object => t.copy() andCollect Unit
      case t: impl.Defn.Def => t.copy() andCollect Unit
      case t: impl.Defn.Macro => t.copy() andCollect Unit
      case t: impl.Pkg => t.copy() andCollect Unit
      case t: impl.Pkg.Object => t.copy() andCollect Unit
      case t: impl.Ctor.Primary => t.copy() andCollect Unit
      case t: impl.Ctor.Secondary => t.copy() andCollect Unit

      case t: impl.Template => t.copy() andCollect Unit

      case t: impl.Mod.Annot => t.copy() andCollect Unit
      case t: impl.Mod.Private => t.copy() andCollect Unit
      case t: impl.Mod.Protected => t.copy() andCollect Unit
      case t: impl.Mod.Implicit => t.copy() andCollect Unit
      case t: impl.Mod.Final => t.copy() andCollect Unit
      case t: impl.Mod.Sealed => t.copy() andCollect Unit
      case t: impl.Mod.Override => t.copy() andCollect Unit
      case t: impl.Mod.Case => t.copy() andCollect Unit
      case t: impl.Mod.Abstract => t.copy() andCollect Unit
      case t: impl.Mod.Covariant => t.copy() andCollect Unit
      case t: impl.Mod.Contravariant => t.copy() andCollect Unit
      case t: impl.Mod.Lazy => t.copy() andCollect Unit
      case t: impl.Mod.ValParam => t.copy() andCollect Unit
      case t: impl.Mod.VarParam => t.copy() andCollect Unit

      case t: impl.Enumerator.Val => t.copy() andCollect Unit
      case t: impl.Enumerator.Generator => t.copy() andCollect Unit
      case t: impl.Enumerator.Guard => t.copy() andCollect Unit

      case t: impl.Import.Selector.Name => t.copy() andCollect Unit
      case t: impl.Import.Selector.Rename => t.copy() andCollect Unit
      case t: impl.Import.Selector.Unimport => t.copy() andCollect Unit
      case t: impl.Import.Selector.Wildcard => t.copy() andCollect Unit
      case t: impl.Import => t.copy() andCollect Unit

      case t: impl.Case => t.copy() andCollect Unit

      case t: impl.Source => t.copy() andCollect Unit
  })

  def loop(dir: File): Unit = {
    def linePositionTest(src: File): Unit = {
      test("Testing synthetic tokens for file " + src.getAbsolutePath) {
        assert(src.exists)
        val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
        val content = scala.io.Source.fromFile(src)(codec).mkString
        val parsed = src.parse[Source]
        val transformed = transformAll(parsed)
        val newCode = transformed.tree.get.tokens.map(_.show[Code]).mkString
        Try(newCode.parse[Source]) match {
          case Success(_) => /* cool */
          case Failure(err) =>
            println("====================================================")
            println(content)
            println("----------------------------------------------------")
            println(newCode)
            println("----------------------------------------------------")
            println(err.getMessage)
            println("====================================================")
            fail
        }
      }
    }
    dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).filter(f => !ignoredFiles.contains(f.getName)).map(linePositionTest(_))
    dir.listFiles.filter(_.isDirectory).map(loop(_))
  }
  loop(dir)
}

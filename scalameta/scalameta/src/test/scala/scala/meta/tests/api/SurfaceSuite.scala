package scala.meta.tests
package api

import org.scalatest._
import org.scalameta.tests._
import org.scalameta.explore._
import scala.compat.Platform.EOL

// TODO: Implement datamining and checking of the entire API surface,
// i.e. not only check names of classes/objects, but also their members.
// See the unimplemented publicSurface macro for more details.

class SurfaceSuite extends scala.meta.tests.ast.AstSuite {
  import AstReflection._
  val reflectedTrees = {
    val root = symbolOf[scala.meta.Tree].asRoot
    val all = List(root) ++ root.allBranches ++ root.allLeafs
    all.map(_.sym.fullName).toSet
  }

  val tlds = publicToplevelDefinitions("scala.meta").toSet
  val exts = publicExtensionMethods("scala.meta")
  val trees = tlds.filter(s => s != "scala.meta.Tree" && reflectedTrees(s.stripSuffix(".Api")))
  val tokens = tlds.filter(_.startsWith("scala.meta.tokens.Token."))
  val core = tlds -- trees -- tokens

  test("public top-level definitions (core)") {
    // println(core.toList.sorted.mkString(EOL))
    assert(core.toList.sorted.mkString(EOL) === """
      |scala.meta.Dialect
      |scala.meta.InputWithDialect
      |scala.meta.Tree
      |scala.meta.classifiers
      |scala.meta.convert
      |scala.meta.dialects
      |scala.meta.inputs
      |scala.meta.inputs.Content
      |scala.meta.inputs.Input
      |scala.meta.inputs.Input.File
      |scala.meta.inputs.Input.Slice
      |scala.meta.inputs.Input.Stream
      |scala.meta.inputs.Input.String
      |scala.meta.inputs.Point
      |scala.meta.inputs.Point.Offset
      |scala.meta.inputs.Position
      |scala.meta.inputs.Position.Range
      |scala.meta.internal
      |scala.meta.package
      |scala.meta.parsers
      |scala.meta.parsers.ParseException
      |scala.meta.parsers.Parsed
      |scala.meta.parsers.Parsed.Error
      |scala.meta.parsers.Parsed.Success
      |scala.meta.prettyprinters
      |scala.meta.prettyprinters.Structure
      |scala.meta.prettyprinters.Syntax
      |scala.meta.quasiquotes
      |scala.meta.quasiquotes.Lift
      |scala.meta.quasiquotes.Unlift
      |scala.meta.tests
      |scala.meta.tokenizers
      |scala.meta.tokenizers.TokenizeException
      |scala.meta.tokenizers.Tokenized
      |scala.meta.tokenizers.Tokenized.Error
      |scala.meta.tokenizers.Tokenized.Success
      |scala.meta.tokens
      |scala.meta.tokens.Token
      |scala.meta.tokens.Tokens
      |scala.meta.tokens.Tokens.Projection
      |scala.meta.transversers
      |scala.meta.transversers.Transformer
      |scala.meta.transversers.Traverser
    """.trim.stripMargin)

    val prettyprinterTests = new scala.meta.tests.prettyprinters.PublicSuite().testNames
    val nonPackages = core.filter(_.exists(_.isUpper))
    nonPackages.foreach(name => {
      val isTested = prettyprinterTests.exists(testName => testName.startsWith(name))
      assert(isTested, s"$name prettyprinting is not tested")
    })
  }

  test("public extension methods") {
    // println(exts.sorted.mkString(EOL))
    assert(exts.sorted.mkString(EOL) === """
      |Seq[scala.meta.tokens.Token].toTokens
      |T(implicit scala.meta.classifiers.Classifiable[T]).is(implicit scala.meta.classifiers.Classifier[T,U])
      |T(implicit scala.meta.classifiers.Classifiable[T]).isNot(implicit scala.meta.classifiers.Classifier[T,U])
      |T(implicit scala.meta.prettyprinters.Structure[T]).structure
      |T(implicit scala.meta.prettyprinters.Syntax[T]).syntax
      |T.parse(implicit scala.meta.convert.Convert[T,scala.meta.inputs.Input], scala.meta.parsers.Parse[U], scala.meta.Dialect)
      |T.resetAllTokens
      |T.resetTokens
      |T.show(implicit Style[T])
      |T.tokenize(implicit scala.meta.convert.Convert[T,scala.meta.inputs.Input], scala.meta.tokenizers.Tokenize, scala.meta.Dialect)
      |scala.meta.Dialect.apply(T)(implicit scala.meta.convert.Convert[T,scala.meta.inputs.Input])
      |scala.meta.Pat.Type.tpe
      |scala.meta.Tree.collect(PartialFunction[scala.meta.Tree,T])
      |scala.meta.Tree.dialect
      |scala.meta.Tree.end
      |scala.meta.Tree.input
      |scala.meta.Tree.point
      |scala.meta.Tree.position
      |scala.meta.Tree.start
      |scala.meta.Tree.transform(PartialFunction[scala.meta.Tree,scala.meta.Tree])
      |scala.meta.Tree.traverse(PartialFunction[scala.meta.Tree,Unit])
      |scala.meta.Type.ctorRef(scala.meta.Ctor.Name)
      |scala.meta.Type.pat
    """.trim.stripMargin)
  }

  test("public top-level definitions (trees)") {
    // println(trees.toList.sorted.mkString(EOL))
    assert(trees.toList.sorted.mkString(EOL) === """
      |scala.meta.Case
      |scala.meta.Case.Api
      |scala.meta.Ctor
      |scala.meta.Ctor.Call
      |scala.meta.Ctor.Primary
      |scala.meta.Ctor.Primary.Api
      |scala.meta.Ctor.Ref
      |scala.meta.Ctor.Ref.Function
      |scala.meta.Ctor.Ref.Function.Api
      |scala.meta.Ctor.Ref.Name
      |scala.meta.Ctor.Ref.Name.Api
      |scala.meta.Ctor.Ref.Project
      |scala.meta.Ctor.Ref.Project.Api
      |scala.meta.Ctor.Ref.Select
      |scala.meta.Ctor.Ref.Select.Api
      |scala.meta.Ctor.Secondary
      |scala.meta.Ctor.Secondary.Api
      |scala.meta.Decl
      |scala.meta.Decl.Def
      |scala.meta.Decl.Def.Api
      |scala.meta.Decl.Type
      |scala.meta.Decl.Type.Api
      |scala.meta.Decl.Val
      |scala.meta.Decl.Val.Api
      |scala.meta.Decl.Var
      |scala.meta.Decl.Var.Api
      |scala.meta.Defn
      |scala.meta.Defn.Class
      |scala.meta.Defn.Class.Api
      |scala.meta.Defn.Def
      |scala.meta.Defn.Def.Api
      |scala.meta.Defn.Macro
      |scala.meta.Defn.Macro.Api
      |scala.meta.Defn.Object
      |scala.meta.Defn.Object.Api
      |scala.meta.Defn.Trait
      |scala.meta.Defn.Trait.Api
      |scala.meta.Defn.Type
      |scala.meta.Defn.Type.Api
      |scala.meta.Defn.Val
      |scala.meta.Defn.Val.Api
      |scala.meta.Defn.Var
      |scala.meta.Defn.Var.Api
      |scala.meta.Enumerator
      |scala.meta.Enumerator.Generator
      |scala.meta.Enumerator.Generator.Api
      |scala.meta.Enumerator.Guard
      |scala.meta.Enumerator.Guard.Api
      |scala.meta.Enumerator.Val
      |scala.meta.Enumerator.Val.Api
      |scala.meta.Import
      |scala.meta.Import.Api
      |scala.meta.Importee
      |scala.meta.Importee.Name
      |scala.meta.Importee.Name.Api
      |scala.meta.Importee.Rename
      |scala.meta.Importee.Rename.Api
      |scala.meta.Importee.Unimport
      |scala.meta.Importee.Unimport.Api
      |scala.meta.Importee.Wildcard
      |scala.meta.Importee.Wildcard.Api
      |scala.meta.Importer
      |scala.meta.Importer.Api
      |scala.meta.Lit
      |scala.meta.Lit.Api
      |scala.meta.Member
      |scala.meta.Member.Term
      |scala.meta.Member.Type
      |scala.meta.Mod
      |scala.meta.Mod.Abstract
      |scala.meta.Mod.Abstract.Api
      |scala.meta.Mod.Annot
      |scala.meta.Mod.Annot.Api
      |scala.meta.Mod.Case
      |scala.meta.Mod.Case.Api
      |scala.meta.Mod.Contravariant
      |scala.meta.Mod.Contravariant.Api
      |scala.meta.Mod.Covariant
      |scala.meta.Mod.Covariant.Api
      |scala.meta.Mod.Final
      |scala.meta.Mod.Final.Api
      |scala.meta.Mod.Implicit
      |scala.meta.Mod.Implicit.Api
      |scala.meta.Mod.Lazy
      |scala.meta.Mod.Lazy.Api
      |scala.meta.Mod.Override
      |scala.meta.Mod.Override.Api
      |scala.meta.Mod.Private
      |scala.meta.Mod.Private.Api
      |scala.meta.Mod.Protected
      |scala.meta.Mod.Protected.Api
      |scala.meta.Mod.Sealed
      |scala.meta.Mod.Sealed.Api
      |scala.meta.Mod.ValParam
      |scala.meta.Mod.ValParam.Api
      |scala.meta.Mod.VarParam
      |scala.meta.Mod.VarParam.Api
      |scala.meta.Name
      |scala.meta.Name.Anonymous
      |scala.meta.Name.Anonymous.Api
      |scala.meta.Name.Indeterminate
      |scala.meta.Name.Indeterminate.Api
      |scala.meta.Name.Qualifier
      |scala.meta.Pat
      |scala.meta.Pat.Alternative
      |scala.meta.Pat.Alternative.Api
      |scala.meta.Pat.Arg
      |scala.meta.Pat.Arg.SeqWildcard
      |scala.meta.Pat.Arg.SeqWildcard.Api
      |scala.meta.Pat.Bind
      |scala.meta.Pat.Bind.Api
      |scala.meta.Pat.Extract
      |scala.meta.Pat.Extract.Api
      |scala.meta.Pat.ExtractInfix
      |scala.meta.Pat.ExtractInfix.Api
      |scala.meta.Pat.Interpolate
      |scala.meta.Pat.Interpolate.Api
      |scala.meta.Pat.Tuple
      |scala.meta.Pat.Tuple.Api
      |scala.meta.Pat.Type
      |scala.meta.Pat.Type.Annotate
      |scala.meta.Pat.Type.Annotate.Api
      |scala.meta.Pat.Type.Apply
      |scala.meta.Pat.Type.Apply.Api
      |scala.meta.Pat.Type.ApplyInfix
      |scala.meta.Pat.Type.ApplyInfix.Api
      |scala.meta.Pat.Type.Compound
      |scala.meta.Pat.Type.Compound.Api
      |scala.meta.Pat.Type.Existential
      |scala.meta.Pat.Type.Existential.Api
      |scala.meta.Pat.Type.Function
      |scala.meta.Pat.Type.Function.Api
      |scala.meta.Pat.Type.Placeholder
      |scala.meta.Pat.Type.Placeholder.Api
      |scala.meta.Pat.Type.Project
      |scala.meta.Pat.Type.Project.Api
      |scala.meta.Pat.Type.Ref
      |scala.meta.Pat.Type.Tuple
      |scala.meta.Pat.Type.Tuple.Api
      |scala.meta.Pat.Type.Wildcard
      |scala.meta.Pat.Type.Wildcard.Api
      |scala.meta.Pat.Typed
      |scala.meta.Pat.Typed.Api
      |scala.meta.Pat.Var
      |scala.meta.Pat.Var.Term
      |scala.meta.Pat.Var.Term.Api
      |scala.meta.Pat.Var.Type
      |scala.meta.Pat.Var.Type.Api
      |scala.meta.Pat.Wildcard
      |scala.meta.Pat.Wildcard.Api
      |scala.meta.Pkg
      |scala.meta.Pkg.Api
      |scala.meta.Pkg.Object
      |scala.meta.Pkg.Object.Api
      |scala.meta.Ref
      |scala.meta.Scope
      |scala.meta.Source
      |scala.meta.Source.Api
      |scala.meta.Stat
      |scala.meta.Template
      |scala.meta.Template.Api
      |scala.meta.Term
      |scala.meta.Term.Annotate
      |scala.meta.Term.Annotate.Api
      |scala.meta.Term.Apply
      |scala.meta.Term.Apply.Api
      |scala.meta.Term.ApplyInfix
      |scala.meta.Term.ApplyInfix.Api
      |scala.meta.Term.ApplyType
      |scala.meta.Term.ApplyType.Api
      |scala.meta.Term.ApplyUnary
      |scala.meta.Term.ApplyUnary.Api
      |scala.meta.Term.Arg
      |scala.meta.Term.Arg.Named
      |scala.meta.Term.Arg.Named.Api
      |scala.meta.Term.Arg.Repeated
      |scala.meta.Term.Arg.Repeated.Api
      |scala.meta.Term.Ascribe
      |scala.meta.Term.Ascribe.Api
      |scala.meta.Term.Assign
      |scala.meta.Term.Assign.Api
      |scala.meta.Term.Block
      |scala.meta.Term.Block.Api
      |scala.meta.Term.Do
      |scala.meta.Term.Do.Api
      |scala.meta.Term.Eta
      |scala.meta.Term.Eta.Api
      |scala.meta.Term.For
      |scala.meta.Term.For.Api
      |scala.meta.Term.ForYield
      |scala.meta.Term.ForYield.Api
      |scala.meta.Term.Function
      |scala.meta.Term.Function.Api
      |scala.meta.Term.If
      |scala.meta.Term.If.Api
      |scala.meta.Term.Interpolate
      |scala.meta.Term.Interpolate.Api
      |scala.meta.Term.Match
      |scala.meta.Term.Match.Api
      |scala.meta.Term.Name
      |scala.meta.Term.Name.Api
      |scala.meta.Term.New
      |scala.meta.Term.New.Api
      |scala.meta.Term.Param
      |scala.meta.Term.Param.Api
      |scala.meta.Term.Param.Name
      |scala.meta.Term.PartialFunction
      |scala.meta.Term.PartialFunction.Api
      |scala.meta.Term.Placeholder
      |scala.meta.Term.Placeholder.Api
      |scala.meta.Term.Ref
      |scala.meta.Term.Return
      |scala.meta.Term.Return.Api
      |scala.meta.Term.Select
      |scala.meta.Term.Select.Api
      |scala.meta.Term.Super
      |scala.meta.Term.Super.Api
      |scala.meta.Term.This
      |scala.meta.Term.This.Api
      |scala.meta.Term.Throw
      |scala.meta.Term.Throw.Api
      |scala.meta.Term.TryWithCases
      |scala.meta.Term.TryWithCases.Api
      |scala.meta.Term.TryWithTerm
      |scala.meta.Term.TryWithTerm.Api
      |scala.meta.Term.Tuple
      |scala.meta.Term.Tuple.Api
      |scala.meta.Term.Update
      |scala.meta.Term.Update.Api
      |scala.meta.Term.While
      |scala.meta.Term.While.Api
      |scala.meta.Type
      |scala.meta.Type.Annotate
      |scala.meta.Type.Annotate.Api
      |scala.meta.Type.Apply
      |scala.meta.Type.Apply.Api
      |scala.meta.Type.ApplyInfix
      |scala.meta.Type.ApplyInfix.Api
      |scala.meta.Type.Arg
      |scala.meta.Type.Arg.ByName
      |scala.meta.Type.Arg.ByName.Api
      |scala.meta.Type.Arg.Repeated
      |scala.meta.Type.Arg.Repeated.Api
      |scala.meta.Type.Bounds
      |scala.meta.Type.Bounds.Api
      |scala.meta.Type.Compound
      |scala.meta.Type.Compound.Api
      |scala.meta.Type.Existential
      |scala.meta.Type.Existential.Api
      |scala.meta.Type.Function
      |scala.meta.Type.Function.Api
      |scala.meta.Type.Name
      |scala.meta.Type.Name.Api
      |scala.meta.Type.Param
      |scala.meta.Type.Param.Api
      |scala.meta.Type.Param.Name
      |scala.meta.Type.Placeholder
      |scala.meta.Type.Placeholder.Api
      |scala.meta.Type.Project
      |scala.meta.Type.Project.Api
      |scala.meta.Type.Ref
      |scala.meta.Type.Select
      |scala.meta.Type.Select.Api
      |scala.meta.Type.Singleton
      |scala.meta.Type.Singleton.Api
      |scala.meta.Type.Tuple
      |scala.meta.Type.Tuple.Api
    """.trim.stripMargin)
  }

  test("public top-level definitions (tokens)") {
    def encode(name: String) = name.replace(" ", "WHITESPACE").replace("\n", "\\n").replace("\r", "\\r").replace("\f", "\\f").replace("\t", "\\t")
    // println(tokens.toList.map(encode).sorted.mkString(EOL))
    assert(tokens.toList.map(encode).sorted.mkString(EOL) === """
      |scala.meta.tokens.Token.Abstract
      |scala.meta.tokens.Token.At
      |scala.meta.tokens.Token.BOF
      |scala.meta.tokens.Token.CR
      |scala.meta.tokens.Token.Case
      |scala.meta.tokens.Token.Catch
      |scala.meta.tokens.Token.Class
      |scala.meta.tokens.Token.Colon
      |scala.meta.tokens.Token.Comma
      |scala.meta.tokens.Token.Comment
      |scala.meta.tokens.Token.Constant
      |scala.meta.tokens.Token.Constant.Char
      |scala.meta.tokens.Token.Constant.Double
      |scala.meta.tokens.Token.Constant.Float
      |scala.meta.tokens.Token.Constant.Int
      |scala.meta.tokens.Token.Constant.Long
      |scala.meta.tokens.Token.Constant.String
      |scala.meta.tokens.Token.Constant.Symbol
      |scala.meta.tokens.Token.Def
      |scala.meta.tokens.Token.Do
      |scala.meta.tokens.Token.Dot
      |scala.meta.tokens.Token.EOF
      |scala.meta.tokens.Token.Else
      |scala.meta.tokens.Token.Equals
      |scala.meta.tokens.Token.Extends
      |scala.meta.tokens.Token.FF
      |scala.meta.tokens.Token.False
      |scala.meta.tokens.Token.Final
      |scala.meta.tokens.Token.Finally
      |scala.meta.tokens.Token.For
      |scala.meta.tokens.Token.ForSome
      |scala.meta.tokens.Token.Hash
      |scala.meta.tokens.Token.Ident
      |scala.meta.tokens.Token.If
      |scala.meta.tokens.Token.Implicit
      |scala.meta.tokens.Token.Import
      |scala.meta.tokens.Token.Interpolation
      |scala.meta.tokens.Token.Interpolation.End
      |scala.meta.tokens.Token.Interpolation.Id
      |scala.meta.tokens.Token.Interpolation.Part
      |scala.meta.tokens.Token.Interpolation.SpliceEnd
      |scala.meta.tokens.Token.Interpolation.SpliceStart
      |scala.meta.tokens.Token.Interpolation.Start
      |scala.meta.tokens.Token.LF
      |scala.meta.tokens.Token.Lazy
      |scala.meta.tokens.Token.LeftArrow
      |scala.meta.tokens.Token.LeftBrace
      |scala.meta.tokens.Token.LeftBracket
      |scala.meta.tokens.Token.LeftParen
      |scala.meta.tokens.Token.Macro
      |scala.meta.tokens.Token.Match
      |scala.meta.tokens.Token.New
      |scala.meta.tokens.Token.Null
      |scala.meta.tokens.Token.Object
      |scala.meta.tokens.Token.Override
      |scala.meta.tokens.Token.Package
      |scala.meta.tokens.Token.Private
      |scala.meta.tokens.Token.Protected
      |scala.meta.tokens.Token.Return
      |scala.meta.tokens.Token.RightArrow
      |scala.meta.tokens.Token.RightBrace
      |scala.meta.tokens.Token.RightBracket
      |scala.meta.tokens.Token.RightParen
      |scala.meta.tokens.Token.Sealed
      |scala.meta.tokens.Token.Semicolon
      |scala.meta.tokens.Token.Space
      |scala.meta.tokens.Token.Subtype
      |scala.meta.tokens.Token.Super
      |scala.meta.tokens.Token.Supertype
      |scala.meta.tokens.Token.Tab
      |scala.meta.tokens.Token.This
      |scala.meta.tokens.Token.Throw
      |scala.meta.tokens.Token.Trait
      |scala.meta.tokens.Token.True
      |scala.meta.tokens.Token.Try
      |scala.meta.tokens.Token.Type
      |scala.meta.tokens.Token.Underscore
      |scala.meta.tokens.Token.Val
      |scala.meta.tokens.Token.Var
      |scala.meta.tokens.Token.Viewbound
      |scala.meta.tokens.Token.While
      |scala.meta.tokens.Token.With
      |scala.meta.tokens.Token.Xml
      |scala.meta.tokens.Token.Xml.End
      |scala.meta.tokens.Token.Xml.Part
      |scala.meta.tokens.Token.Xml.SpliceEnd
      |scala.meta.tokens.Token.Xml.SpliceStart
      |scala.meta.tokens.Token.Xml.Start
      |scala.meta.tokens.Token.Yield
    """.trim.stripMargin)
  }
}

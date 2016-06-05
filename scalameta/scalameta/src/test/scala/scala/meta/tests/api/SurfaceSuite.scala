package scala.meta.tests
package api

import org.scalatest._
import org.scalameta.tests._
import org.scalameta.explore
import scala.compat.Platform.EOL

class SurfaceSuite extends scala.meta.tests.ast.AstSuite {
  import AstReflection._
  val reflectedTrees = {
    val root = symbolOf[scala.meta.Tree].asRoot
    val all = List(root) ++ root.allBranches ++ root.allLeafs
    all.map(_.sym.fullName).toSet
  }

  lazy val wildcardImportStatics = explore.wildcardImportStatics("scala.meta")
  lazy val allStatics = explore.allStatics("scala.meta")
  lazy val trees = wildcardImportStatics.filter(s => s != "scala.meta.Tree" && reflectedTrees(s.stripSuffix(".Api")))
  lazy val tokens = wildcardImportStatics.filter(_.startsWith("scala.meta.tokens.Token."))
  lazy val core = allStatics.diff(trees).diff(tokens).map(fullName => (fullName, wildcardImportStatics.contains(fullName))).toMap
  lazy val allSurface = explore.allSurface("scala.meta")
  lazy val coreSurface = allSurface.filter(entry => !(tokens ++ trees).exists(noncore => entry.startsWith(noncore)))

  test("statics (core)") {
    val diagnostic = core.keys.toList.sorted.map(fullName => {
      val suffix = if (core(fullName)) "" else " *"
      s"$fullName$suffix"
    }).mkString(EOL)
    // println(diagnostic)
    assert(diagnostic === """
      |scala.meta.Dialect
      |scala.meta.Tree
      |scala.meta.classifiers
      |scala.meta.classifiers.Classifiable *
      |scala.meta.classifiers.Classifier *
      |scala.meta.common
      |scala.meta.common.Convert *
      |scala.meta.common.Optional *
      |scala.meta.dialects
      |scala.meta.dialects.Dotty *
      |scala.meta.dialects.Metalevel *
      |scala.meta.dialects.Metalevel.Quoted *
      |scala.meta.dialects.Metalevel.Zero *
      |scala.meta.dialects.Sbt0136 *
      |scala.meta.dialects.Sbt0137 *
      |scala.meta.dialects.Scala210 *
      |scala.meta.dialects.Scala211 *
      |scala.meta.inline
      |scala.meta.inputs
      |scala.meta.inputs.Input
      |scala.meta.inputs.Input.File
      |scala.meta.inputs.Input.None
      |scala.meta.inputs.Input.Slice
      |scala.meta.inputs.Input.Stream
      |scala.meta.inputs.Input.String
      |scala.meta.inputs.Point
      |scala.meta.inputs.Point.None
      |scala.meta.inputs.Point.Offset
      |scala.meta.inputs.Position
      |scala.meta.inputs.Position.None
      |scala.meta.inputs.Position.Range
      |scala.meta.internal
      |scala.meta.parsers
      |scala.meta.parsers.Parse *
      |scala.meta.parsers.ParseException
      |scala.meta.parsers.Parsed
      |scala.meta.parsers.Parsed.Error
      |scala.meta.parsers.Parsed.Success
      |scala.meta.prettyprinters
      |scala.meta.prettyprinters.LowPriorityOptions.Lazy *
      |scala.meta.prettyprinters.Options *
      |scala.meta.prettyprinters.Options.Eager *
      |scala.meta.prettyprinters.Show *
      |scala.meta.prettyprinters.Structure
      |scala.meta.prettyprinters.Syntax
      |scala.meta.quasiquotes
      |scala.meta.quasiquotes.Lift
      |scala.meta.quasiquotes.Unlift
      |scala.meta.tokenizers
      |scala.meta.tokenizers.Tokenize *
      |scala.meta.tokenizers.TokenizeException
      |scala.meta.tokenizers.Tokenized
      |scala.meta.tokenizers.Tokenized.Error
      |scala.meta.tokenizers.Tokenized.Success
      |scala.meta.tokens
      |scala.meta.tokens.Token
      |scala.meta.tokens.Tokens
      |scala.meta.transversers
      |scala.meta.transversers.Transformer
      |scala.meta.transversers.Traverser
    """.trim.stripMargin)
  }

  test("prettyprinters for statics (core)") {
    val prettyprinterTests = new scala.meta.tests.prettyprinters.PublicSuite().testNames
    val nonPackageStatics = core.keys.filter(_.exists(_.isUpper))
    nonPackageStatics.foreach(name => {
      val isTested = prettyprinterTests.exists(testName => testName.startsWith(name))
      assert(isTested, s"$name prettyprinting is not tested")
    })
  }

  test("surface (core)") {
    // NOTE: I wanted to print out the entire coreSurface, but it's too big and the benefit is unclear.
    // If we're worried about binary compatibility, we should go ahead and use mima.
    // However, extension methods are few enough to digest and interesting enough to warrant printing out.

    // println(coreSurface.filter(_.startsWith("*")).sorted.mkString(EOL))
    assert(coreSurface.filter(_.startsWith("*")).sorted.mkString(EOL) === """
      |* (scala.meta.Dialect, scala.meta.Tree).syntax(implicit scala.meta.prettyprinters.Options): String
      |* (scala.meta.Dialect, scala.meta.inputs.Input).parse(implicit scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U]
      |* (scala.meta.Dialect, scala.meta.inputs.Input).tokenize(implicit scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized
      |* (scala.meta.Dialect, scala.meta.tokens.Token).syntax(implicit scala.meta.prettyprinters.Options): String
      |* (scala.meta.Dialect, scala.meta.tokens.Tokens).parse(implicit scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U]
      |* (scala.meta.Dialect, scala.meta.tokens.Tokens).syntax(implicit scala.meta.prettyprinters.Options): String
      |* (scala.meta.Dialect, scala.meta.tokens.Tokens).tokenize(implicit scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized
      |* (scala.meta.inputs.Input, scala.meta.Dialect).parse(implicit scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U]
      |* (scala.meta.inputs.Input, scala.meta.Dialect).tokenize(implicit scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized
      |* T(implicit scala.meta.classifiers.Classifiable[T]).is(implicit scala.meta.classifiers.Classifier[T,U]): Boolean
      |* T(implicit scala.meta.classifiers.Classifiable[T]).isNot(implicit scala.meta.classifiers.Classifier[T,U]): Boolean
      |* T(implicit scala.meta.prettyprinters.Structure[T]).structure: String
      |* T(implicit scala.meta.prettyprinters.Syntax[T]).syntax: String
      |* T.parse(implicit scala.meta.common.Convert[T,scala.meta.inputs.Input], scala.meta.parsers.Parse[U], scala.meta.Dialect): scala.meta.parsers.Parsed[U]
      |* T.show(implicit Style[T]): String
      |* T.tokenize(implicit scala.meta.common.Convert[T,scala.meta.inputs.Input], scala.meta.tokenizers.Tokenize, scala.meta.Dialect): scala.meta.tokenizers.Tokenized
      |* scala.meta.Dialect.apply(T)(implicit scala.meta.common.Convert[T,scala.meta.inputs.Input]): (scala.meta.Dialect, scala.meta.inputs.Input)
      |* scala.meta.Dialect.apply(scala.meta.Tree): (scala.meta.Dialect, scala.meta.Tree)
      |* scala.meta.Dialect.apply(scala.meta.tokens.Token): (scala.meta.Dialect, scala.meta.tokens.Token)
      |* scala.meta.Dialect.apply(scala.meta.tokens.Tokens): (scala.meta.Dialect, scala.meta.tokens.Tokens)
      |* scala.meta.Pat.Type.tpe: scala.meta.Type
      |* scala.meta.Tree.collect(PartialFunction[scala.meta.Tree,T]): List[T]
      |* scala.meta.Tree.transform(PartialFunction[scala.meta.Tree,scala.meta.Tree]): scala.meta.Tree
      |* scala.meta.Tree.traverse(PartialFunction[scala.meta.Tree,Unit]): Unit
      |* scala.meta.Type.ctorRef(scala.meta.Ctor.Name): scala.meta.Ctor.Call
      |* scala.meta.Type.pat: scala.meta.Pat.Type
    """.trim.stripMargin)
  }

  test("statics (trees)") {
    // println(trees.toList.sorted.mkString(EOL))
    assert(trees.toList.sorted.mkString(EOL) === """
      |scala.meta.Case
      |scala.meta.Ctor
      |scala.meta.Ctor.Call
      |scala.meta.Ctor.Primary
      |scala.meta.Ctor.Ref
      |scala.meta.Ctor.Ref.Function
      |scala.meta.Ctor.Ref.Name
      |scala.meta.Ctor.Ref.Project
      |scala.meta.Ctor.Ref.Select
      |scala.meta.Ctor.Secondary
      |scala.meta.Decl
      |scala.meta.Decl.Def
      |scala.meta.Decl.Type
      |scala.meta.Decl.Val
      |scala.meta.Decl.Var
      |scala.meta.Defn
      |scala.meta.Defn.Class
      |scala.meta.Defn.Def
      |scala.meta.Defn.Macro
      |scala.meta.Defn.Object
      |scala.meta.Defn.Trait
      |scala.meta.Defn.Type
      |scala.meta.Defn.Val
      |scala.meta.Defn.Var
      |scala.meta.Enumerator
      |scala.meta.Enumerator.Generator
      |scala.meta.Enumerator.Guard
      |scala.meta.Enumerator.Val
      |scala.meta.Import
      |scala.meta.Importee
      |scala.meta.Importee.Name
      |scala.meta.Importee.Rename
      |scala.meta.Importee.Unimport
      |scala.meta.Importee.Wildcard
      |scala.meta.Importer
      |scala.meta.Lit
      |scala.meta.Member
      |scala.meta.Member.Term
      |scala.meta.Member.Type
      |scala.meta.Mod
      |scala.meta.Mod.Abstract
      |scala.meta.Mod.Annot
      |scala.meta.Mod.Case
      |scala.meta.Mod.Contravariant
      |scala.meta.Mod.Covariant
      |scala.meta.Mod.Final
      |scala.meta.Mod.Implicit
      |scala.meta.Mod.Lazy
      |scala.meta.Mod.Override
      |scala.meta.Mod.Private
      |scala.meta.Mod.Protected
      |scala.meta.Mod.Sealed
      |scala.meta.Mod.ValParam
      |scala.meta.Mod.VarParam
      |scala.meta.Name
      |scala.meta.Name.Anonymous
      |scala.meta.Name.Indeterminate
      |scala.meta.Name.Qualifier
      |scala.meta.Pat
      |scala.meta.Pat.Alternative
      |scala.meta.Pat.Arg
      |scala.meta.Pat.Arg.SeqWildcard
      |scala.meta.Pat.Bind
      |scala.meta.Pat.Extract
      |scala.meta.Pat.ExtractInfix
      |scala.meta.Pat.Interpolate
      |scala.meta.Pat.Tuple
      |scala.meta.Pat.Type
      |scala.meta.Pat.Type.Annotate
      |scala.meta.Pat.Type.Apply
      |scala.meta.Pat.Type.ApplyInfix
      |scala.meta.Pat.Type.Compound
      |scala.meta.Pat.Type.Existential
      |scala.meta.Pat.Type.Function
      |scala.meta.Pat.Type.Placeholder
      |scala.meta.Pat.Type.Project
      |scala.meta.Pat.Type.Ref
      |scala.meta.Pat.Type.Tuple
      |scala.meta.Pat.Type.Wildcard
      |scala.meta.Pat.Typed
      |scala.meta.Pat.Var
      |scala.meta.Pat.Var.Term
      |scala.meta.Pat.Var.Type
      |scala.meta.Pat.Wildcard
      |scala.meta.Pat.Xml
      |scala.meta.Pkg
      |scala.meta.Pkg.Object
      |scala.meta.Ref
      |scala.meta.Scope
      |scala.meta.Source
      |scala.meta.Stat
      |scala.meta.Template
      |scala.meta.Term
      |scala.meta.Term.Annotate
      |scala.meta.Term.Apply
      |scala.meta.Term.ApplyInfix
      |scala.meta.Term.ApplyType
      |scala.meta.Term.ApplyUnary
      |scala.meta.Term.Arg
      |scala.meta.Term.Arg.Named
      |scala.meta.Term.Arg.Repeated
      |scala.meta.Term.Ascribe
      |scala.meta.Term.Assign
      |scala.meta.Term.Block
      |scala.meta.Term.Do
      |scala.meta.Term.Eta
      |scala.meta.Term.For
      |scala.meta.Term.ForYield
      |scala.meta.Term.Function
      |scala.meta.Term.If
      |scala.meta.Term.Interpolate
      |scala.meta.Term.Match
      |scala.meta.Term.Name
      |scala.meta.Term.New
      |scala.meta.Term.Param
      |scala.meta.Term.Param.Name
      |scala.meta.Term.PartialFunction
      |scala.meta.Term.Placeholder
      |scala.meta.Term.Ref
      |scala.meta.Term.Return
      |scala.meta.Term.Select
      |scala.meta.Term.Super
      |scala.meta.Term.This
      |scala.meta.Term.Throw
      |scala.meta.Term.TryWithCases
      |scala.meta.Term.TryWithTerm
      |scala.meta.Term.Tuple
      |scala.meta.Term.Update
      |scala.meta.Term.While
      |scala.meta.Term.Xml
      |scala.meta.Type
      |scala.meta.Type.Annotate
      |scala.meta.Type.Apply
      |scala.meta.Type.ApplyInfix
      |scala.meta.Type.Arg
      |scala.meta.Type.Arg.ByName
      |scala.meta.Type.Arg.Repeated
      |scala.meta.Type.Bounds
      |scala.meta.Type.Compound
      |scala.meta.Type.Existential
      |scala.meta.Type.Function
      |scala.meta.Type.Name
      |scala.meta.Type.Param
      |scala.meta.Type.Param.Name
      |scala.meta.Type.Placeholder
      |scala.meta.Type.Project
      |scala.meta.Type.Ref
      |scala.meta.Type.Select
      |scala.meta.Type.Singleton
      |scala.meta.Type.Tuple
    """.trim.stripMargin)
  }

  test("statics (tokens)") {
    def encode(name: String) = name.replace(" ", "WHITESPACE").replace("\n", "\\n").replace("\r", "\\r").replace("\f", "\\f").replace("\t", "\\t")
    // println(tokens.toList.map(encode).sorted.mkString(EOL))
    assert(tokens.toList.map(encode).sorted.mkString(EOL) === """
      |scala.meta.tokens.Token.At
      |scala.meta.tokens.Token.BOF
      |scala.meta.tokens.Token.CR
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
      |scala.meta.tokens.Token.Dot
      |scala.meta.tokens.Token.EOF
      |scala.meta.tokens.Token.Equals
      |scala.meta.tokens.Token.FF
      |scala.meta.tokens.Token.Hash
      |scala.meta.tokens.Token.Ident
      |scala.meta.tokens.Token.Interpolation
      |scala.meta.tokens.Token.Interpolation.End
      |scala.meta.tokens.Token.Interpolation.Id
      |scala.meta.tokens.Token.Interpolation.Part
      |scala.meta.tokens.Token.Interpolation.SpliceEnd
      |scala.meta.tokens.Token.Interpolation.SpliceStart
      |scala.meta.tokens.Token.Interpolation.Start
      |scala.meta.tokens.Token.KwAbstract
      |scala.meta.tokens.Token.KwCase
      |scala.meta.tokens.Token.KwCatch
      |scala.meta.tokens.Token.KwClass
      |scala.meta.tokens.Token.KwDef
      |scala.meta.tokens.Token.KwDo
      |scala.meta.tokens.Token.KwElse
      |scala.meta.tokens.Token.KwExtends
      |scala.meta.tokens.Token.KwFalse
      |scala.meta.tokens.Token.KwFinal
      |scala.meta.tokens.Token.KwFinally
      |scala.meta.tokens.Token.KwFor
      |scala.meta.tokens.Token.KwForsome
      |scala.meta.tokens.Token.KwIf
      |scala.meta.tokens.Token.KwImplicit
      |scala.meta.tokens.Token.KwImport
      |scala.meta.tokens.Token.KwLazy
      |scala.meta.tokens.Token.KwMacro
      |scala.meta.tokens.Token.KwMatch
      |scala.meta.tokens.Token.KwNew
      |scala.meta.tokens.Token.KwNull
      |scala.meta.tokens.Token.KwObject
      |scala.meta.tokens.Token.KwOverride
      |scala.meta.tokens.Token.KwPackage
      |scala.meta.tokens.Token.KwPrivate
      |scala.meta.tokens.Token.KwProtected
      |scala.meta.tokens.Token.KwReturn
      |scala.meta.tokens.Token.KwSealed
      |scala.meta.tokens.Token.KwSuper
      |scala.meta.tokens.Token.KwThis
      |scala.meta.tokens.Token.KwThrow
      |scala.meta.tokens.Token.KwTrait
      |scala.meta.tokens.Token.KwTrue
      |scala.meta.tokens.Token.KwTry
      |scala.meta.tokens.Token.KwType
      |scala.meta.tokens.Token.KwVal
      |scala.meta.tokens.Token.KwVar
      |scala.meta.tokens.Token.KwWhile
      |scala.meta.tokens.Token.KwWith
      |scala.meta.tokens.Token.KwYield
      |scala.meta.tokens.Token.LF
      |scala.meta.tokens.Token.LeftArrow
      |scala.meta.tokens.Token.LeftBrace
      |scala.meta.tokens.Token.LeftBracket
      |scala.meta.tokens.Token.LeftParen
      |scala.meta.tokens.Token.RightArrow
      |scala.meta.tokens.Token.RightBrace
      |scala.meta.tokens.Token.RightBracket
      |scala.meta.tokens.Token.RightParen
      |scala.meta.tokens.Token.Semicolon
      |scala.meta.tokens.Token.Space
      |scala.meta.tokens.Token.Subtype
      |scala.meta.tokens.Token.Supertype
      |scala.meta.tokens.Token.Tab
      |scala.meta.tokens.Token.Underscore
      |scala.meta.tokens.Token.Viewbound
      |scala.meta.tokens.Token.Xml
      |scala.meta.tokens.Token.Xml.End
      |scala.meta.tokens.Token.Xml.Part
      |scala.meta.tokens.Token.Xml.SpliceEnd
      |scala.meta.tokens.Token.Xml.SpliceStart
      |scala.meta.tokens.Token.Xml.Start
    """.trim.stripMargin)
  }
}

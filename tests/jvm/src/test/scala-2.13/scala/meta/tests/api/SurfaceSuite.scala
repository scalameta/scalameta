package scala.meta.tests
package api

import org.scalameta.explore
import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.internal.trees.AstNamerMacros

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class SurfaceSuite extends TreeSuiteBase {
  object CoreReflection
      extends {
        val u: ru.type = ru
        val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.Tree].getClassLoader)
      }
      with scala.meta.internal.trees.Reflection
      with scala.meta.internal.tokens.Reflection
  import CoreReflection._

  private lazy val reflectedTrees = {
    val root = symbolOf[scala.meta.Tree].asRoot
    val all = List(root) ++ root.allBranches ++ root.allLeafs
    all.map(_.sym.fullName).toSet
  }
  private def isPublic(x: Symbol) = x.isPublic
  private def isPrivateWithin(x: Symbol) = x.privateWithin != NoSymbol
  @inline
  private def filterFullNames(a: List[Adt], f: Symbol => Boolean) = a.flatMap { x =>
    val sym = x.sym
    if (f(sym)) Some(sym.fullName) else None
  }
  private lazy val wildcardImportStatics = explore.wildcardImportStatics("scala.meta")
  private def lsp4s(symbol: String): Boolean = {
    val fullName = symbol.stripPrefix("* ")
    // We ignore the symbols from scalameta/lsp4s that are transitively brought
    // in via the bloop-frontend dependency in testsJVM.
    fullName.startsWith("monix") || fullName.startsWith("scala.meta.lsp") ||
    fullName.startsWith("scala.meta.jsonrpc") || {
      val name = fullName.substring(fullName.lastIndexOf('.') + 1)
      name == AstNamerMacros.initialName || name.startsWith(AstNamerMacros.afterNamePrefix)
    }
  }
  private lazy val trees = wildcardImportStatics
    .filter(s => s != "scala.meta.Tree" && reflectedTrees(s.stripSuffix(".Api")))
  private lazy val tokenRoot = symbolOf[scala.meta.tokens.Token].asRoot
  private lazy val tokens = filterFullNames(tokenRoot.allLeafs, isPublic).sorted
  private lazy val core = explore.allStatics("scala.meta").filterNot(lsp4s).diff(trees).diff(tokens)
    .diff(filterFullNames(tokenRoot.allLeafs, isPrivateWithin))
    .diff(filterFullNames(tokenRoot.allBranches, x => isPrivateWithin(x) || isPublic(x)))
    .map(fullName => (fullName, wildcardImportStatics.contains(fullName))).toMap

  test("statics (core)") {
    val diagnostic = core.keys.toList.filter(!_.endsWith("LowPriority")).sorted.map { fullName =>
      val suffix = if (core(fullName)) "" else " *"
      s"$fullName$suffix"
    }.mkString(EOL)
    // println(diagnostic)
    assertNoDiff(
      diagnostic,
      """|
         |scala.meta.Dialect
         |scala.meta.Tree
         |scala.meta.Tree.ImplicitOptionTree
         |scala.meta.Tree.ImplicitTree
         |scala.meta.XtensionDialectApply
         |scala.meta.XtensionDialectTokenSyntax
         |scala.meta.XtensionDialectTokensSyntax
         |scala.meta.XtensionDialectTreeSyntax
         |scala.meta.XtensionTree
         |scala.meta.XtensionTreeT
         |scala.meta.classifiers
         |scala.meta.classifiers.Classifiable *
         |scala.meta.classifiers.Classifier
         |scala.meta.cli
         |scala.meta.cli.Metac *
         |scala.meta.cli.Metacp *
         |scala.meta.cli.Metap *
         |scala.meta.cli.Reporter *
         |scala.meta.common
         |scala.meta.common.Convert *
         |scala.meta.common.Optional *
         |scala.meta.dialects
         |scala.meta.dialects.Dotty *
         |scala.meta.dialects.Paradise211 *
         |scala.meta.dialects.Paradise212 *
         |scala.meta.dialects.ParadiseTypelevel211 *
         |scala.meta.dialects.ParadiseTypelevel212 *
         |scala.meta.dialects.Sbt *
         |scala.meta.dialects.Sbt0136 *
         |scala.meta.dialects.Sbt0137 *
         |scala.meta.dialects.Sbt1 *
         |scala.meta.dialects.Scala *
         |scala.meta.dialects.Scala210 *
         |scala.meta.dialects.Scala211 *
         |scala.meta.dialects.Scala212 *
         |scala.meta.dialects.Scala212Source3 *
         |scala.meta.dialects.Scala213 *
         |scala.meta.dialects.Scala213Source3 *
         |scala.meta.dialects.Scala3 *
         |scala.meta.dialects.Scala30 *
         |scala.meta.dialects.Scala31 *
         |scala.meta.dialects.Scala32 *
         |scala.meta.dialects.Scala33 *
         |scala.meta.dialects.Scala34 *
         |scala.meta.dialects.Scala35 *
         |scala.meta.dialects.Scala36 *
         |scala.meta.dialects.Scala3Future *
         |scala.meta.dialects.Typelevel211 *
         |scala.meta.dialects.Typelevel212 *
         |scala.meta.inputs
         |scala.meta.inputs.Input
         |scala.meta.inputs.Input.Ammonite *
         |scala.meta.inputs.Input.File *
         |scala.meta.inputs.Input.None *
         |scala.meta.inputs.Input.Proxy *
         |scala.meta.inputs.Input.Slice *
         |scala.meta.inputs.Input.Stream *
         |scala.meta.inputs.Input.String *
         |scala.meta.inputs.Input.Text *
         |scala.meta.inputs.Input.VirtualFile *
         |scala.meta.inputs.Input.WithTokenizerOptions *
         |scala.meta.inputs.InputException *
         |scala.meta.inputs.InputRange *
         |scala.meta.inputs.Position
         |scala.meta.inputs.Position.None *
         |scala.meta.inputs.Position.Range *
         |scala.meta.internal
         |scala.meta.io
         |scala.meta.io.AbsolutePath
         |scala.meta.io.Classpath
         |scala.meta.io.RelativePath
         |scala.meta.metac
         |scala.meta.metac.Settings *
         |scala.meta.metacp
         |scala.meta.metacp.Result *
         |scala.meta.metacp.Settings *
         |scala.meta.metap
         |scala.meta.metap.Format *
         |scala.meta.metap.Format.Compact *
         |scala.meta.metap.Format.Detailed *
         |scala.meta.metap.Format.Proto *
         |scala.meta.metap.Settings *
         |scala.meta.parsers
         |scala.meta.parsers.Parse *
         |scala.meta.parsers.ParseException
         |scala.meta.parsers.Parsed
         |scala.meta.parsers.Parsed.Error *
         |scala.meta.parsers.Parsed.Success *
         |scala.meta.prettyprinters
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
         |scala.meta.tokenizers.Tokenized.Error *
         |scala.meta.tokenizers.Tokenized.Success *
         |scala.meta.tokenizers.TokenizerOptions *
         |scala.meta.tokens
         |scala.meta.tokens.StringExtensions *
         |scala.meta.tokens.Token
         |scala.meta.tokens.Token.Interpolation *
         |scala.meta.tokens.Token.Xml *
         |scala.meta.tokens.TokenExtensions *
         |scala.meta.tokens.Tokens
         |scala.meta.transversers
         |scala.meta.transversers.SimpleTraverser *
         |scala.meta.transversers.Transformer
         |scala.meta.transversers.Traverser
         |scala.meta.trees
         |scala.meta.trees.Error *
         |scala.meta.trees.Error.MissingDialectException *
         |scala.meta.trees.Origin *
         |scala.meta.trees.Origin.DialectOnly *
         |scala.meta.trees.Origin.None *
         |scala.meta.trees.Origin.Parsed *
         |scala.meta.trees.Origin.ParsedSource *
         |""".stripMargin.lf2nl
    )
  }

  test("surface (core)") {
    // NOTE: I wanted to print out the entire coreSurface, but it's too big and the benefit is unclear.
    // If we're worried about binary compatibility, we should go ahead and use mima.
    // However, extension methods are few enough to digest and interesting enough to warrant printing out.

    // println(coreSurface.filter(_.startsWith("*")).sorted.mkString(EOL))
    val surfaceTypes = tokens ++ trees
    val obtained = explore.extensionSurface("scala.meta")
      .filter(x => x.startsWith("*") && !lsp4s(x) && !surfaceTypes.exists(x.startsWith))
    assertNoDiff(
      obtained.sorted.mkString(EOL),
      """|
         |* (scala.meta.Dialect, scala.meta.Tree).equals(Any): Boolean
         |* (scala.meta.Dialect, scala.meta.Tree).hashCode(): Int
         |* (scala.meta.Dialect, scala.meta.Tree).syntax: String
         |* (scala.meta.Dialect, scala.meta.inputs.Input).parse(implicit scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U]
         |* (scala.meta.Dialect, scala.meta.inputs.Input).tokenize(implicit scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized
         |* (scala.meta.Dialect, scala.meta.tokens.Token).equals(Any): Boolean
         |* (scala.meta.Dialect, scala.meta.tokens.Token).hashCode(): Int
         |* (scala.meta.Dialect, scala.meta.tokens.Token).syntax: String
         |* (scala.meta.Dialect, scala.meta.tokens.Tokens).equals(Any): Boolean
         |* (scala.meta.Dialect, scala.meta.tokens.Tokens).hashCode(): Int
         |* (scala.meta.Dialect, scala.meta.tokens.Tokens).parse(implicit scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U]
         |* (scala.meta.Dialect, scala.meta.tokens.Tokens).syntax: String
         |* (scala.meta.Dialect, scala.meta.tokens.Tokens).tokenize(implicit scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized
         |* (scala.meta.inputs.Input, scala.meta.Dialect).parse(implicit scala.meta.parsers.Parse[U]): scala.meta.parsers.Parsed[U]
         |* (scala.meta.inputs.Input, scala.meta.Dialect).tokenize(implicit scala.meta.tokenizers.Tokenize): scala.meta.tokenizers.Tokenized
         |* A.ancestor(Int): Option[scala.meta.Tree]
         |* A.equals(Any): Boolean
         |* A.hashCode(): Int
         |* A.maybeParse(implicit scala.meta.Dialect, scala.meta.parsers.Parse[A]): scala.meta.package.Parsed[A]
         |* Option[A].ancestor(Int): Option[scala.meta.Tree]
         |* Option[A].equals(Any): Boolean
         |* Option[A].hashCode(): Int
         |* Option[A].parent: Option[scala.meta.Tree]
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).is(implicit XtensionOptionClassifiable.this.C[U]): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAny(implicit XtensionOptionClassifiable.this.C[U1], XtensionOptionClassifiable.this.C[U2]): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAny(implicit XtensionOptionClassifiable.this.C[U1], XtensionOptionClassifiable.this.C[U2], XtensionOptionClassifiable.this.C[U3]): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAny(implicit XtensionOptionClassifiable.this.C[U1], XtensionOptionClassifiable.this.C[U2], XtensionOptionClassifiable.this.C[U3], XtensionOptionClassifiable.this.C[U4]): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAnyOf(XtensionOptionClassifiable.this.C[_]*): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAnyOfOpt(XtensionOptionClassifiable.this.C[_]*): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAnyOpt(implicit XtensionOptionClassifiable.this.C[U1], XtensionOptionClassifiable.this.C[U2]): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAnyOpt(implicit XtensionOptionClassifiable.this.C[U1], XtensionOptionClassifiable.this.C[U2], XtensionOptionClassifiable.this.C[U3]): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isAnyOpt(implicit XtensionOptionClassifiable.this.C[U1], XtensionOptionClassifiable.this.C[U2], XtensionOptionClassifiable.this.C[U3], XtensionOptionClassifiable.this.C[U4]): Boolean
         |* Option[T](implicit scala.meta.classifiers.Classifiable[T]).isOpt(implicit XtensionOptionClassifiable.this.C[U]): Boolean
         |* String.equals(Any): Boolean
         |* String.hashCode(): Int
         |* String.isBackquoted: Boolean
         |* String.isIdentSymbolicInfixOperator: Boolean
         |* T(implicit scala.meta.classifiers.Classifiable[T]).is(implicit XtensionClassifiable.this.C[U]): Boolean
         |* T(implicit scala.meta.classifiers.Classifiable[T]).isAny(implicit XtensionClassifiable.this.C[U1], XtensionClassifiable.this.C[U2]): Boolean
         |* T(implicit scala.meta.classifiers.Classifiable[T]).isAny(implicit XtensionClassifiable.this.C[U1], XtensionClassifiable.this.C[U2], XtensionClassifiable.this.C[U3]): Boolean
         |* T(implicit scala.meta.classifiers.Classifiable[T]).isAny(implicit XtensionClassifiable.this.C[U1], XtensionClassifiable.this.C[U2], XtensionClassifiable.this.C[U3], XtensionClassifiable.this.C[U4]): Boolean
         |* T(implicit scala.meta.classifiers.Classifiable[T]).isAnyOf(XtensionClassifiable.this.C[_]*): Boolean
         |* T(implicit scala.meta.classifiers.Classifiable[T]).isNot(implicit XtensionClassifiable.this.C[U]): Boolean
         |* T(implicit scala.meta.prettyprinters.Structure[T]).structure: String
         |* T(implicit scala.meta.prettyprinters.Syntax[T]).syntax: String
         |* T.parse(implicit scala.meta.common.Convert[T,scala.meta.inputs.Input], scala.meta.parsers.Parse[U], scala.meta.Dialect): scala.meta.parsers.Parsed[U]
         |* T.show(implicit Style[T]): String
         |* T.tokenize(implicit scala.meta.common.Convert[T,scala.meta.inputs.Input], scala.meta.tokenizers.Tokenize, scala.meta.Dialect): scala.meta.tokenizers.Tokenized
         |* T.withDialectIfRootAndNotSet(implicit scala.meta.Dialect): T
         |* scala.meta.Dialect.apply(T)(implicit scala.meta.common.Convert[T,scala.meta.inputs.Input]): (scala.meta.Dialect, scala.meta.inputs.Input)
         |* scala.meta.Dialect.apply(scala.meta.Tree): (scala.meta.Dialect, scala.meta.Tree)
         |* scala.meta.Dialect.apply(scala.meta.tokens.Token): (scala.meta.Dialect, scala.meta.tokens.Token)
         |* scala.meta.Dialect.apply(scala.meta.tokens.Tokens): (scala.meta.Dialect, scala.meta.tokens.Tokens)
         |* scala.meta.Dialect.equals(Any): Boolean
         |* scala.meta.Dialect.hashCode(): Int
         |* scala.meta.Tree.collect(PartialFunction[scala.meta.Tree,T]): List[T]
         |* scala.meta.Tree.equals(Any): Boolean
         |* scala.meta.Tree.hashCode(): Int
         |* scala.meta.Tree.maybeParseAs(implicit scala.reflect.ClassTag[A], scala.meta.Dialect, scala.meta.parsers.Parse[A]): scala.meta.package.Parsed[A]
         |* scala.meta.Tree.reparseAs(implicit scala.meta.Dialect, scala.meta.parsers.Parse[A]): scala.meta.package.Parsed[A]
         |* scala.meta.Tree.transform(PartialFunction[scala.meta.Tree,scala.meta.Tree]): scala.meta.Tree
         |* scala.meta.Tree.traverse(PartialFunction[scala.meta.Tree,Unit]): Unit
         |* scala.meta.tokens.Token.equals(Any): Boolean
         |* scala.meta.tokens.Token.hashCode(): Int
         |* scala.meta.tokens.Token.isBackquoted: Boolean
         |* scala.meta.tokens.Token.isIdentSymbolicInfixOperator: Boolean
         |* scala.meta.tokens.Token.isSymbolicInfixOperator: Boolean
         |""".stripMargin.lf2nl
    )
  }

  test("statics (trees)") {
    // println(trees.toList.sorted.mkString(EOL))
    val obtained = trees.filterNot(_.endsWith("Quasi"))
    assertNoDiff(
      obtained.sorted.mkString(EOL),
      """|
         |scala.meta.Case
         |scala.meta.CaseTree
         |scala.meta.Ctor
         |scala.meta.Ctor.Block
         |scala.meta.Ctor.Primary
         |scala.meta.Ctor.Secondary
         |scala.meta.Decl
         |scala.meta.Decl.Def
         |scala.meta.Decl.Given
         |scala.meta.Decl.Type
         |scala.meta.Decl.Val
         |scala.meta.Decl.Var
         |scala.meta.Defn
         |scala.meta.Defn.Class
         |scala.meta.Defn.Def
         |scala.meta.Defn.Enum
         |scala.meta.Defn.EnumCase
         |scala.meta.Defn.ExtensionGroup
         |scala.meta.Defn.Given
         |scala.meta.Defn.GivenAlias
         |scala.meta.Defn.Macro
         |scala.meta.Defn.Object
         |scala.meta.Defn.RepeatedEnumCase
         |scala.meta.Defn.Trait
         |scala.meta.Defn.Type
         |scala.meta.Defn.Val
         |scala.meta.Defn.Var
         |scala.meta.Enumerator
         |scala.meta.Enumerator.Assign
         |scala.meta.Enumerator.CaseGenerator
         |scala.meta.Enumerator.Generator
         |scala.meta.Enumerator.Guard
         |scala.meta.Enumerator.Val
         |scala.meta.Export
         |scala.meta.Import
         |scala.meta.ImportExportStat
         |scala.meta.Importee
         |scala.meta.Importee.Given
         |scala.meta.Importee.GivenAll
         |scala.meta.Importee.Name
         |scala.meta.Importee.Rename
         |scala.meta.Importee.Unimport
         |scala.meta.Importee.Wildcard
         |scala.meta.Importer
         |scala.meta.Init
         |scala.meta.Lit
         |scala.meta.Lit.Boolean
         |scala.meta.Lit.Byte
         |scala.meta.Lit.Char
         |scala.meta.Lit.Double
         |scala.meta.Lit.Float
         |scala.meta.Lit.Int
         |scala.meta.Lit.Long
         |scala.meta.Lit.Null
         |scala.meta.Lit.Short
         |scala.meta.Lit.String
         |scala.meta.Lit.Symbol
         |scala.meta.Lit.Unit
         |scala.meta.Member
         |scala.meta.Member.Apply
         |scala.meta.Member.ArgClause
         |scala.meta.Member.Function
         |scala.meta.Member.Infix
         |scala.meta.Member.Param
         |scala.meta.Member.ParamClause
         |scala.meta.Member.ParamClauseGroup
         |scala.meta.Member.SyntaxValuesClause
         |scala.meta.Member.Term
         |scala.meta.Member.Tuple
         |scala.meta.Member.Type
         |scala.meta.Mod
         |scala.meta.Mod.Abstract
         |scala.meta.Mod.Annot
         |scala.meta.Mod.ArgsType
         |scala.meta.Mod.Case
         |scala.meta.Mod.Contravariant
         |scala.meta.Mod.Covariant
         |scala.meta.Mod.Erased
         |scala.meta.Mod.Final
         |scala.meta.Mod.Implicit
         |scala.meta.Mod.Infix
         |scala.meta.Mod.Inline
         |scala.meta.Mod.Lazy
         |scala.meta.Mod.Opaque
         |scala.meta.Mod.Open
         |scala.meta.Mod.Override
         |scala.meta.Mod.ParamsType
         |scala.meta.Mod.Private
         |scala.meta.Mod.Protected
         |scala.meta.Mod.Sealed
         |scala.meta.Mod.Super
         |scala.meta.Mod.Tracked
         |scala.meta.Mod.Transparent
         |scala.meta.Mod.Using
         |scala.meta.Mod.ValParam
         |scala.meta.Mod.VarParam
         |scala.meta.Mod.Variant
         |scala.meta.Mod.WithWithin
         |scala.meta.MultiSource
         |scala.meta.Name
         |scala.meta.Name.Anonymous
         |scala.meta.Name.Indeterminate
         |scala.meta.Name.Placeholder
         |scala.meta.Name.This
         |scala.meta.Pat
         |scala.meta.Pat.Alternative
         |scala.meta.Pat.ArgClause
         |scala.meta.Pat.Assign
         |scala.meta.Pat.Bind
         |scala.meta.Pat.Extract
         |scala.meta.Pat.ExtractInfix
         |scala.meta.Pat.Given
         |scala.meta.Pat.Interpolate
         |scala.meta.Pat.Macro
         |scala.meta.Pat.Repeated
         |scala.meta.Pat.SeqWildcard
         |scala.meta.Pat.Tuple
         |scala.meta.Pat.Typed
         |scala.meta.Pat.Var
         |scala.meta.Pat.Wildcard
         |scala.meta.Pat.Xml
         |scala.meta.Pkg
         |scala.meta.Pkg.Body
         |scala.meta.Pkg.Object
         |scala.meta.Ref
         |scala.meta.Self
         |scala.meta.Source
         |scala.meta.Stat
         |scala.meta.Stat.Block
         |scala.meta.Stat.TypeDef
         |scala.meta.Stat.WithCtor
         |scala.meta.Stat.WithMods
         |scala.meta.Stat.WithTemplate
         |scala.meta.Template
         |scala.meta.Template.Body
         |scala.meta.Term
         |scala.meta.Term.Annotate
         |scala.meta.Term.Anonymous
         |scala.meta.Term.AnonymousFunction
         |scala.meta.Term.Apply
         |scala.meta.Term.ApplyInfix
         |scala.meta.Term.ApplyType
         |scala.meta.Term.ApplyUnary
         |scala.meta.Term.ApplyUsing
         |scala.meta.Term.ArgClause
         |scala.meta.Term.Ascribe
         |scala.meta.Term.Assign
         |scala.meta.Term.Block
         |scala.meta.Term.CasesBlock
         |scala.meta.Term.ContextFunction
         |scala.meta.Term.Do
         |scala.meta.Term.EndMarker
         |scala.meta.Term.EnumeratorsBlock
         |scala.meta.Term.Eta
         |scala.meta.Term.For
         |scala.meta.Term.ForClause
         |scala.meta.Term.ForYield
         |scala.meta.Term.Function
         |scala.meta.Term.FunctionTerm
         |scala.meta.Term.If
         |scala.meta.Term.Interpolate
         |scala.meta.Term.Match
         |scala.meta.Term.Name
         |scala.meta.Term.New
         |scala.meta.Term.NewAnonymous
         |scala.meta.Term.Param
         |scala.meta.Term.ParamClause
         |scala.meta.Term.PartialFunction
         |scala.meta.Term.Placeholder
         |scala.meta.Term.PolyFunction
         |scala.meta.Term.QuotedMacroExpr
         |scala.meta.Term.QuotedMacroType
         |scala.meta.Term.Ref
         |scala.meta.Term.Repeated
         |scala.meta.Term.Return
         |scala.meta.Term.Select
         |scala.meta.Term.SplicedMacroExpr
         |scala.meta.Term.SplicedMacroPat
         |scala.meta.Term.Super
         |scala.meta.Term.This
         |scala.meta.Term.Throw
         |scala.meta.Term.Try
         |scala.meta.Term.TryClause
         |scala.meta.Term.TryWithHandler
         |scala.meta.Term.Tuple
         |scala.meta.Term.While
         |scala.meta.Term.Xml
         |scala.meta.Tree.Block
         |scala.meta.Tree.CasesBlock
         |scala.meta.Tree.WithBody
         |scala.meta.Tree.WithCases
         |scala.meta.Tree.WithCasesBlock
         |scala.meta.Tree.WithCond
         |scala.meta.Tree.WithCondOpt
         |scala.meta.Tree.WithDeclTpe
         |scala.meta.Tree.WithDeclTpeOpt
         |scala.meta.Tree.WithEnumeratorsBlock
         |scala.meta.Tree.WithEnums
         |scala.meta.Tree.WithExprs
         |scala.meta.Tree.WithParamClauseGroup
         |scala.meta.Tree.WithParamClauseGroups
         |scala.meta.Tree.WithParamClauses
         |scala.meta.Tree.WithPats
         |scala.meta.Tree.WithStats
         |scala.meta.Tree.WithStatsBlock
         |scala.meta.Tree.WithTParamClause
         |scala.meta.Type
         |scala.meta.Type.And
         |scala.meta.Type.Annotate
         |scala.meta.Type.AnonymousLambda
         |scala.meta.Type.AnonymousName
         |scala.meta.Type.AnonymousParam
         |scala.meta.Type.Apply
         |scala.meta.Type.ApplyInfix
         |scala.meta.Type.ArgClause
         |scala.meta.Type.Block
         |scala.meta.Type.Bounds
         |scala.meta.Type.BoundsAlias
         |scala.meta.Type.ByName
         |scala.meta.Type.CasesBlock
         |scala.meta.Type.ContextFunction
         |scala.meta.Type.Existential
         |scala.meta.Type.FuncParamClause
         |scala.meta.Type.Function
         |scala.meta.Type.FunctionArg
         |scala.meta.Type.FunctionParamOrArg
         |scala.meta.Type.FunctionType
         |scala.meta.Type.ImplicitFunction
         |scala.meta.Type.Lambda
         |scala.meta.Type.Macro
         |scala.meta.Type.Match
         |scala.meta.Type.Method
         |scala.meta.Type.Name
         |scala.meta.Type.Or
         |scala.meta.Type.Param
         |scala.meta.Type.ParamClause
         |scala.meta.Type.PatWildcard
         |scala.meta.Type.Placeholder
         |scala.meta.Type.Placeholder.Impl
         |scala.meta.Type.PolyFunction
         |scala.meta.Type.Project
         |scala.meta.Type.Ref
         |scala.meta.Type.Refine
         |scala.meta.Type.Repeated
         |scala.meta.Type.Select
         |scala.meta.Type.Singleton
         |scala.meta.Type.Tuple
         |scala.meta.Type.TypedParam
         |scala.meta.Type.Var
         |scala.meta.Type.Wildcard
         |scala.meta.Type.With
         |scala.meta.TypeCase
         |""".stripMargin.lf2nl
    )
  }

  test("statics (tokens)") {
    assertNoDiff(
      tokens.sorted.mkString(EOL),
      """|
         |scala.meta.tokens.Token.At
         |scala.meta.tokens.Token.BOF
         |scala.meta.tokens.Token.CR
         |scala.meta.tokens.Token.CRLF
         |scala.meta.tokens.Token.Colon
         |scala.meta.tokens.Token.Comma
         |scala.meta.tokens.Token.Comment
         |scala.meta.tokens.Token.Constant.Char
         |scala.meta.tokens.Token.Constant.Double
         |scala.meta.tokens.Token.Constant.Float
         |scala.meta.tokens.Token.Constant.Int
         |scala.meta.tokens.Token.Constant.Long
         |scala.meta.tokens.Token.Constant.String
         |scala.meta.tokens.Token.Constant.Symbol
         |scala.meta.tokens.Token.ContextArrow
         |scala.meta.tokens.Token.Dot
         |scala.meta.tokens.Token.EOF
         |scala.meta.tokens.Token.Equals
         |scala.meta.tokens.Token.FF
         |scala.meta.tokens.Token.Hash
         |scala.meta.tokens.Token.Ident
         |scala.meta.tokens.Token.Indentation.Indent
         |scala.meta.tokens.Token.Indentation.Outdent
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
         |scala.meta.tokens.Token.KwEnum
         |scala.meta.tokens.Token.KwExport
         |scala.meta.tokens.Token.KwExtends
         |scala.meta.tokens.Token.KwFalse
         |scala.meta.tokens.Token.KwFinal
         |scala.meta.tokens.Token.KwFinally
         |scala.meta.tokens.Token.KwFor
         |scala.meta.tokens.Token.KwForsome
         |scala.meta.tokens.Token.KwGiven
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
         |scala.meta.tokens.Token.KwThen
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
         |scala.meta.tokens.Token.MacroQuote
         |scala.meta.tokens.Token.MacroSplice
         |scala.meta.tokens.Token.MultiHS
         |scala.meta.tokens.Token.MultiNL
         |scala.meta.tokens.Token.RightArrow
         |scala.meta.tokens.Token.RightBrace
         |scala.meta.tokens.Token.RightBracket
         |scala.meta.tokens.Token.RightParen
         |scala.meta.tokens.Token.Semicolon
         |scala.meta.tokens.Token.Shebang
         |scala.meta.tokens.Token.Space
         |scala.meta.tokens.Token.Subtype
         |scala.meta.tokens.Token.Supertype
         |scala.meta.tokens.Token.Tab
         |scala.meta.tokens.Token.TypeLambdaArrow
         |scala.meta.tokens.Token.Underscore
         |scala.meta.tokens.Token.Viewbound
         |scala.meta.tokens.Token.Xml.End
         |scala.meta.tokens.Token.Xml.Part
         |scala.meta.tokens.Token.Xml.SpliceEnd
         |scala.meta.tokens.Token.Xml.SpliceStart
         |scala.meta.tokens.Token.Xml.Start
         |""".stripMargin.lf2nl
    )
  }
}

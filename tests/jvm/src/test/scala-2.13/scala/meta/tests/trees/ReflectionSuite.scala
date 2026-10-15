package scala.meta.tests
package trees

import org.scalameta.internal.ScalaCompat.EOL

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class ReflectionSuite extends TreeSuiteBase {
  object TreeReflection
      extends {
        val u: ru.type = ru
        val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.Tree].getClassLoader)
      }
      with scala.meta.internal.trees.Reflection
  import TreeReflection._

  // NOTE: These counts are important because our TreeReflection infrastructure is quite fragile.
  // Therefore we do need additional safeguards in place to prevent silent failures.
  // I understand that it's inconvenient to update these numbers every time something changes,
  // but please deal with that (or come up with a more effective way of testing TreeReflection)
  test("root") {
    val sym = symbolOf[scala.meta.Tree]
    assert(sym.isRoot)
    val root = sym.asRoot
    assertEquals((root.allBranches.length, root.allLeafs.length), (78, 496))
  }

  test("branch extractors") {
    val obtained = symbolOf[scala.meta.Tree].asRoot.allBranches.map { b =>
      val unapply = b.sym.companion.info.member(TermName("unapply"))
      s"${b.sym.fullName}: ${unapply.alternatives.map(_.asMethod.returnType).mkString(", ")}"
    }.sorted.mkString(EOL)
    assertNoDiff(
      obtained,
      """|
         |scala.meta.CaseTree: Option[(scala.meta.Tree, scala.meta.Tree)]
         |scala.meta.Ctor: Boolean
         |scala.meta.Decl.GivenLike: Option[(List[scala.meta.Mod], scala.meta.Name, List[scala.meta.Member.ParamClauseGroup], scala.meta.Type)]
         |scala.meta.Decl: Boolean
         |scala.meta.Defn: Boolean
         |scala.meta.Enumerator.Assign: Option[(scala.meta.Pat, scala.meta.Term)]
         |scala.meta.Enumerator: Boolean
         |scala.meta.ImportExportStat: Option[List[scala.meta.Importer]]
         |scala.meta.Importee.ClauseLike: Boolean
         |scala.meta.Importee: Boolean
         |scala.meta.Lit: Option[Any]
         |scala.meta.Member.Apply: Option[(scala.meta.Tree, scala.meta.Member.ArgClause)]
         |scala.meta.Member.ArgClause: Option[List[scala.meta.Tree]]
         |scala.meta.Member.Function: Option[(scala.meta.Member.SyntaxValuesClause, scala.meta.Tree)]
         |scala.meta.Member.Infix: Option[(scala.meta.Tree, scala.meta.Name, scala.meta.Tree)]
         |scala.meta.Member.Param: Option[List[scala.meta.Mod]]
         |scala.meta.Member.ParamClause: Option[List[scala.meta.Member.Param]]
         |scala.meta.Member.SyntaxValuesClause: Option[List[scala.meta.Tree]]
         |scala.meta.Member.Term: Option[meta.Term.Name]
         |scala.meta.Member.Tuple: Option[List[scala.meta.Tree]]
         |scala.meta.Member.Type: Option[meta.Type.Name]
         |scala.meta.Member: Option[scala.meta.Name]
         |scala.meta.Mod.ArgsType: Boolean
         |scala.meta.Mod.ParamsType: Boolean
         |scala.meta.Mod.Variant: Boolean
         |scala.meta.Mod.WithWithin: Option[scala.meta.Ref]
         |scala.meta.Mod: Boolean
         |scala.meta.Name: Option[String]
         |scala.meta.Pat: Boolean
         |scala.meta.Ref: Boolean
         |scala.meta.Stat.GivenLike: Option[(List[scala.meta.Mod], scala.meta.Name, List[scala.meta.Member.ParamClauseGroup])]
         |scala.meta.Stat.TypeDef: Option[(List[scala.meta.Mod], meta.Type.ParamClause, meta.Type.Bounds)]
         |scala.meta.Stat.WithCtor: Option[scala.meta.Ctor.Primary]
         |scala.meta.Stat.WithMods: Option[List[scala.meta.Mod]]
         |scala.meta.Stat.WithTemplate: Option[scala.meta.Template]
         |scala.meta.Stat: Boolean
         |scala.meta.Term.ForClause: Option[(scala.meta.Term.EnumeratorsBlock, scala.meta.Tree)]
         |scala.meta.Term.FunctionLike: Option[(scala.meta.Member.ParamClause, scala.meta.Term)]
         |scala.meta.Term.FunctionTerm: Option[(scala.meta.Term.ParamClause, scala.meta.Term)]
         |scala.meta.Term.MacroLike: Option[scala.meta.Tree]
         |scala.meta.Term.MatchLike: Option[(scala.meta.Term, scala.meta.Term.CasesBlock, List[scala.meta.Mod])]
         |scala.meta.Term.QuotedMacroLike: Option[scala.meta.Tree]
         |scala.meta.Term.Ref: Boolean
         |scala.meta.Term.SelectLike: Option[(scala.meta.Term, scala.meta.Term.Name)]
         |scala.meta.Term.SplicedMacroLike: Option[scala.meta.Tree]
         |scala.meta.Term.TryClause: Option[(scala.meta.Term, Option[scala.meta.Tree], Option[scala.meta.Term])]
         |scala.meta.Term: Boolean
         |scala.meta.Tree.Block: Option[List[scala.meta.Tree]]
         |scala.meta.Tree.CasesBlock: Option[List[scala.meta.CaseTree]]
         |scala.meta.Tree.Repeated: Option[scala.meta.Tree]
         |scala.meta.Tree.WithBody: Option[scala.meta.Tree]
         |scala.meta.Tree.WithCases: Option[List[scala.meta.CaseTree]]
         |scala.meta.Tree.WithCasesBlock: Option[scala.meta.Tree.CasesBlock]
         |scala.meta.Tree.WithCond: Option[scala.meta.Term]
         |scala.meta.Tree.WithCondOpt: Option[Option[scala.meta.Term]]
         |scala.meta.Tree.WithDeclTpe: Option[scala.meta.Type]
         |scala.meta.Tree.WithDeclTpeOpt: Option[Option[scala.meta.Type]]
         |scala.meta.Tree.WithEnumeratorsBlock: Option[scala.meta.Term.EnumeratorsBlock]
         |scala.meta.Tree.WithEnums: Option[List[scala.meta.Enumerator]]
         |scala.meta.Tree.WithExprs: Option[List[scala.meta.Tree]]
         |scala.meta.Tree.WithParamClauseGroup: Option[Option[scala.meta.Member.ParamClauseGroup]]
         |scala.meta.Tree.WithParamClauseGroups: Option[List[scala.meta.Member.ParamClauseGroup]]
         |scala.meta.Tree.WithParamClauses: Option[Seq[scala.meta.Term.ParamClause]]
         |scala.meta.Tree.WithPats: Option[List[scala.meta.Pat]]
         |scala.meta.Tree.WithStats: Option[List[scala.meta.Stat]]
         |scala.meta.Tree.WithStatsBlock: Option[scala.meta.Stat.Block]
         |scala.meta.Tree.WithTParamClause: Option[scala.meta.Type.ParamClause]
         |scala.meta.Type.ByNameType: Option[scala.meta.Type]
         |scala.meta.Type.Captures: Boolean
         |scala.meta.Type.FunctionLikeType: Option[scala.meta.Type]
         |scala.meta.Type.FunctionParamOrArg: Option[(List[scala.meta.Mod], Option[scala.meta.Type.Name], scala.meta.Type)]
         |scala.meta.Type.FunctionType: Option[(scala.meta.Type.FuncParamClause, scala.meta.Type)]
         |scala.meta.Type.ParamFunctionType: Option[(scala.meta.Type.FuncParamClause, scala.meta.Type)]
         |scala.meta.Type.Placeholder: Option[scala.meta.Type.Bounds]
         |scala.meta.Type.PureFunctionType: Option[(scala.meta.Type.FuncParamClause, scala.meta.Type)]
         |scala.meta.Type.Ref: Boolean
         |scala.meta.Type: Boolean
         |scala.meta.internal.trees.Quasi: Option[(Int, scala.meta.Tree, Class[_])]
         |""".stripMargin.lf2nl,
    )
  }

  test("If") {
    val iff = symbolOf[scala.meta.Term.If].asLeaf
    assertEquals(
      iff.fields.map(_.toString),
      List(
        "field Term.If.cond: scala.meta.Term",
        "field Term.If.thenp: scala.meta.Term",
        "field Term.If.elsep: scala.meta.Term",
        "field Term.If.mods: List[scala.meta.Mod]",
      ),
    )
    assertEquals(
      iff.allFields.map(_.toString),
      List(
        "field Term.If.origin: scala.meta.trees.Origin",
        "field Term.If.begComment: Option[meta.Tree.Comments]",
        "field Term.If.endComment: Option[meta.Tree.Comments]",
        "field Term.If.cond: scala.meta.Term",
        "field Term.If.thenp: scala.meta.Term",
        "field Term.If.elsep: scala.meta.Term",
        "field Term.If.mods: List[scala.meta.Mod]",
      ),
    )
  }

  test("Term.Name") {
    val iff = symbolOf[scala.meta.Term.Name].asLeaf
    assertEquals(
      iff.fields.map(_.toString),
      List("field Term.Name.value: String @org.scalameta.invariants.nonEmpty"),
    )
    assertEquals(
      iff.allFields.map(_.toString),
      List(
        "field Term.Name.origin: scala.meta.trees.Origin",
        "field Term.Name.begComment: Option[meta.Tree.Comments]",
        "field Term.Name.endComment: Option[meta.Tree.Comments]",
        "field Term.Name.value: String @org.scalameta.invariants.nonEmpty",
      ),
    )
  }

  test("allFields") {
    val allRelevantFields = symbolOf[scala.meta.Tree].asRoot.allLeafs
      .filter(!_.sym.fullName.endsWith(".Quasi")).flatMap(_.fields)
    val duplicateRelevantFieldTpes = allRelevantFields.map(_.tpe).map {
      case AnnotatedType(_, tpe) => tpe
      case tpe => tpe
    }
    // NOTE: we can't just do `duplicateRelevantFieldTpes.distinct`, because that doesn't account for `=:=`
    val distinctRelevantFieldTpes = ListBuffer[Type]()
    duplicateRelevantFieldTpes.foreach(tpe =>
      if (!distinctRelevantFieldTpes.exists(_ =:= tpe)) distinctRelevantFieldTpes += tpe,
    )
    val obtained = distinctRelevantFieldTpes.sortBy(_.toString).mkString(EOL)
    assertNoDiff(
      obtained,
      """|
         |Boolean
         |Byte
         |Char
         |Int
         |List[meta.Term.ParamClause]
         |List[scala.meta.Case]
         |List[scala.meta.Enumerator]
         |List[scala.meta.Importee]
         |List[scala.meta.Importer]
         |List[scala.meta.Init]
         |List[scala.meta.Lit.String]
         |List[scala.meta.Lit]
         |List[scala.meta.Member.ParamClauseGroup]
         |List[scala.meta.Mod.Annot]
         |List[scala.meta.Mod]
         |List[scala.meta.Pat]
         |List[scala.meta.Source]
         |List[scala.meta.Stat.TypeDef]
         |List[scala.meta.Stat]
         |List[scala.meta.Term.Name]
         |List[scala.meta.Term.Param]
         |List[scala.meta.Term.Ref]
         |List[scala.meta.Term]
         |List[scala.meta.Tree.Comment]
         |List[scala.meta.Type.Param]
         |List[scala.meta.TypeCase]
         |List[scala.meta.Type]
         |Long
         |Option[scala.meta.Member.ParamClauseGroup]
         |Option[scala.meta.Mod.ArgsType]
         |Option[scala.meta.Mod.ParamsType]
         |Option[scala.meta.Mod.Variant]
         |Option[scala.meta.Self]
         |Option[scala.meta.Stat.Block]
         |Option[scala.meta.Term.CasesBlock]
         |Option[scala.meta.Term]
         |Option[scala.meta.Type]
         |Seq[scala.meta.Term.ArgClause]
         |Seq[scala.meta.Term.ParamClause]
         |Short
         |String
         |Symbol
         |meta.Type.ParamClause
         |scala.meta.Ctor.Block
         |scala.meta.Ctor.Primary
         |scala.meta.Importee.ClauseLike
         |scala.meta.Init
         |scala.meta.Lit
         |scala.meta.Name
         |scala.meta.Name.Anonymous
         |scala.meta.Pat
         |scala.meta.Pat.ArgClause
         |scala.meta.Pkg.Body
         |scala.meta.Ref
         |scala.meta.Stat
         |scala.meta.Stat.Block
         |scala.meta.Template
         |scala.meta.Template.Body
         |scala.meta.Term
         |scala.meta.Term.ArgClause
         |scala.meta.Term.CasesBlock
         |scala.meta.Term.EnumeratorsBlock
         |scala.meta.Term.Name
         |scala.meta.Term.ParamClause
         |scala.meta.Term.Ref
         |scala.meta.Type
         |scala.meta.Type.ArgClause
         |scala.meta.Type.Bounds
         |scala.meta.Type.Captures
         |scala.meta.Type.CasesBlock
         |scala.meta.Type.FuncParamClause
         |scala.meta.Type.Name
         |""".stripMargin.lf2nl,
    )
  }
}
